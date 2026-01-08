###
# Análise de emissões evitadas - pós-matching
# Data: novembro/2025
# Autor: Letícia Lopes Dias (leticia_lopes@discente.ufg.br)
###

if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,
  MatchIt,
  terra
)

# Analisar primeiro período pós métrica de carbono
mat_p2 <- readRDS("Finais/m_data_16_24.rds")
glimpse(mat_p2)
# Esse dataset contém unidades pareadas e adiciona colunas para distance,
# weights, e subclass

# Inspecionar dataset do matching
table(mat_p2$protecao)
mat_p2 |> filter(subclass == 1) |> view()
# Cada subclasse representa um grupo de célular pareadas

hist(mat_p2$weights)
range(mat_p2$weights) 
# Unidades de controle que foram pareadas com mais de uma unidade tratada recebem 
# pesos maiores (por exemplo, weight = 2 se foram usadas duas vezes)
# Não representa diretamente q quantidade de pares em que participa, mas uma
# ideia de "importância"
# weight = 0 → unidade foi excluída do pareamento

hist(mat_p2$desmatamento)

hist(mat_p2$distance)
# O escore de propensão foi estimado via um modelo logístico
# O valor da coluna distance é o valor predito por esse modelo para cada unidade.
# distance ≈ 0 → unidade tem baixa chance de estar no grupo tratado 
# distance ≈ 1 → unidade tem alta chance de estar no grupo tratado

mat_p2 |> 
  group_by(subclass) |> 
  count(protecao) |> 
  view()

# Várias células de controle foram pareadas com uma ou mais de tratamento;
# isso forma a subclass

# Para selecionar células para o "melhor" grupo controle e ver o total de 
# desmatamento que ocorreu nelas (com tamanho do grupo = tamanho tratadas):
# Por subclasse;
# selecionar o mesmo número de células controle, em relação às tratadas
# selecionar aquelas com maior prop score

grupo_controle <- mat_p2  |> 
  group_split(subclass) |> 
  map_dfr(\(sub_df) {
    n_tratados <- sub_df |> filter(protecao == 1) |> nrow()
    controles <- sub_df |>  filter(protecao == 0) |> arrange(desc(distance))
    n_controles <- nrow(controles)
    
    if (n_controles >= n_tratados) {
      # Caso comum: controles suficientes → seleciona os melhores
      controles |> slice_max(n = n_tratados, order_by = distance)
    } else {
      # Caso especial: controles insuficientes → repete amostras
      reps <- ceiling(n_tratados / n_controles)
      controles_replicados <- controles |> 
        slice(rep(1:n(), reps)) |> 
        slice_head(n = n_tratados)
      controles_replicados
    }
  })

glimpse(grupo_controle)
# Salvar resultado
vroom::vroom_write(grupo_controle, "Intermediarios/Controle_16_24.csv", append = F)

### Analise
grupo_controle <- vroom::vroom("Intermediarios/Controle_16_24.csv")
glimpse(grupo_controle)

grupo_controle |> 
  count(subclass) |> view()

# conferir se grupo controle foi selecionado corretamente, ou seja,
# pelo maior propensity score
mat_p2 |> 
  filter(protecao == 0) |> 
  filter(subclass == 1) |> 
  view()

grupo_controle |> 
  filter(subclass == 1) |> 
  view()
# Ok!

# Agora, calcular área desmatada
# Cada célula = 1 km²

sum(grupo_controle$desmatamento, na.rm = T)
# 100.709 km²
# 10.070.900 ha

# Comparar com valor obtido na análise por ano
dados <- readxl::read_xlsx("Finais/resultado_por_ano.xlsx")
glimpse(dados)

dados |> 
  filter(ano > 2014) |> 
  summarise(
    soma = sum(desm_evitado, na.rm = T)
  )
# 109.076 km²
# Valor próximo, mas não idêntico

# Limpar memória
rm("dados")

# Pegar carbono de cada célula; calcular emissões evitadas espacialmente
# Atribuir desmatamento das células controle às células protegidas
# Calcular perda de carbono com base no desmatamento que teria ocorrido
glimpse(mat_p2)
sum(is.na(mat_p2$biomass_Mgha_WGS84))
# remover células para as quais não tem valor de carbono

glimpse(grupo_controle)

grupo_controle2 <- grupo_controle |> 
  select(cell, subclass, desmatamento) |> 
  rename(cell_cont = cell,
         desm_evit = desmatamento,
         subclass_cont = subclass) |> 
  mutate(subclass_cont = as.factor(subclass_cont)) |> 
  arrange(subclass_cont)
glimpse(grupo_controle2)

analise_c <- mat_p2 |> 
  arrange(subclass) |> 
  filter(protecao == 1) |> 
  cbind(grupo_controle2) |> 
  filter(!is.na(biomass_Mgha_WGS84))
glimpse(analise_c)
n_distinct(analise_c$subclass)

# checar
sum(analise_c$desm_evit, na.rm = T)
# valor diferente, porque tirei células sem valor de biomassa

# checar se pareou todas as células na subclasse correta
all(as.double(analise_c$subclass) == as.double(analise_c$subclass_cont))

# multiplicar área de desm evitado por 100, para ter área em hectares
analise_c <- analise_c |> 
  mutate(carb_evit = desm_evit * 100 * biomass_Mgha_WGS84)
glimpse(analise_c)

# Estoque de carbono total
sum(analise_c$biomass_Mgha_WGS84*100)
# 45,311,230,840
# 45,3 bilhões de toneladas de carbono estocadas

# Mapa do EBA já vem com a conversão de biomassa para carbono realizada: 
# Our AGB map was converted to carbon (AGBC) by multiplying it by a factor of 
# 0.4742 to enable comparison with the 3rd Brazilian National Communication

sum(analise_c$carb_evit, na.rm = T)
# 2,178,621,241
# 2,2 bilhões de tC seriam evitadas, considerando conversão de 100% da biomassa
# inicial

# Limpar memória
rm("grupo_controle")
rm("grupo_controle2")


# Rasterizar as minhas variaveis de interesse do dataframe
# para obter resultado espacializado e identificar cada AP 
# Usar o raster modelo para obter os índices de célula
modelo <- rast("Intermediarios/modelo.tif")
plot(modelo)

r <- modelo # criar cópia
vals <- rep(NA_real_, ncell(r))
vals[analise_c$cell] <- analise_c[["desm_evit"]]
values(r) <- vals
names(r) <- "desm_evitado"
r
plot(r)
writeRaster(r, "Finais/Desm_evitado.tif")

## Agora, obter id das células para parear com AP
glimpse(analise_c)

# Carregar extensão de cada classe de AP
ucs_amz <- vect("Intermediarios/ucs.gpkg")
tis_amz <- vect("Intermediarios/tis.gpkg")
quil_amz <- vect("Intermediarios/quilombos.gpkg")

# Criar função para parear ID das células com id APs
parear_celulas <- function(rast) {
  n_cell <- cells(modelo, rast) |> as_tibble()
  rast_df <- rast |> 
    as_tibble() |> 
    rownames_to_column("num") |>
    mutate(num = as.numeric(num)) |> 
    left_join(n_cell, by = c('num' = "ID"))
}

ucs_df <- parear_celulas(ucs_amz)
glimpse(ucs_df)

tis_df <- parear_celulas(tis_amz)
glimpse(tis_df)

quil_df <- parear_celulas(quil_amz)
glimpse(quil_df)

aps_cell <- bind_rows(
  ucs_df, tis_df, quil_df
)

glimpse(aps_cell)
glimpse(analise_c)

resu_aps <- aps_cell |> 
  left_join(analise_c, by = 'cell') |> 
  select(-num)
glimpse(resu_aps)

# Salvar resultado - por células
vroom::vroom_write(resu_aps, "Finais/Res_APs_cell.csv", append = F)

resu_aps2 <- resu_aps |> 
  group_by(id) |> 
  summarise(
    ano = first(ano),
    tipo = first(tipo),
    across(c(agua_solo, carbono_solo, declividade:temperatura), 
           \(x) mean(x, na.rm = T)),
    desmatamento = sum(desmatamento, na.rm = T),
    biomass_Mgha = sum(biomass_Mgha_WGS84, na.rm = T),
    desm_evit = sum(desm_evit, na.rm = T),
    carb_evit = sum(carb_evit, na.rm = T)
  )

glimpse(resu_aps2)

## Criar tabela com dados das APs:
# id
# nome
# area/tamanho

# dicionário de substituições - estados
siglas <- c(
  "AMAZONAS" = "AM",
  "MARANHÃO" = "MA",
  "RONDÔNIA" = "RO",
  "AMAPÁ" = "AP",
  "MATO GROSSO" = "MT",
  "ACRE" = "AC",
  "PARÁ" = "PA",
  "TOCANTINS" = "TO",
  "RORAIMA" = "RR",
  "PARANÁ" = "PA" # esse Paraná parece ter vindo por erro de digitação no arquivo original
)

# Organizar dados das Unidades de Conservação
ucs <- vect("Dados/shp_cnuc_2025_03/cnuc_2025_03.shp") |> 
  as_tibble() |> 
  filter(cd_cnuc %in% resu_aps2$id) |> 
  mutate(
    ano = year(dmy(cria_ano)),
    ha_total = as.double(ha_total),
    uf = str_replace_all(uf, siglas)
  ) |> 
  select(
    c(cd_cnuc, NomeAbrev, ha_total, esfera, uf, grupo)
  ) |> 
  rename(
    id = cd_cnuc,
    nome = NomeAbrev,
    area = ha_total,
    grupo_uc = grupo
  )

glimpse(ucs) # 364 UCs, ok
hist(ucs$area)
unique(ucs$uf)

# Organizar dados das Terras indígenas:
tis <- vect("Dados/tis_poligonais/tis_poligonaisPolygon.shp") |> 
  as_tibble() |> 
  filter(terrai_cod %in% resu_aps2$id)  |> 
  select(
    c(terrai_cod, terrai_nom, uf_sigla, superficie)
  ) |> 
  mutate(
    terrai_cod = as.character(terrai_cod)
  ) |> 
  rename(
    id = terrai_cod,
    nome = terrai_nom,
    area = superficie,
    uf = uf_sigla
  )

glimpse(tis) 
hist(tis$superficie) # tb parece estar em hectares

# Organizar dados dos Quilombos
quil <- vect("Dados/Áreas de Quilombolas/Áreas de Quilombolas.shp") |> 
  as_tibble() |> 
  mutate(
    nr_process = str_extract(str_remove_all(nr_process, "[/|//.|-]"), "[0-9]+")
  ) |> 
  group_by(nr_process) |> 
  summarise(cd_quilomb = first(cd_quilomb),
            nm_comunid = first(nm_comunid),
            cd_uf = first(cd_uf),
            area_calc_ = sum(area_calc_, na.rm = T)
            ) |> 
  filter(nr_process %in% resu_aps2$id[resu_aps2$tipo == "tq"]) |> 
  select(-cd_quilomb) |> 
  rename(
    id = nr_process,
    nome = nm_comunid,
    area = area_calc_,
    uf = cd_uf
  )
glimpse(quil)
hist(quil$area)

# Juntar dados das APs em única tabela
aps <- ucs |> 
  bind_rows(tis) |> 
  bind_rows(quil) |> 
  left_join(resu_aps2)
glimpse(aps)

# Salvar resultado
vroom::vroom_write(aps, "Finais/Res_APs.csv", append = F)

