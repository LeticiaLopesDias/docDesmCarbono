###
# Prepara os dados das APs, por ano de criação
# agosto/2025
###

if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  terra,
  tidyverse,
  sf
)

# Abrir modelo
modelo <- rast("Intermediarios/modelo.tif")
plot(modelo)

amz <- geobr::read_biomes() |> filter(name_biome == "Amazônia") |> vect()
amz <- project(amz, crs(modelo))
plot(amz)


### Preciso rasterizar as áreas ocupadas por UCs, TIs e TQs
# Um raster AP por ano - impacto no desmatamento vai ser APÓS a existência da área

# Passo 1: carregar vetores:
# Unidades de conservação:
ucs <- vect("Dados/shp_cnuc_2025_03/cnuc_2025_03.shp")
crs(ucs)

ucs <- ucs |> terra::project(crs(modelo))
ucs_amz <- terra::intersect(ucs, amz)
ucs_amz # 364 unidades
plot(ucs_amz)

# Criar coluna com ano de criacao
ucs_amz$ano <- year(dmy(ucs_amz$cria_ano)) 
head(ucs_amz)
names(ucs_amz)

# Mantém apenas as colunas "cd_cnuc" e "ano"
ucs_amz <- ucs_amz[, c("cd_cnuc", "ano")]
ucs_amz$tipo <- "uc"


# Terras indígenas:
tis <- vect("Dados/tis_poligonais/tis_poligonaisPolygon.shp")
tis <- tis |> terra::project(crs(modelo))
names(tis)
unique(tis$fase_ti)
# Tirar TIs em estudo e "Encaminhada RI" - considerar apenas fases após delimitação
tis <- tis[tis$fase_ti != "Em Estudo", ]
tis <- tis[tis$fase_ti != "Encaminhada RI", ]

unique(tis$fase_ti)
table(tis$fase_ti)

tis_amz <- terra::intersect(tis, amz)
head(tis_amz) # 320 TIs

# Pegar ano de criação das TIs pelas portarias
tis_port <- read_csv("Dados/tis_poligonais_portarias.csv")
glimpse(tis_port)

tis_port <- tis_port |> 
  filter(terrai_codigo %in% tis_amz$terrai_cod)

tis_port |> select(c(data_em_estudo,
                   data_delimitada, 
                   data_declarada,
                   data_regularizada, 
                   data_homologada))

sum(is.na(tis_port$data_em_estudo))
sum(is.na(tis_port$data_delimitada))
sum(is.na(tis_port$data_declarada))
sum(is.na(tis_port$data_regularizada))
sum(is.na(tis_port$data_homologada))
# ordem temporal: em estudo > delimitada > declarada > homologada > regulatirzada

tis_port |> filter(is.na(data_homologada)) |> view()

# Usar o valor de ano mais antigo
tis_port2 <- tis_port |> 
  mutate(across(starts_with("data_"), ymd))  |> 
  mutate(ano = pmap_int(across(starts_with("data_")),
                        ~ year(min(c(...), na.rm = TRUE)))) |> 
  filter(!is.na(ano)) # 3 TIs não tinham nenhuma data

glimpse(tis_port2)
hist(tis_port$ano)
sum(is.na(tis_port$ano))

# Junta os anos no shape de tis
tis_amz <- merge(tis_amz,
                 tis_port2[, c("terrai_codigo", "ano")],
                 by.x = "terrai_cod",
                 by.y = "terrai_codigo",
                 all.x = F # remove tis_amz q não tem no tis_port2
                 )
names(tis_amz)

tis_amz <- tis_amz[, c("terrai_cod", "ano")]
tis_amz$tipo <- "ti"



# Quilombos
quil <- vect("Dados/Áreas de Quilombolas/Áreas de Quilombolas.shp")
plot(quil)
crs(quil)

quil <- quil |> terra::project(crs(modelo))
names(quil)
unique(quil$fase)
# Tirar NA e titulo anulado

quil <- quil[quil$fase != "TITULO ANULADO", ]
quil <- quil[!is.na(quil$fase), ]
unique(quil$fase)
quil_amz <- terra::intersect(quil, amz)

# Algumas linhas representam o mesmo quilombo - unificar pelo no. do processo
head(quil_amz)
str_extract(str_remove_all(quil_amz$nr_process, "[/|//.|-]"), "[0-9]+")

quil_amz2 <- quil_amz |> 
  st_as_sf() |> 
  select(c(cd_quilomb:cd_uf, nr_familia, nr_area_ha, 
           esfera, fase, responsave, 
           dt_publica, dt_titulac, dt_decreto)) |>
  st_make_valid() |> 
  mutate(
    nr_process = str_extract(str_remove_all(nr_process, "[/|//.|-]"), "[0-9]+")
  ) |> 
  group_by(nr_process) |> 
  summarise(geometry = st_union(geometry), .groups = "drop",
            cd_quilomb = first(cd_quilomb),
            nm_comunid = first(nm_comunid),
            cd_sr = first(cd_sr),
            nm_municip = first(nm_municip),
            cd_uf = first(cd_uf),
            nr_familia = first(nr_familia),
            fase = first(fase),
            dt_publica = first(dt_publica), 
            dt_titulac = first(dt_titulac), 
            dt_decreto = first(dt_decreto)
  ) |> 
  vect()
# De 125 foi para 122 quilombos
n_distinct(quil_amz$nr_process)

names(quil_amz2)
quil_amz2[, c("dt_publica", "dt_titulac", "dt_decreto")] |> view()

sum(is.na(quil_amz2$dt_publica))
sum(is.na(quil_amz2$dt_titulac))
sum(is.na(quil_amz2$dt_decreto))
# Fazer como com as TIs, pegar data mais antiga

# Usar o valor de ano mais antigo
quil_amz3 <- quil_amz2 |> 
  as_tibble() |> 
  mutate(across(starts_with("dt_"), dmy))  |> 
  mutate(ano = pmap_int(across(starts_with("dt_")),
                        ~ year(min(c(...), na.rm = TRUE)))) |> 
  filter(!is.na(ano))

glimpse(quil_amz3)
hist(quil_amz3$ano)

# Junta os anos no shape de tis
quil_amz2 <- merge(quil_amz2,
                   quil_amz3[, c("nr_process", "ano")],
                   by = "nr_process",
                   all.x = F
)
names(quil_amz2)

quil_amz2 <- quil_amz2[, c("nr_process", "ano")]
quil_amz2$tipo <- "tq"


# Juntas aps em único shape
ucs_amz
tis_amz
quil_amz2

# Renomeia as colunas-chave como "id" e converte todas para character
ucs_amz$id <- as.character(ucs_amz$cd_cnuc)
ucs_amz <- ucs_amz[, c("id", "ano", "tipo")]

tis_amz$id <- as.character(tis_amz$terrai_cod)
tis_amz <- tis_amz[, c("id", "ano", "tipo")]

quil_amz2$id <- as.character(quil_amz2$nr_process)
quil_amz2 <- quil_amz2[, c("id", "ano", "tipo")]

# Salvar vetores por tipo de AP:
writeVector(ucs_amz, "Intermediarios/ucs.gpkg", overwrite = T)
writeVector(tis_amz, "Intermediarios/tis.gpkg", overwrite = T)
writeVector(quil_amz2, "Intermediarios/quilombos.gpkg", overwrite = T)

# Agora, rasterizar
ucs_amz <- vect("Intermediarios/ucs.gpkg")
tis_amz <- vect("Intermediarios/tis.gpkg")
quil_amz2 <- vect("Intermediarios/quilombos.gpkg")

# Junta os três shapes: UCs, TIs e TQs
aps <- rbind(ucs_amz, tis_amz, quil_amz2)
plot(aps["tipo"])
table(aps$tipo) # tudo certo!

# Agora, rasterizar por ano!

# Primeiro, áreas criadas ate 1985
aps85 <- aps[aps$ano <= 1985, ]
plot(aps85)
table(aps85$tipo) # ok!

# Rasterizar o shapefile calculando a proporção de cobertura em cada pixel
# A opção fun = "cover" calcula a fração da célula coberta pela geometria do vetor
r_aps85 <- rasterize(aps85, modelo, cover = T, touches = TRUE)

# Criar um raster binário com base no critério de 75% de cobertura
m <- matrix(c(0, 0.75, NA, 0.75, 1, 1), ncol = 3, byrow = TRUE)
r2_aps85 <- classify(r_aps85, rcl = m)
r2_aps85 <- ifel(is.na(r2_aps85), 0, 1)
r2_aps85 <- mask(r2_aps85, amz)
plot(r2_aps85)

# Depois, por ano - sempre incluir anos anteriores
unique(aps$ano) |> sort()

# Fazer função para rasterizar por ano
rasterizar_aps <- function(ano) {
  
  ap <- aps[aps$ano <= ano, ]
  r1 <- rasterize(ap, modelo, cover = T, touches = TRUE)
  r2 <- classify(r1, rcl = m)
  r3 <- ifel(is.na(r2), 0, 1)
  r4 <- mask(r3, amz)
  
  writeRaster(r4, 
              stringr::str_c("Intermediarios/APs_", 
                             as.character(ano), ".tif"),
              overwrite = TRUE)
}

rasterizar_aps(1985)

# teste <- rast("Intermediarios/APs_1985.tif")
# plot(teste)
# plot(r2_aps85) # idênticos, ok!

map(c(1986:2024), rasterizar_aps, .progress = T)



# Periodos -----------------------------------------------------------

## APs 1985-2015
## APs total

lista <- list.files("Intermediarios/", pattern = "APs", full.names = T)
aps <- rast(lista)

plot(aps[[1]])
aps[[31]]
aps86_15 <- sum(aps[[c(1:31)]])
plot(aps86_15)

# Deixar como 0 e 1
m2 <- matrix(c(0, 0, 0, 1, 32, 1), ncol = 3, byrow = TRUE)
aps86_15_c <- classify(aps86_15, rcl = m2)
plot(aps86_15_c)

aps_todas <- sum(aps[[c(1:40)]])
plot(aps_todas)

m3 <- matrix(c(0, 0, 0, 1, 41, 1), ncol = 3, byrow = TRUE)
aps_todas_c <- classify(aps_todas, rcl = m3)
plot(aps_todas_c)


writeRaster(aps86_15_c, "Intermediarios/APs_86_15.tif", overwrite = TRUE)
writeRaster(aps_todas_c, "Intermediarios/APs_todas.tif", overwrite = TRUE)
