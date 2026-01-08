### 
# Prepara tabelas de dados para matching
# Data: outubro/2025
# Autor: Letícia Lopes Dias (leticia_lopes@discente.ufg.br)
###

if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  terra,
  tidyverse
)

# Preparar tabela de dados ------------------------------------------------

list.files("Intermediarios/", full.names = T)

covariaveis <- rast(
  c(
    "Intermediarios/Agua_solo.tif",
    "Intermediarios/Carbono_solo.tif",
    "Intermediarios/Cobertura_orig.tif",
    "Intermediarios/Declividade.tif",
    "Intermediarios/Dist_hidrovias.tif",
    "Intermediarios/Dist_mineracao.tif",
    "Intermediarios/Dist_municipios.tif",
    "Intermediarios/Dist_rodovias.tif",
    "Intermediarios/Elevacao.tif",
    "Intermediarios/Nitrogenio_solo.tif",
    "Intermediarios/Precip_media_anual.tif",
    "Intermediarios/Temp_media_anual.tif"
  )
)

names(covariaveis) <- c("agua_solo", "carbono_solo", "cobertura", 
                        "declividade", "d_hidrovias", "d_mineracao", "d_municipios",
                        "d_rodovias", "elevacao", "nitrogenio", "precipitacao",
                        "temperatura")
plot(covariaveis)

# Ver onde nos rasters das covariaveis está como NA
# Plotar o raster, destacando os valores NA
# plot(covariaveis[[1]], colNA = "red")
# plot(covariaveis[[4]], colNA = "red")

# NA está na parte externa, que eu não quero considerar mesmo. 
# Preciso indicar isso 
# no momento de transformar em tabela

res(covariaveis)*111
ncell(covariaveis) # 8 milhões de células

# Entrada da função Match precisa ser dataframe
covariaveis_df <- as.data.frame(covariaveis, xy = T, cells = T, na.rm = T)
glimpse(covariaveis_df) # 4 milhões de células, sem NA

# Para algumas células, não tem valor de carbono (raster não são totalmente
# alinhados) - por isso, adicionar separadamente - a partir de 2016
carbono <- rast("Intermediarios/Carbono_eba.tif")
plot(carbono)
carb_df <- as.data.frame(carbono, xy = T, cells = T, na.rm = T)

# Salvar tabelas por ano
salvar_tabelas <- function(ano) {
  
  prot <- rast(str_c("Intermediarios/APs_", as.character(ano), ".tif"))
  prot_df <- as.data.frame(prot, xy = T, cells = T, na.rm = T) |> 
    rename(protecao = layer)

  tb <- covariaveis_df |> left_join(prot_df)

  desm <- rast(str_c("Intermediarios/Desm_", as.character(ano+1),
                     ".tif"))
  desm_df <- as.data.frame(desm, xy = T, cells = T, na.rm = T)
  
  tb_d <- tb |> left_join(desm_df)
  tb_d <- tb_d  |> 
    rename_with(~ str_replace_all(.x, "classification", "deforestation"),
                .cols = starts_with("classification"))
  
  if (ano >= 2016) {
    
    # Adicionar carbono
    tb_c <- tb_d |> left_join(carb_df)
    vroom::vroom_write(tb_c, str_c("Finais/Dados_", as.character(ano),".csv"),
                       append = F)

  } else {
    
    vroom::vroom_write(tb_d, str_c("Finais/Dados_", as.character(ano),".csv"),
                       append = F)
  }
  
}

# vroom otimiza letura de tabelas com muitas linhas
salvar_tabelas(1985)

# teste <- vroom::vroom("Finais/Dados_1985.csv")
# glimpse(teste)
# glimpse(dados_c) # ok!

# Loop para todos os anos
map(c(1985:2023), salvar_tabelas, .progress = T)

# Para análises por período:
## Tabela 1985-2015
prot1 <- rast(str_c("Intermediarios/APs_86_15.tif"))
prot1_df <- as.data.frame(prot1, xy = T, cells = T, na.rm = T) |> 
  rename(protecao = sum)

tb1 <- covariaveis_df |> left_join(prot1_df)
glimpse(tb1)

desm1 <- rast(str_c("Intermediarios/Desm_86_15.tif"))
desm1_df <- as.data.frame(desm1, xy = T, cells = T, na.rm = T) |> 
  rename(desmatamento = sum)

tb1_d <- tb1 |> left_join(desm1_df)
glimpse(tb1_d)

vroom::vroom_write(tb1_d, "Finais/Dados_85_15.csv", append = F)

## Tabela 2016-2024
prot2 <- rast("Intermediarios/APs_todas.tif")
prot2_df <- as.data.frame(prot2, xy = T, cells = T, na.rm = T) |> 
  rename(protecao = sum)

tb2 <- covariaveis_df |> left_join(prot2_df)
glimpse(tb2)

desm2 <- rast("Intermediarios/Desm_16_24.tif")
desm2_df <- as.data.frame(desm2, xy = T, cells = T, na.rm = T) |> 
  rename(desmatamento = sum)

tb2_d <- tb2 |> 
  left_join(desm2_df) |> 
  left_join(carb_df)
glimpse(tb2_d)

vroom::vroom_write(tb2_d, "Finais/Dados_16_24.csv", append = F)
