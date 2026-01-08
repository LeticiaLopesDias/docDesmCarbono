### 
# Cálculo do Desmatamento na Amazônia de 1986 a 2024
# Data: outubro/2025
# Autor: Letícia Lopes Dias (leticia_lopes@discente.ufg.br)
###

if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  terra,
  tidyverse
)

# Testar se raster está ok
teste <- rast("Dados/MapBiomas/r_amz/UsoCobertura_1985.tif")
plot(teste)
res(teste)*111 # ~1km
unique(teste)

leg <- read_delim("Dados/MapBiomas/Codigos-da-legenda-colecao-10.csv", delim = "\t")
# https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2025/08/Legenda-Colecao-10-Descricao-Detalhada-PDF_PT-BR_EN.pdf
glimpse(leg)
plot(teste, col = leg$Color)
# No raster, números indicam class de nível 2 (maioria)
leg |> 
  mutate(as.character(Class_ID)) |> 
  filter(Class_ID %in% as.character(unique(teste)$classification_1985)) |> 
  distinct(Descricao) |> view()

# Criar coluna indicando se nativa, não-nativa ou não se aplica
nativa <- c(
  "Formação Florestal",
  "Formação Savânica",
  "Mangue",
  "Floresta Alagável",
  "Restinga Arbórea",
  "Campo Alagado e Área Pantanosa",
  "Formação Campestre",
  "Apicum",
  "Afloramento Rochoso",
  "Restinga Herbácea"
)

nao_nativa <- c(
  "Pastagem",
  "Agricultura",
  "Cana",
  "Soja",
  "Arroz",
  "Algodão (beta)",
  "Outras Lavouras Temporárias",
  "Lavoura Perene",
  "Café",
  "Citrus",
  "Dendê",
  "Outras Lavouras Perenes",
  "Silvicultura",
  "Mosaico de Usos",
  "Área não Vegetada",
  "Área Urbanizada",
  "Mineração",
  "Outras Áreas não Vegetadas"
)

leg <- leg |> 
  mutate(
    nativa = case_when(
      Descricao %in% nativa ~ 1,
      Descricao %in% nao_nativa ~ 0,
      .default = NA
    )
  )

?terra::classify

teste_c <- classify(teste, cbind(leg$Class_ID, leg$nativa))
plot(teste_c)
# Deu certo

# Conferir processo de cálculo do desmatamento, após reclassificar
teste2 <- rast("Dados/MapBiomas/r_amz/UsoCobertura_1986.tif")
teste2_c <- classify(teste2, cbind(leg$Class_ID, leg$nativa))
plot(teste2_c)

desm <- teste_c - teste2_c
plot(desm)
# 0 = manteve mesma classe, seja nativa ou não
# -1 = regeneração, virou nativa
# 1 = desmatamento, virou não-nativa

# Usar esse arquivo para criar modelo
# ajustar para área do bioma amazônia

# Baixar extensão da Amazônia
amz <- geobr::read_biomes() |> filter(name_biome == "Amazônia") |> vect()
plot(amz)
crs(amz)

# Reprojetar para CRS do raster
amz <- project(amz, crs(teste))
plot(desm)
plot(amz, add = T)

# Deixar células do modelo = 1
modelo <- desm^0
modelo <- modelo |> 
  crop(amz) |> 
  mask(amz) 
plot(modelo)
writeRaster(modelo, "Intermediarios/modelo.tif")

# Criar uma função para calcular desmatamento anual e salvar tif

calcula_desm <- function(ano) {

  ano_in <- rast(stringr::str_c("Dados/MapBiomas/r_amz/UsoCobertura_", 
                                as.character(ano), ".tif"))
  ano_fin <- rast(stringr::str_c("Dados/MapBiomas/r_amz/UsoCobertura_", 
                                 as.character(ano+1), ".tif"))
  
  ano_in_c <- classify(ano_in, cbind(leg$Class_ID, leg$nativa))
  ano_fin_c <- classify(ano_fin, cbind(leg$Class_ID, leg$nativa))
  
  desm <- ano_in_c - ano_fin_c
  desm <- desm |> crop(amz) |> mask(amz) 
  
  writeRaster(desm, 
              stringr::str_c("Intermediarios/Desm_", as.character(ano+1), ".tif"),
              overwrite = TRUE
              )
}

calcula_desm(1985)

# Checar arquvivo gerado
# teste3 <- rast("Intermediarios/Desm_1986.tif")
# plot(teste3)
# plot(desm) # ok
# Por enquanto, vou manter valor de regeneração (-1)

map(c(1985:2023), calcula_desm, .progress = T)

# Para outra análise, separar períodos:
## Desmatamento 1985-2015
## Desmatamento 2016-2024

lista <- list.files("Intermediarios/", pattern = "Desm", full.names = T)
desm <- rast(lista)

plot(desm[[2]])
desm[[30]]
desm86_15 <- sum(desm[[c(1:30)]])
plot(desm86_15)

desm16_24 <- sum(desm[[c(31:39)]])
plot(desm16_24)

writeRaster(desm86_15, "Intermediarios/Desm_86_15.tif", overwrite = TRUE)
writeRaster(desm16_24, "Intermediarios/Desm_16_24.tif", overwrite = TRUE)
