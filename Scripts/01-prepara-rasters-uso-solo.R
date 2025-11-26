###
# Combinar rasters do MapBiomas de uso e cobertura
# para o análise do doutorado
# agosto/2025
### 

pacman::p_load(tidyverse,
               terra
               )

# Código do GEE que usei para baixar os arquivos:
# https://code.earthengine.google.com/9a966a49de108b6fb5609d3f1ca507c4

# Referência:
# Projeto MapBiomas – Coleção 10 da Série Anual de Mapas de Cobertura e Uso da Terra do Brasil, acessado em 18/08/2025 através do link: https://plataforma.brasil.mapbiomas.org/coverage/
# Souza at. al. (2020) - Reconstructing Three Decades of Land Use and Land Cover Changes in Brazilian Biomes with Landsat Archive and Earth Engine - Remote Sensing, Volume 12, Issue 17, 10.3390/rs12172735.


# Combinar rasters --------------------------------------------------------

teste <- rast("Dados/MapBiomas/LandCover_02019.tif")
plot(teste)
res(teste)*111 # ~1km
crs(teste)

leg <- read_delim("Dados/MapBiomas/Codigos-da-legenda-colecao-10.csv", delim = "\t")
# https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2025/08/Legenda-Colecao-10-Descricao-Detalhada-PDF_PT-BR_EN.pdf
glimpse(leg)
unique(leg$Descricao)
unique(leg$Class_ID) |> sort()
plot(teste, col = leg$Color)

unique(teste)
hist(teste$classification_2019)
rm("teste")

# Combinar todos os raster em uma única imagem
# 1. criar SpatRasterCollection com sprc()
# 2. depois usar merge()
# 3. Loop por ano

# list.files(pattern = stringr::str_c("1985", ".tif$"), full.names = T)

mesclar_rasters <- function(ano) {
  an <- list.files("Dados/MapBiomas/",
                   pattern = stringr::str_c(ano, ".tif$"), full.names = T)
  ran <- sprc(an)
  m <- merge(ran)
  writeRaster(m, str_c("Dados/MapBiomas/r_amz/UsoCobertura_", ano, ".tif"), overwrite=TRUE)
}

mesclar_rasters("1985")
mesclar_rasters("2024")

# checar <- rast("Dados/MapBiomas/r_amz/UsoCobertura_1985.tif")
# plot(checar)

map(as.character(c(1985:2024)), mesclar_rasters, .progress = T)
