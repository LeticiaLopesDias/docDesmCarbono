###
# Preparar dados para matching - Relatório II
# agosto/2025
###

if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  terra,
  tidyverse,
  sf
)

# Transformar dados em rasters padronizados de 1km de resolução, na extensão 
# da Amazônia

# Abrir modelo
modelo <- rast("Intermediarios/modelo.tif")
plot(modelo)
res(modelo)*111 # 1km
crs(modelo)

amz <- geobr::read_biomes() |> filter(name_biome == "Amazônia") |> vect()
amz <- project(amz, crs(modelo))
plot(amz)

# Rodovias ----------------------------------------------------------------
# Calcular a distância com os dados para o Brasil e depois recortar raster de
# distância para Amazônia

# Rodovias federais:
fed <- vect("Dados/rodovia-federal/rodovia-federal.shp")
plot(fed)
crs(fed)

# Simplificar o vetor para facilitar o cálculo
fed <- simplifyGeom(fed, tolerance = 0.1, makeValid = T)
plot(fed)
names(fed)

# Rodovias estaduais:
est <- vect("Dados/rodovia-estadual/rodovia-estadual.shp")
plot(est)

est <- simplifyGeom(est, tolerance = 0.1, makeValid = T)
plot(est)

# Rasterizar as rodovias
fed_r <- rasterize(fed, modelo, background = 0)
plot(fed_r)

est_r <- rasterize(est, modelo, background = 0)
plot(est_r)

# Fazer um único raster de estradas
estradas <- fed_r + est_r

# Deixar como 0 e 1
estradas <- classify(estradas, cbind(2, 1))
plot(estradas)

dist_estradas <- gridDist(estradas, target = 1, scale = 1000)
# scale = 1000 significa que resultado será em km

dist_estradas <- mask(dist_estradas, modelo)
plot(dist_estradas)
res(dist_estradas)*111
compareGeom(dist_estradas, modelo)

writeRaster(dist_estradas, "Intermediarios/Dist_rodovias.tif", overwrite = TRUE)

fed_amz <- mask(fed, amz)
est_amz <- mask(est, amz)

# Salvar mapa com distâncias e estradas
# https://www.rdocumentation.org/packages/terra/versions/1.2-5/topics/plot
tiff("Img/estradas.tif",
     res = 300,
     width = 12, height = 10, units = "cm")

plot(dist_estradas,
     col = viridis::viridis(option = "turbo", n = 70, direction = -1),
     plg = list(title = "Distância de \nestradas", size = c(.7,1)),
     axes = F)
#plot(fed_amz, add = T, col = "grey70")
#plot(est_amz, add = T, col = "grey70")

dev.off()

rm(list = c(
  "estradas",
  "dist_estradas",
  "est",
  "est_r",
  "est_amz",
  "fed",
  "fed_r",
  "fed_amz"
))

# Hidrovias ---------------------------------------------------------------

hid <- vect("Dados/vw_snv_aqa_hidrovias/vw_snv_aqa.shp")
plot(hid)
crs(hid)

hid <- hid |> project(crs(modelo))
hid <- simplifyGeom(hid, tolerance = 0.1, makeValid = T)
plot(hid)

# Rasterizar as hidrovias
hid_r <- rasterize(hid, modelo, background = 0)
plot(hid_r)

dist_hid <- gridDist(hid_r, target = 1, scale = 1000)
# scale = 1000 significa que resultado será em km
dist_hid <- mask(dist_hid, modelo)
plot(dist_hid)
compareGeom(dist_hid, modelo)

writeRaster(dist_hid, "Intermediarios/Dist_hidrovias.tif", overwrite = TRUE)

# hid_amz <- mask(hid, amz)
# hid_amz <- crop(hid_amz, amz)

# Salvar mapa com distâncias e estradas
# https://www.rdocumentation.org/packages/terra/versions/1.2-5/topics/plot
tiff("Img/hidrovias.tif",
     res = 300,
     width = 12, height = 10, units = "cm")

plot(dist_hid,
     col = viridis::viridis(option = "turbo", n = 70, direction = -1),
     plg = list(title = "Distância de \nhidrovias", size = c(.7,1)),
     axes = F)

dev.off()

# Limpar memória
rm(list = c(
  "dist_hid",
  "hid",
  "hid_r",
  "hid_amz"
)
)


# Areas urbanizadas -------------------------------------------------------

urb <- vect("Dados/areas_urbanizadas_do_Brasil_2005_shapes/AreasUrbanizadas_MunicipiosAcima100k_porMunicipio.shp")
plot(urb)
crs(urb)

urb <- urb |> project(crs(modelo))

# Para calcular distância, ver centroide das áreas
names(urb)
head(urb)
unique(urb$NOME_MUNIC)

?centroids
urb_cent <- centroids(urb)
plot(urb_cent)


# Rasterizar as hidrovias
urb_r <- rasterize(urb_cent, modelo, background = 0)
plot(urb_r)

dist_urb <- gridDist(urb_r, target = 1, scale = 1000)
# scale = 1000 significa que resultado será em km
dist_urb <- mask(dist_urb, modelo)
plot(dist_urb)
compareGeom(dist_urb, modelo)

writeRaster(dist_urb, "Intermediarios/Dist_municipios.tif", overwrite = TRUE)

# urb_cent <- mask(urb_cent, amz)
# unique(urb_cent$Tipo)
# n_distinct(urb_cent$NOME_MUNIC)


# Salvar mapa com distâncias e estradas
# https://www.rdocumentation.org/packages/terra/versions/1.2-5/topics/plot
tiff("Img/municipios.tif",
     res = 300,
     width = 12, height = 10, units = "cm")

plot(dist_urb,
     col = viridis::viridis(option = "turbo", n = 70, direction = -1),
     plg = list(title = "Distância de \nmunicípios", size = c(.7,1)),
     axes = F)

dev.off()


# Limpar memória
rm(list = c(
  "urb",
  "urb_amz",
  "urb_cent",
  "urb_r",
  "dist_urb"
)
)

# Mineração   -------------------------------------------------------

min <- vect("Dados/Processos minerários ativos BRASIL/BRASIL.shp")
plot(min)
crs(min)

min <- min |> project(crs(modelo))

names(min)
head(min)
unique(min$ANO) # Filtrar processo até 1985

min_85 <- min[min$ANO < 1985, ]
min_85 <- terra::intersect(min_85, amz)
plot(min_85)

min_85

# Para calcular distância, ver centroide das áreas
?centroids
min_cent <- centroids(min_85)
plot(min_cent)

# Rasterizar
min_r <- rasterize(min_cent, modelo, background = 0)
plot(min_r)

dist_min <- gridDist(min_r, target = 1, scale = 1000)
# scale = 1000 significa que resultado será em km
dist_min <- mask(dist_min, modelo)
plot(dist_min)
compareGeom(dist_min, modelo)

writeRaster(dist_min, "Intermediarios/Dist_mineracao.tif", overwrite = T)

# Salvar mapa

tiff("Img/mineracao.tif",
     res = 300,
     width = 12, height = 10, units = "cm")

plot(dist_min,
     col = viridis::viridis(option = "turbo", n = 70, direction = -1),
     plg = list(title = "Distância de \nmineração", size = c(.7,1)),
     axes = F)

dev.off()

# Limpar memória
rm(list = c(
  "dist_min",
  "min",
  "min_85",
  "min_cent",
  "min_r"
)
)


# Precipitacao ------------------------------------------------------------

prec <- rast("Dados/BR_WC21_PP/BR_Prec01.asc")
plot(prec)
res(prec)
res(modelo)
compareGeom(modelo, prec) # vou ter que usar o resample


# Cada raster de precipitação é o acumulado de um mês
# Então vou fazer a média anual para Amazônia

# Abrir todos os rasters num único arquivo
lista_prec <- list.files("Dados/BR_WC21_PP/", 
                         pattern = "BR_Prec[0-9].\\.asc",
                        full.names = T)
lista_prec

prec_todas <- rast(lista_prec)
prec_todas
crs(prec_todas)
plot(prec_todas[[1]])

# Recortar para Amazônia
prec_amz <- resample(prec_todas, modelo, method = "bilinear")
prec_amz <- project(prec_amz, crs(modelo))
plot(prec_amz[[1]])

prec_amz2 <- mask(prec_amz, amz)
plot(prec_amz2[[1]])

# Tirar a média anual
media <- sum(prec_amz2, na.rm = T)/12
plot(media)
media
compareGeom(media, modelo)

writeRaster(media, "Intermediarios/Precip_media_anual.tif", overwrite = T)

# Salvar mapa

tiff("Img/precipitacao.tif",
     res = 300,
     width = 12, height = 10, units = "cm")

plot(media,
     col = viridis::viridis(option = "turbo", n = 70, direction = 1),
     plg = list(title = "Precipitação \nmédia anual", size = c(.7,1)),
     axes = F)

dev.off()


rm(list = c(
  "media",
  "prec",
  "prec_amz",
  "prec_amz2",
  "prec_todas",
  "lista_prec"
))


# Temperatura  ------------------------------------------------------------

# Cada raster de temperatura é a média de um mês
# Então vou fazer a média anual para Amazônia

# Abrir todos os rasters num único arquivo
lista_temp <- list.files("Dados/BR_WC21_Tmed/", 
                         pattern = "BR_Tmed[0-9].\\.asc",
                         full.names = T)
lista_temp

temp_todas <- rast(lista_temp)
temp_todas
plot(temp_todas[[1]])

# Recortar para Amazônia
temp_amz <- resample(temp_todas, modelo, method = "bilinear")
temp_amz <- project(temp_amz, crs(modelo))
temp_amz
plot(temp_amz[[1]])

temp_amz2 <- mask(temp_amz, amz)
plot(temp_amz2[[1]])

# Tirar a média anual
media <- sum(temp_amz2, na.rm = T)/12
plot(media)
media

writeRaster(media, "Intermediarios/Temp_media_anual.tif", overwrite = T)

# Salvar mapa

tiff("Img/temperatura.tif",
     res = 300,
     width = 12, height = 10, units = "cm")

plot(media,
     col = viridis::viridis(option = "turbo", n = 70, direction = 1),
     plg = list(title = "Temperatura \nmédia anual", size = c(.7,1)),
     axes = F)

dev.off()

rm(list = c(
  "media",
  "temp",
  "temp_amz",
  "temp_amz2",
  "temp_todas",
  "lista_temp"
))



# Elevação e Declividade ----------------------------------------------------

elev <- rast("Dados/BR_WC21_Elev/BR_elev.asc")
plot(elev)
crs(elev)
res(elev)

# Recortar para Amazônia
elev_amz <- resample(elev, modelo, method = "bilinear")
elev_amz <- project(elev_amz, crs(modelo)) |> mask(amz)
plot(elev_amz)

writeRaster(elev_amz, "Intermediarios/Elevacao.tif", overwrite = T)

# Salvar mapa

tiff("Img/elevacao.tif",
     res = 300,
     width = 12, height = 10, units = "cm")

plot(elev_amz,
     col = viridis::viridis(option = "turbo", n = 70, direction = 1),
     plg = list(title = "Elevação", size = c(.7,1)),
     axes = F)

dev.off()


# Calcular declividade a partir da elevação
# Compara a altitude do pixel com seus vizinhos
decl <- terra::terrain(elev_amz, "slope")
decl <- mask(decl, amz)
plot(decl)

writeRaster(decl, "Intermediarios/Declividade.tif", overwrite = T)

# Salvar mapa

tiff("Img/declividade.tif",
     res = 300,
     width = 12, height = 10, units = "cm")

plot(decl,
     col = viridis::viridis(option = "turbo", n = 70, direction = 1),
     plg = list(title = "Declividade", size = c(.7,1)),
     axes = F)

dev.off()


rm(list = c(
  "elev",
  "elev_amz",
  "decl"
))

# Agua no solo ----------------------------------------------------------------

agua <- rast("Dados/cfvo_0-5cm_mean_1000.tif")
plot(agua)
crs(agua)
res(agua)

agua <- agua |> project(crs(modelo))
res(agua) # mudou a resolução

# Recortar para Amazônia
agua_amz <- resample(agua, modelo, method = "bilinear")
agua_amz <- mask(agua_amz, amz)
plot(agua_amz)

writeRaster(agua_amz, "Intermediarios/Agua_solo.tif", overwrite = T)

# Salvar mapa

tiff("Img/agua.tif",
     res = 300,
     width = 12, height = 10, units = "cm")

plot(agua_amz,
     col = viridis::viridis(option = "turbo", n = 70, direction = -1),
     plg = list(title = "Conteúdo de \nágua no solo", size = c(.7,1)),
     axes = F)

dev.off()

rm( list = c(
  "agua",
  "agua_amz"
)
)


# Carbono no solo ----------------------------------------------------------------

carb <- rast("Dados/ocd_0-5cm_mean_1000.tif")
plot(carb)
crs(carb)
res(carb)

carb <- carb |> project(crs(modelo))

# Recortar para Amazônia
carb_amz <- resample(carb, modelo, method = "bilinear")
carb_amz <- mask(carb_amz, amz)
plot(carb_amz)

writeRaster(carb_amz, "Intermediarios/Carbono_solo.tif", overwrite = T)


# Salvar mapa

tiff("Img/carbono_solo.tif",
     res = 300,
     width = 12, height = 10, units = "cm")

plot(carb_amz,
     col = viridis::viridis(option = "turbo", n = 70, direction = 1),
     plg = list(title = "Carbono no \nsolo", size = c(.7,1)),
     axes = F)

dev.off()

rm( list = c(
  "carb",
  "carb_amz"
)
)


# Nitrogênio no solo ----------------------------------------------------------------

nit <- rast("Dados/nitrogen_0-5cm_mean_1000.tif")
plot(nit)
crs(nit)
res(nit)

nit <- nit |> project(crs(modelo))

# Recortar para Amazônia
nit_amz <- resample(nit, modelo, method = "bilinear")
nit_amz <- mask(nit_amz, amz)
plot(nit_amz)

writeRaster(nit_amz, "Intermediarios/Nitrogenio_solo.tif", overwrite = T)

# Salvar mapa

tiff("Img/nitrogenio.tif",
     res = 300,
     width = 12, height = 10, units = "cm")

plot(nit_amz,
     col = viridis::viridis(option = "turbo", n = 70, direction = 1),
     plg = list(title = "Nitrogênio no \nsolo", size = c(.7,1)),
     axes = F)

dev.off()

rm( list = c(
  "nit",
  "nit_amz"
)
)


# Carbono - EBA ----------------------------------------------------------------

carb <- rast("Dados/biomass_Mgha_WGS84.tif")
plot(carb)
crs(carb)
res(carb)

carb <- carb |> project(crs(modelo))
plot(carb)

# Ver onde estão valores de NA
plot(carb, colNA = "red")

# Reamostrar raster
carb_amz <- resample(carb, modelo, method = "bilinear")
carb_amz <- mask(carb_amz, amz)
plot(carb_amz)
plot(carb_amz, colNA = "red")
plot(modelo, colNA = "red")

# Contar celulas q não são NA em cada raster
# Transform non-NA values to 1
carb_amz2 <- carb_amz
carb_amz2[!is.na(carb_amz2)] <- 1
plot(carb_amz2)

# Get frequency of pixels by value
freq(carb_amz2)
freq(modelo) # tem uma parte no modelo que não existe no mapa de biomassa

carb_amz2[is.na(carb_amz2)] <- 0
modelo[is.na(modelo)] <- 0
plot(modelo-carb_amz2)
freq(modelo-carb_amz2)
# Para algumas células, não terei estimativa de carbono

#carb_amz[263530]
#carb_amz[266980]

writeRaster(carb_amz, "Intermediarios/Carbono_eba.tif", overwrite = T)

# Salvar mapa

tiff("Img/carbono_eba.tif",
     res = 300,
     width = 12, height = 10, units = "cm")

plot(carb_amz,
     col = viridis::viridis(option = "turbo", n = 70, direction = 1),
     plg = list(title = "Carbono (tC/ha)", size = c(.7,1)),
     axes = F)

dev.off()

rm(list = c(
  "carb",
  "carb_amz"
)
)


# Cobertura original ------------------------------------------------------

# Carregar dados de uso do solo 1985 - MapBiomas 
orig <- rast("Dados/MapBiomas/r_amz/UsoCobertura_1985.tif")
plot(orig)
res(orig)*111 # ~1km

leg <- read_delim("Dados/MapBiomas/Codigos-da-legenda-colecao-10.csv", delim = "\t")
glimpse(leg)
plot(orig, col = leg$Color)

compareGeom(orig, modelo)
# Reamostrar classes para modelo
orig_amz <- resample(orig, modelo, method = "near")
unique(orig_amz) # manteve valores, ok
orig_amz <- mask(orig_amz, amz)
plot(orig_amz, col = leg$Color)

compareGeom(orig_amz, modelo) # ok

writeRaster(orig_amz, "Intermediarios/Cobertura_orig.tif", overwrite = T)

# Transformar em fator com os rótulos corretos
classes <- unique(orig_amz)
leg_plot <- leg |> filter(Class_ID %in% classes$classification_1985)
leg_plot <- leg_plot[order(leg_plot$Class_ID), ]

orig_amz2 <- orig_amz
levels(orig_amz2) <- data.frame(ID = leg_plot$Class_ID, 
                               value = leg_plot$Descricao)
levels(orig_amz2)[[1]]$value
freq(orig_amz2)
plot(orig_amz2)

cores <- c("#1f8d49", "#7dc975", "#04381d", "#026975", "#7a5900",
           "#519799", "#d6bc74", "#edde8e", "#ffefc3","#ffa07a",
           "#d4271e", "#db4d4f", "#ffaa5f", "#9c0027", "#fc8114",
           "#2532e4", "#9065d0", "#f5b3c8", "#f54ca9","#e6ccff")
 
# Salvar mapa
tiff("Img/cobertura1985.tif",
     res = 300,
     width = 20, height = 10, units = "cm")

plot(orig_amz2, 
     col = cores,
     type = "classes", 
     axes = F,
     plg = list(
       cex = 0.8, # Tamanho da fonte
       ncol = 1,
       size = c(.8,1)
     ),
     colNA = "white"
    )


dev.off()

