###
# Preparar anexos e figuras para o Relatório II (2026)
# Data: janeiro/2026
# Autor: Letícia Lopes Dias (leticia_lopes@discente.ufg.br)
###

if(!require(pacman)) install.packages("pacman")
pacman::p_load(
 tidyverse,
 readxl,
 writexl,
 scales,
 terra,
 sf,
 patchwork
 )


# Carregar extensão da Amazônia
amz <- geobr::read_biomes() |> filter(name_biome == "Amazônia")
amz <- sf::st_transform(amz, "EPSG:4326")
plot(st_geometry(amz))

# Escolher paleta de cores amigável para daltônicos
scales::show_col(pal_viridis(option = "turbo")(6))


# Mapas Covariáveis ------------------------------------------------------

# Criar função:
plotar_cov <- function(cov, leg) {
    r <- rast(paste0("Intermediarios/", cov,".tif"))
    df <- terra::as.data.frame(r, xy=TRUE)
    col <- names(df)[3]

    ggplot() +
      geom_raster(data = df, 
                  aes(x = x, y = y, fill = .data[[col]])) +
      geom_sf(data = amz,  colour = "grey60", fill = NA) +
      viridis::scale_fill_viridis(option = "turbo") +
      labs(x = element_blank(),
          y = element_blank(),
          fill = leg) +
      theme_void() +
      theme(legend.position = "inside",
            legend.position.inside = c(0.87,0.2),
            legend.justification = c(0.4, 0.5),
            legend.box.margin = margin(8,3,8,8),
            plot.background = element_rect(fill = "white", linewidth = 0)
          )
}

list.files("Intermediarios/")

# Elevação
plotar_cov("Elevacao", "Elevação (m)")
ggsave("Img/Elevacao.png", width = 15, height = 11.5, units = "cm")

# Precipitação
plotar_cov("Precip_media_anual", "Precipitação \nmédia anual\n(mm)") +
        viridis::scale_fill_viridis(option = "turbo", 
                                    direction = -1, 
                                    limits = c(0, 350))
ggsave("Img/Precip.png", width = 15, height = 11.5, units = "cm")

# Temperatura
plotar_cov("Temp_media_anual", "Temperatura \nmédia anual\n(°C)") +
        viridis::scale_fill_viridis(option = "turbo", 
                                    limits = c(15, 30))
ggsave("Img/Temperatura.png", width = 15, height = 11.5, units = "cm")

# Nitrogênio
plotar_cov("Nitrogenio_solo", "Nitrogênio \nno solo (g/kg)")
ggsave("Img/Nitrogenio.png", width = 15, height = 11.5, units = "cm")

# Rodovias
plotar_cov("Dist_rodovias", "Distância de\nrodovias (km)") +
        viridis::scale_fill_viridis(option = "turbo", 
                                    direction = -1,
                                    limits = c(0, 580))
ggsave("Img/Rodovias.png", width = 15, height = 11.5, units = "cm")

# Centros urbanos
plotar_cov("Dist_municipios", "Distância \nde centros\nurbanos (km)") +
        viridis::scale_fill_viridis(option = "turbo", 
                                    direction = -1,
                                    limits = c(0, 1200))
ggsave("Img/Centr_urb.png", width = 15, height = 11.5, units = "cm")

# Mineração
plotar_cov("Dist_mineracao", "Distância \nde áreas de\nmineração (km)") +
        viridis::scale_fill_viridis(option = "turbo", 
                                    direction = -1,
                                    limits = c(0, 580))
ggsave("Img/Mineracao.png", width = 15, height = 11.5, units = "cm")

# Hidrovias
plotar_cov("Dist_hidrovias", "Distância de\nhidrovias (km)") +
        viridis::scale_fill_viridis(option = "turbo", 
                                    direction = -1,
                                    limits = c(0, 580))
ggsave("Img/Hidrovias.png", width = 15, height = 11.5, units = "cm")

# Declividade
plotar_cov("Declividade", "Declividade (rad)")
ggsave("Img/Declividade.png", width = 15, height = 11.5, units = "cm")

# Carbono solo
plotar_cov("Carbono_solo", 
           "Densidade \nde carbono\nno solo (kg/m³)")
ggsave("Img/Carb_solo.png", width = 15, height = 11.5, units = "cm")

# Água solo
plotar_cov("Agua_solo", 
           "Volume de\nágua no\nsolo (mm/m)")
ggsave("Img/Agua.png", width = 15, height = 11.5, units = "cm")


# Mapa Carbono EBA -------------------------------------------------------

# Fazer esse sem shape da Amazônia de contorno
carb <- rast("Intermediarios/Carbono_eba.tif")
carb_df <- terra::as.data.frame(carb, xy=TRUE)

ggplot() +
      geom_raster(data = carb_df, 
                  aes(x = x, y = y, fill = biomass_Mgha_WGS84)) +
      viridis::scale_fill_viridis(option = "turbo") +
      labs(x = element_blank(),
          y = element_blank(),
          fill = "Estoque\nde Carbono\n(tC/ha)") +
      theme_void() +
      theme(legend.position = "inside",
            legend.position.inside = c(0.87,0.2),
            legend.justification = c(0.4, 0.5),
            legend.box.margin = margin(8,3,8,8),
            plot.background = element_rect(fill = "white", linewidth = 0)
          )
ggsave("Img/Carb_eba.png", width = 15, height = 11.5, units = "cm")


# Mapas de Uso e Cobertura -----------------------------------------------

## 1985
cob <- rast("Intermediarios/Cobertura_orig.tif")
plot(cob)

# Adicionar legendas MapBiomas
leg <- read_delim("Dados/MapBiomas/Codigos-da-legenda-colecao-10.csv", 
                delim = "\t")
# Legenda: https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2025/08/Legenda-Colecao-10-Descricao-Detalhada-PDF_PT-BR_EN.pdf
glimpse(leg)

classes <- unique(cob)
leg_plot <- leg |> filter(Class_ID %in% classes$classification_1985)
leg_plot <- leg_plot |> 
  mutate(
    Color = fct_reorder(Color, Class_ID)    
  )

# Trasnformar classes de uso em fator 
levels(cob) <- data.frame(ID = leg_plot$Class_ID, 
                          classe = leg_plot$Descricao)
levels(cob)[[1]]$classe
freq(cob)
plot(cob)

cob_df <- terra::as.data.frame(cob, xy=TRUE)
glimpse(cob_df)

ggplot() +
  geom_raster(data = cob_df, 
              aes(x = x, y = y, fill = classe)) +
  geom_sf(data = amz,  colour = "grey60", fill = NA) +
  scale_fill_manual(values = as.character(leg_plot$Color)) +
  labs(x = element_blank(),
        y = element_blank(),
        fill = element_blank()) +
  guides(fill = guide_legend(ncol = 4, byrow = TRUE)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.box.margin = margin(.1,.1,.1,.1),
        plot.background = element_rect(fill = "white", linewidth = 0)
        )

ggsave("Img/Cob_orig.png", width = 20, height = 19, units = "cm")

## 2024

cob24 <- rast("Intermediarios/Cobertura_final.tif")
plot(cob24)

classes2 <- unique(cob24)
leg_plot2 <- leg |> filter(Class_ID %in% classes2$classification_2024)
leg_plot2 <- leg_plot2 |> 
  mutate(
    Color = fct_reorder(Color, Class_ID)    
  )

# Trasnformar classes de uso em fator 
levels(cob24) <- data.frame(ID = leg_plot2$Class_ID, 
                            classe = leg_plot2$Descricao)
levels(cob24)[[1]]$classe
freq(cob24)
plot(cob24)

cob24_df <- terra::as.data.frame(cob24, xy=TRUE)
glimpse(cob24_df)

ggplot() +
  geom_raster(data = cob24_df, 
              aes(x = x, y = y, fill = classe)) +
  geom_sf(data = amz,  colour = "grey60", fill = NA) +
  scale_fill_manual(values = as.character(leg_plot2$Color)) +
  labs(x = element_blank(),
        y = element_blank(),
        fill = element_blank()) +
  guides(fill = guide_legend(ncol = 4, byrow = TRUE)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.box.margin = margin(.1,.1,.1,.1),
        plot.background = element_rect(fill = "white", linewidth = 0)
        )

ggsave("Img/Cob_final.png", width = 20, height = 20, units = "cm")


# Desmatamento -----------------------------------------------------------
# Fazer mapa do desmatamento total e gráfico desmatamento total x evitado

lista <- list.files("Intermediarios/", pattern = "Desm", full.names = T)
# Remover da lista arquivos com desmatamento acumulado por período
remover <- c("Intermediarios/Desm_16_24.tif", "Intermediarios/Desm_86_15.tif")
lista2 <- lista[!lista %in% remover]
desm <- rast(lista2)

desm_total <- sum(desm)
# remover áreas restauradas (-1)
desm_total <- subst(desm_total, -1, NA)
plot(desm_total)

# Salvar mapa
desm_df <- terra::as.data.frame(desm_total, xy=TRUE)

ggplot() +
      geom_raster(data = desm_df, 
                  aes(x = x, y = y, fill = as.factor(sum))) +
      geom_sf(data = amz,  colour = "grey60", fill = NA) +
      scale_fill_manual(values = c("#46F884FF", "#7A0403FF")) +
      labs(x = element_blank(),
          y = element_blank(),
          title = "Perda de vegetação nativa de 1985 a 2024"
        ) +
      theme_void() +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "white", linewidth = 0),
        plot.title = element_text(hjust = 0.1)
          )
ggsave("Img/Desmatamento_mapa.png", width = 17, height = 13.5, units = "cm")

# Fazer gráfico
dados <- readxl::read_xlsx("Finais/resultado_por_ano.xlsx")
glimpse(dados)

dados |> 
  select(ano, desm_evitado, desm_total) |> 
  rename(
    Evitado = desm_evitado,
    Total = desm_total
  ) |> 
  pivot_longer(cols = c(Evitado, Total), 
               values_to = "desm", names_to = "tipo") |> 
  ggplot() +
  geom_col(aes(x = ano, y = desm, fill = tipo),
           position = position_dodge()) + 
  scale_fill_manual(values = c("#28BBECFF","#7A0403FF")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     labels = scales::number_format(decimal.mark = ",",
                                                    big.mark = "."),
                     n.breaks = 7) +
  scale_x_continuous(n.breaks = 8,
                     limits = c(1985.5,2024.5),
                     expand = expansion(mult = c(0.01, 0.01)),) +
  labs(x = element_blank(),
       y = "Desmatamento (km²)",
       fill = element_blank()) +
  theme_classic() +
  theme(legend.position = "inside", 
        legend.position.inside = c(.75, .8))

ggsave("Img/Desmatamento_graf.png", width = 15, height = 9,
       units = "cm")



# Gráfico ATT ------------------------------------------------------------

dados |> 
  mutate(ano = ano) |> 
  ggplot(aes(x = ano, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high), fill = "grey70") +
  geom_point(size = .7) +
  geom_line() +
  labs(x = element_blank(),
       y = "Efeito estimado do tratamento") +
  scale_y_continuous(labels = scales::number_format(decimal.mark = ",",
                                                    big.mark = "."),
                     n.breaks = 7) +
  scale_x_continuous(n.breaks = 8,
                     limits = c(1985.5,2024.5),
                     expand = expansion(mult = c(0.01, 0.01))) +
  theme_classic()

ggsave("Img/Estimate.png", width = 15, height = 9,
       units = "cm")


# Gráfico Dif Media --------------------------------------------------------

bal <- readxl::read_xlsx("Finais/balance_por_ano.xlsx")
glimpse(bal)
unique(bal$variavel)

bal |> 
  filter(!str_detect(variavel, "cobertura*")) |> 
  filter(!str_detect(variavel, "distance")) |> 
  mutate(
        variavel = case_when(
                variavel == "agua_solo" ~ "Vol. Água",
                variavel == "carbono_solo" ~ "Carbono no Solo",
                variavel == "declividade" ~ "Declividade",
                variavel == "d_hidrovias" ~ "Dist. Hidrovias",
                variavel == "d_rodovias" ~ "Dist. Rodovias",
                variavel == "d_mineracao" ~ "Dist. Mineração",
                variavel == "d_municipios" ~ "Dist. Centros Urbanos",
                variavel == "elevacao" ~ "Elevação",
                variavel == "nitrogenio" ~ "Nitrogênio no Solo",
                variavel == "precipitacao" ~ "Precipitação",
                variavel == "temperatura" ~ "Temperatura",
                .default = variavel
        )
  ) |> 
  ggplot() +
  geom_point(aes(x = ano, y = std_mean_diff_after), size = .7) + 
  geom_hline(aes(yintercept = .1), color = "#46F884FF") +
  geom_hline(aes(yintercept = .25), color = "#7A0403FF") +
  labs(x = element_blank(),
       y = "Diferença média padronizada \npós-pareamento") +
  facet_wrap(~ variavel) +
  theme_bw()

ggsave("Img/Dif_media_pos.png", width = 20, height = 18, units = "cm")


# Desm Evitado -----------------------------------------------------------

# Mapa de desmatamento evitado
d <- rast("Finais/Desm_evitado.tif")
crs(d)
plot(d)
crs(amz)

# Carregar extensão de cada classe de AP, para o mapa
ucs_amz <- st_read("Intermediarios/ucs.gpkg")
tis_amz <- st_read("Intermediarios/tis.gpkg")
quil_amz <- st_read("Intermediarios/quilombos.gpkg")

# Salvar mapa
d_df <- terra::as.data.frame(d, xy=TRUE)

ggplot() +
      geom_sf(data = amz,  fill = "grey80", colour = NA) +
      geom_raster(data = d_df, 
                  aes(x = x, y = y, fill = as.factor(desm_evitado))) +
      scale_fill_manual(values = c("#46F884FF", "#7A0403FF")) +
      geom_sf(data = ucs_amz,  colour = "white", fill = NA, linewidth = .1) +
      geom_sf(data = tis_amz,  colour = "white", fill = NA, linewidth = .1) +
      geom_sf(data = quil_amz,  colour = "white", fill = NA, linewidth = .1) +
      labs(x = element_blank(),
          y = element_blank(),
          title = "Desmatamento evitado pelas áreas protegidas de 2016 a 2024"
        ) +
      theme_void() +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "white", linewidth = 0),
        plot.title = element_text(hjust = 0.1)
          )
ggsave("Img/Desmatamento_evit.png", width = 17, height = 13.5, units = "cm")


# Gráfico de desmatamento evitado por categoria AP
aps <- vroom::vroom("Finais/Res_APs.csv")
glimpse(aps)

write_csv2(aps, "Docs/dados_aps.csv")

# Desmatamento evitado
aps |> 
  group_by(tipo) |> 
  summarise(total = sum(desm_evit, na.rm = T)) |>
  mutate(tipo = case_when(
    tipo == "uc" ~ "Unidade de\nConservação",
    tipo == "tq" ~ "Território\nQuilombola",
    tipo == "ti" ~ "Terra\nIndígena",
    .default = tipo
  )) |> 
  mutate(
    tipo = fct_reorder(tipo, total)
  ) |> 
  ggplot() +
  geom_col(aes(x = total, y = tipo),
          width = .6) +
  geom_text(aes(x = total+3500, y = tipo, label = scales::number(total, big.mark = "."))) +
  scale_x_continuous(expand = expansion(mult = c(0,.07)),
                     n.breaks = 7,
                     labels = scales::number_format(big.mark = ".")) +
  labs(x = "Desmatamento evitado (km²)",
       y = element_blank()) +
  theme_classic()

ggsave("Img/Desmatamento_evit_graf.png", width = 15, height = 9,
       units = "cm")

# Extensão por classe
aps |> 
  group_by(tipo) |> 
  summarise(total = sum(area, na.rm = T)) |>
  mutate(tipo = case_when(
    tipo == "uc" ~ "Unidade de\nConservação",
    tipo == "tq" ~ "Território\nQuilombola",
    tipo == "ti" ~ "Terra\nIndígena",
    .default = tipo
  )) |> 
  mutate(
    tipo = fct_reorder(tipo, total),
    total = total/100 # converter de ha para km2
  ) |> 
  ggplot() +
  geom_col(aes(x = total, y = tipo),
          width = .6) +
  geom_text(aes(x = total+10^5, y = tipo, label = scales::number(total, big.mark = "."))) +
  scale_x_continuous(expand = expansion(mult = c(0,.08)),
                     n.breaks = 7,
                     labels = scales::number_format(big.mark = ".")) +
  labs(x = "Área (km²)",
       y = element_blank()) +
  theme_classic()

ggsave("Img/ExtensaoAPs.png", width = 15, height = 9,
       units = "cm")
