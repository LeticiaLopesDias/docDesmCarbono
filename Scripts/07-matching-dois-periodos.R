###
# Pareamento (matching) para análise de desmatamento evitado
# separado por períodos: 1985-2015 e 2016 - 2024
# Data: outubro/2025
# Autor: Letícia Lopes Dias (leticia_lopes@discente.ufg.br)
###

if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,
  MatchIt,
  marginaleffects,
  optmatch,
  survey,
  rlang
)

# Tutorial
# https://kosukeimai.github.io/MatchIt/articles/MatchIt.html

### 1985 a 2015
# Desligar ALTREP para evitar picos de RAM
tb1 <- vroom::vroom("Finais/Dados_85_15.csv", altrep = FALSE)
tb1 <- dplyr::mutate(tb1, cobertura = as.factor(cobertura)) |>
  dplyr::filter(sum != -1) |> 
  dplyr::rename(desmatamento = sum)
  # -1 são as áreas com regeneração, excluir da análise por enquanto
  
mat_q1 <- MatchIt::matchit(
    protecao ~ agua_solo + carbono_solo + declividade +
      d_hidrovias + d_mineracao + d_municipios + d_rodovias +
      elevacao + nitrogenio + precipitacao + temperatura,
    exact = ~ cobertura,
    data = tb1,
    method = "quick",
    distance = "glm",
    estimand = "ATT"
  )
  
balance_mat1 <- summary(mat_q1, standardized = TRUE, pair.dist = FALSE, 
                        improvement = TRUE)
readr::write_rds(balance_mat1, file = "Finais/balance_85_15.rds")
rm(balance_mat1); gc()
  
grDevices::png("Img/balance_85_15.png",
               width = 21, height = 16, units = "cm", res = 300)
plot(summary(mat_q1))
grDevices::dev.off()
  
# Estimar efeito do tratamento
# primeiro, extrair tabela de dados pareados. Esse dataset contém apenas  
# as unidades pareadas e adiciona colunas para distance, weights,and subclass.
m_data1 <- MatchIt::match_data(mat_q1)
glimpse(m_data1)
# Salvar esses dados para conseguir retornar ao valor por área protegida
readr::write_rds(m_data1, file = "Finais/m_data_85_15.rds")

# Limpar memória
rm(tb1, mat_q1); gc()
  
# Utilizar Survey-Weighted Generalized Linear Model
design_mat1 <- survey::svydesign(ids = ~subclass, weights = ~weights, 
                                data = m_data1)
  
mod1 <- survey::svyglm(desmatamento ~ protecao, design_mat1, 
                        family = quasibinomial())
ATT_desm1 <- marginaleffects::avg_comparisons(
    mod1, variables = "protecao",
    newdata = subset(m_data1, protecao == 1),
    wts = "weights"
  )
  
df_result1 <- as.data.frame(ATT_desm1)
# Para estimar a área total de desmatamento evitado:
# Calcular a área total protegida, considerando q cada célula = 1km²
# Multiplicar pelo estimate
df_result1$desm_evitado <- sum(m_data1$protecao == 1) * abs(ATT_desm1$estimate)
df_result1$desm_total   <- sum(m_data1$desmatamento == 1)
glimpse(df_result1)
readr::write_csv2(df_result1, file = stringr::str_c("Finais/resultado_85_15.csv"))
  
# Conferir se tenho o resultado por célula salvo, para depois tirar valor por AP

# Ver se resultado foi salvo corretamente
teste <- read_csv2("Finais/resultado_85_15.csv")
teste$p.value
teste$estimate
teste$desm_evitado
teste$desm_total
# ok

teste <- read_rds("Finais/balance_85_15.rds")
teste

# Limpar memória
rm(list = ls()); gc()



### 2016 a 2024
# com carbono
tb2 <- vroom::vroom("Finais/Dados_16_24.csv", altrep = FALSE)
tb2 <- dplyr::mutate(tb2, cobertura = as.factor(cobertura)) |>
  dplyr::filter(desmatamento != -1)

mat_q2 <- MatchIt::matchit(
  protecao ~ agua_solo + carbono_solo + declividade +
    d_hidrovias + d_mineracao + d_municipios + d_rodovias +
    elevacao + nitrogenio + precipitacao + temperatura,
  exact = ~ cobertura,
  data = tb2,
  method = "quick",
  distance = "glm",
  estimand = "ATT"
)

balance_mat2 <- summary(mat_q2, standardized = TRUE, pair.dist = FALSE, 
                        improvement = TRUE)
readr::write_rds(balance_mat2, file = "Finais/balance_16_24.rds")
rm(balance_mat2); gc()

grDevices::png("Img/balance_16_24.png",
               width = 21, height = 16, units = "cm", res = 300)
plot(summary(mat_q2))
grDevices::dev.off()

# Estimar efeito do tratamento
# primeiro, extrair tabela de dados pareados. Esse dataset contém apenas  
# as unidades pareadas e adiciona colunas para distance, weights,and subclass.
m_data2 <- MatchIt::match_data(mat_q2)
# Salvar esses dados para conseguir retornar ao valor por AP
readr::write_rds(m_data2, file = "Finais/m_data_16_24.rds")

rm(tb2, mat_q2); gc()

design_mat2 <- survey::svydesign(ids = ~subclass, weights = ~weights, 
                                data = m_data2)

mod2 <- survey::svyglm(desmatamento ~ protecao, design_mat2, 
                       family = quasibinomial())
ATT_desm2 <- marginaleffects::avg_comparisons(
  mod2,
  variables = "protecao",
  newdata = subset(m_data2, protecao == 1),
  wts = "weights"
)

df_result2 <- as.data.frame(ATT_desm2)
# Para estimar a área total de desmatamento evitado:
# Calcular a área total protegida, considerando q cada célula = 1km²
# Multiplicar pelo estimate
df_result2$desm_evitado <- sum(m_data2$protecao == 1) * abs(ATT_desm2$estimate)
df_result2$desm_total   <- sum(m_data2$desmatamento == 1)
readr::write_csv2(df_result2, file = "Finais/resultado_16_24.csv")
