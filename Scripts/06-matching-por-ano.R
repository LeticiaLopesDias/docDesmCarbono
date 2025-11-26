###
# Pareamento (matching) para análise de desmatamento - Relatório II
# agosto/2025
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


dados <- vroom::vroom("Finais/Dados_1985.csv")
glimpse(dados)
table(dados$protecao)

dados <- dados |> 
  mutate(
    cobertura = as.factor(cobertura)
  )

# Matching com amostra ----------------------------------------------------

# Fazer teste com uma amostra

amostra <- dados |> 
  group_by(protecao) |> 
  slice_sample(n = 20000)  |> 
  ungroup()

glimpse(amostra)
amostra |> count(protecao)

# Rodar quick matching
mat_q <- matchit(protecao ~ agua_solo + carbono_solo + declividade +
                   d_hidrovias + d_mineracao + d_municipios + d_rodovias +
                   elevacao + nitrogenio + precipitacao + temperatura, 
                 exact = ~cobertura,
                 data = amostra, 
                 method = "quick",
                 distance = "glm",
                 estimand = "ATT")

mat_q
# Objeto de resultados guarda: weights (the computed matching weights), 
# subclass (matching pair membership), distance (the estimated 
# propensity score), and match.matrix (which control units are matched to 
# each treated unit) 

summary(mat_q)
# Coluna Std. Pair Diff mostra a média absoluta  da diferença entre os  
# pares para cada covariável 

plot(mat_q, type = "jitter", interactive = FALSE)

plot(mat_q, type = "density", interactive = FALSE,
     which.xs = ~d_hidrovias + d_rodovias + declividade)
dev.off()

plot(summary(mat_q))

rm(
  list = 
    c("amostra",
      "mat_nn",
      "mat_q")
)

# Matching por ano ----------------------------------------------------
# https://kosukeimai.github.io/MatchIt/articles/MatchIt.html


# Criar função para rodar por ano
calcula_matching <- function(ano) {
  
  tb <- vroom::vroom(str_c("Finais/Dados_", ano, ".csv"))
  col_desm <- sym(paste0("deforestation_", ano))
  tb <- tb |> 
    mutate(
      cobertura = as.factor(cobertura)
    ) |>
    rename(deforestation = !!col_desm) |> 
  # -1 são as áreas com regeneração, excluir da análise por enquanto
  filter(deforestation != -1)
  
  # Quick matching
  mat_q <- matchit(protecao ~ agua_solo + carbono_solo + declividade +
                     d_hidrovias + d_mineracao + d_municipios + d_rodovias +
                     elevacao + nitrogenio + precipitacao + temperatura, 
                   exact = ~cobertura,
                   data = tb, 
                   method = "quick",
                   distance = "glm",
                   estimand = "ATT"
  )

  balance_mat <- summary(mat_q, 
                         standardized = T, 
                         pair.dist = F, 
                         improvement = T)

    # Salvar resultados do matching
  write_rds(balance_mat, file = str_c("Finais/balance_", ano,".rds")) 
  
  # Salvar gráfico de resumo do balanço final
  png(str_c("Img/balance_", ano, ".png"),
      width = 21, height = 16, units = "cm", res = 300)
  plot(summary(mat_q))
  dev.off()
  
  # Estimar efeito do tratamento
  # primeiro, extrair tabela de dados pareados. Esse dataset contém apenas  
  # as unidades pareadas e adiciona colunas para distance, weights,and subclass.
  m_data <- match_data(mat_q)
  
  # Modelamos o resultado usando funções comuns para regressões como lm() or glm()
  # incluindo os weights do matching. 
  # Depois, usamos marginaleffects::avg_comparisons() para realizar g-computations 
  # e estimar o ATT.
  
  # Uilizar Survey-Weighted Generalized Linear Model
  # adequado quando matching é feito com reposição (indicação Luis)
  design_mat <- svydesign(ids = ~subclass, weights = ~weights, data = m_data)
  
  # Primeiro, com desmatamento final
  mod <- svyglm(deforestation ~ protecao, 
                design_mat, 
                family = quasibinomial()
  )
  
  ATT_desm <- avg_comparisons(mod, 
                              variables = "protecao",
                              newdata = subset(m_data, protecao == 1),
                              wts = "weights")
  
  # Salvar resultado
  # Converter valores em data frame
  df_result <- as.data.frame(ATT_desm) 
  # Para estimar a área total de desmatamento evitado:
  # Calcular a área total protegida, considerando q cada célula = 1km²
  # Multiplicar pelo estimate
  df_result$desm_evitado <- sum(m_data$protecao == 1) * abs(ATT_desm$estimate)
  df_result$desm_total <- sum(m_data$deforestation == 1)
  write_csv2(df_result, file = str_c("Finais/resultado_", ano, ".csv"))
  
}


calcula_matching <- function(ano) {
  on.exit({
    try({
      while (grDevices::dev.cur() > 1) grDevices::dev.off()
    }, silent = TRUE)
    rm(list = c("tb","tb_small","mat_q","balance_mat","m_data",
                "design_mat","mod","ATT_desm","df_result"))
    gc()
  }, add = TRUE)
  
  # Desligue ALTREP para evitar picos de RAM
  tb <- vroom::vroom(
    stringr::str_c("Finais/Dados_", ano, ".csv"),
    altrep = FALSE
  )
  
  col_desm <- rlang::sym(paste0("deforestation_", ano))
  tb <- dplyr::mutate(tb, cobertura = as.factor(cobertura)) |>
    dplyr::rename(deforestation = !!col_desm) |>
    dplyr::filter(deforestation != -1)
  # -1 são as áreas com regeneração, excluir da análise por enquanto
  
  # Reduza o data.frame ANTES do matchit (só o que entra no modelo)
  tb_small <- dplyr::select(
    tb, deforestation, protecao, cobertura,
    agua_solo, carbono_solo, declividade,
    d_hidrovias, d_mineracao, d_municipios, d_rodovias,
    elevacao, nitrogenio, precipitacao, temperatura
  )
  rm(tb); gc()
  
  mat_q <- MatchIt::matchit(
    protecao ~ agua_solo + carbono_solo + declividade +
      d_hidrovias + d_mineracao + d_municipios + d_rodovias +
      elevacao + nitrogenio + precipitacao + temperatura,
    exact = ~ cobertura,
    data = tb_small,
    method = "quick",
    distance = "glm",
    estimand = "ATT"
  )
  
  balance_mat <- summary(mat_q, standardized = TRUE, pair.dist = FALSE, 
                         improvement = TRUE)
  readr::write_rds(balance_mat, file = stringr::str_c("Finais/balance_", ano,".rds"))
  rm(balance_mat); gc()
  
  grDevices::png(stringr::str_c("Img/balance_", ano, ".png"),
                 width = 21, height = 16, units = "cm", res = 300)
  plot(summary(mat_q))
  grDevices::dev.off()
  
  # Estimar efeito do tratamento
  # primeiro, extrair tabela de dados pareados. Esse dataset contém apenas  
  # as unidades pareadas e adiciona colunas para distance, weights,and subclass.
  m_data <- MatchIt::match_data(mat_q)
  rm(tb_small, mat_q); gc()
  
  # Modelamos o resultado usando funções comuns para regressões como lm() or glm()
  # incluindo os weights do matching. 
  # Depois, usamos marginaleffects::avg_comparisons() para realizar g-computations 
  # e estimar o ATT.
  
  # Uilizar Survey-Weighted Generalized Linear Model
  # adequado quando matching é feito com reposição (indicação Luis)
  design_mat <- survey::svydesign(ids = ~subclass, weights = ~weights, data = m_data)
  
  mod <- survey::svyglm(deforestation ~ protecao, design_mat, 
                        family = quasibinomial())
  ATT_desm <- marginaleffects::avg_comparisons(
    mod, variables = "protecao",
    newdata = subset(m_data, protecao == 1),
    wts = "weights"
  )
  
  df_result <- as.data.frame(ATT_desm)
  # Para estimar a área total de desmatamento evitado:
  # Calcular a área total protegida, considerando q cada célula = 1km²
  # Multiplicar pelo estimate
  df_result$desm_evitado <- sum(m_data$protecao == 1) * abs(ATT_desm$estimate)
  df_result$desm_total   <- sum(m_data$deforestation == 1)
  readr::write_csv2(df_result, file = stringr::str_c("Finais/resultado_", ano, ".csv"))
  
  invisible(NULL)
}


calcula_matching("1985")

# Ver se resultado foi salvo corretamente
teste <- read_csv2("Finais/resultado_1985.csv")
teste$p.value
teste$estimate
teste$desm_evitado
teste$desm_total
# ok, salvo corretamente 

# ATT_desm
# Estimate = -0,0004
# Interpretação:
# O valor -0,0004 significa uma redução de 0,04 pontos percentuais na chance de 
# desmatamento por célula.
# Obs: na função avg_comparisons(), estimativa já está na escala da resposta, 
# ou seja, na escala da probabilidade.
# Se olhasse o coef do modelo (summary(mod)), aí sim ele estaria na escala do 
# link logit, ou seja, seria um log-odds, e você precisaria converter para
# probabilidade usando a função logística

# A análise indica em 1985, as áreas protegidas da Amazônia 
# reduziram em média a chance de desmatamento das células em 0,04 % 
# Isso representa um desmatamento evitado de ~ 143 km² no período.

# teste <- read_rds("Finais/balance_1985.rds")

# Rodar cada ano em processos separados (para liberar memória)
purrr::walk(2007:2023, ~ callr::r(calcula_matching, args = list(.x)), 
            .progress = TRUE)
