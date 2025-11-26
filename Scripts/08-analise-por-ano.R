###
# Análise dos resultados de matching
# outubro/2025
###

if(!require(pacman)) install.packages("pacman")
pacman::p_load(
 tidyverse
)


# Tabela de Resultados ----------------------------------------------------

# Criar tabela com ano x desmatamento total x desmatamento evitado 
# e outros resultados da regressão

### Resultados
# Define o diretório onde estão os arquivos
caminho <- "Finais/"

# Lista todos os arquivos que seguem o padrão
arquivos <- list.files(path = caminho, pattern = "^resultado_\\d{4}\\.csv$", 
                       full.names = TRUE)

# Lê e empilha todos os csv
dados <- arquivos |> 
  # cria uma tabela com o nome do arquivo e o conteúdo
  map_df(~ read_csv2(.x) |> 
           mutate(ano = str_extract(basename(.x), "\\d{4}")))  |> 
  relocate(ano, .before = term) |> 
  mutate(ano = as.integer(ano))

# Visualiza o resultado
glimpse(dados)
dados2 <- dados |> 
  mutate(ano = ano+1)
# Somar +1 no ano, porque na verdade o resultado diz respeito ao desmamento
# no ano seguinte ao do que está no nome do arquivo

writexl::write_xlsx(dados2, "Finais/resultado_por_ano.xlsx")


### Qualidade matching

b1 <- readRDS("Finais/balance_1985.rds")
glimpse(b1$reduction)
str(b1)

b1
class(b1$sum.all)
antes  <- as.data.frame(b1$sum.all)
head(antes)
names(antes)
antes$variavel  <- rownames(b1$sum.all)


# Lista todos os arquivos que seguem o padrão
arquivos <- list.files(path = caminho, pattern = "^balance_\\d{4}\\.rds$", 
                       full.names = TRUE)

# Ler e extrair informações relevantes de cada arquivo
dados_balance <- map_dfr(arquivos, function(arq) {
  
  ano <- str_extract(basename(arq), "\\d{4}") %>% as.integer()
  
  obj <- readRDS(arq)
  
  # Converter as tabelas num data frame
  antes  <- as.data.frame(obj$sum.all)
  depois <- as.data.frame(obj$sum.matched)
  reducao <- as.data.frame(obj$reduction)
  
  # Adicionar nome das variáveis
  antes$variavel  <- rownames(obj$sum.all)
  depois$variavel <- rownames(obj$sum.matched)
  reducao$variavel <- rownames(obj$reduction)
  
  # Selecionar apenas o Std. Mean Diff.
  antes <- antes  |> 
    select(variavel, std_mean_diff_before = `Std. Mean Diff.`)
  depois <- depois  |> 
    select(variavel, std_mean_diff_after = `Std. Mean Diff.`)
  reducao <- reducao  |> 
    select(variavel, reduction = `Std. Mean Diff.`)
  
  # Juntar as três tabelas
  joined <- antes |> 
    left_join(depois, by = "variavel") |> 
    left_join(reducao, by = "variavel")  |> 
    mutate(ano = ano)
  
  joined
})

# 3. Reorganizar colunas
dados_balance <- dados_balance |> 
  select(ano, variavel, std_mean_diff_before, 
         std_mean_diff_after, reduction
         )
glimpse(dados_balance)

# Salvar o resultado
writexl::write_xlsx(dados_balance, "Finais/balance_por_ano.xlsx")



# Análise Resultados por Ano ----------------------------------------------

bal <- readxl::read_xlsx("Finais/balance_por_ano.xlsx")
glimpse(bal)

bal |> 
  ggplot() +
  geom_point(aes(x = ano, y = reduction)) +
  facet_wrap(~ variavel)

bal |> 
  ggplot() +
  geom_point(aes(x = ano, y = std_mean_diff_after), size = .7) + 
  geom_hline(aes(yintercept = .1), color = "forestgreen") +
  geom_hline(aes(yintercept = .25), color = "darkred") +
  labs(x = element_blank(),
       y = "Diferença média padronizada \npós-pareamento") +
  facet_wrap(~ variavel) +
  theme_bw()
ggsave("Img/Dif_media_pos.png", width = 21, height = 23,
       units = "cm")

bal |> 
  ggplot() +
  geom_point(aes(x = ano, y = std_mean_diff_before)) + 
  geom_hline(aes(yintercept = .1), color = "green") +
  geom_hline(aes(yintercept = .25), color = "red") +
  facet_wrap(~ variavel)

# Como usei exact = ~ cobertura, o MatchIt garante que cada obs. tratada
# seja pareada apenas com controles da mesma classe de cobertura

# Por isso, as dif. padronizadas são muito próximas de zero. O pareamento
# impediu combinações entre categorias diferentes.

bal |> 
  group_by(ano) |> 
  filter(variavel != "distance") |> 
  summarise(
    media_an = mean(std_mean_diff_before, na.rm = TRUE),
    media_de  = mean(std_mean_diff_after, na.rm = TRUE),
    media_red = mean(reduction, na.rm = TRUE)
  ) |> 
  ggplot() +
  geom_point(aes(x = ano, y = media_de)) + 
  geom_hline(aes(yintercept = .1), color = "green") +
  geom_hline(aes(yintercept = .25), color = "red")



## Resultados da regresão
dados <- readxl::read_xlsx("Finais/resultado_por_ano.xlsx")
glimpse(dados)

dados |> 
  ggplot() +
  geom_point(aes(x = ano, y = estimate))

dados |> 
  ggplot() +
  geom_point(aes(x = ano, y = p.value))
# Todos os anos tiveram resultado p <.5

viridis::turbo(n = 5)


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

ggsave("Img/Desmatamento.png", width = 15, height = 9,
       units = "cm")


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
