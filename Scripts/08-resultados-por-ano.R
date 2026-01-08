###
# Organiza e explora os resultados do matching por ano (1985-2024)
# Data: outubro/2025
# Autor: Letícia Lopes Dias (leticia_lopes@discente.ufg.br)
###

if(!require(pacman)) install.packages("pacman")
pacman::p_load(
 tidyverse
)

# Organizar dados de resultados -----------------------------------------------

### Resultados do matching
# Definir diretório onde estão os arquivos
caminho <- "Finais/"

# Listar todos os arquivos que seguem o padrão
arquivos <- list.files(path = caminho, pattern = "^resultado_\\d{4}\\.csv$", 
                       full.names = TRUE)

# Ler e empilhar todos os csv
dados <- arquivos |> 
  # cria uma tabela com o nome do arquivo e o conteúdo
  map_df(~ read_csv2(.x) |> 
           mutate(ano = str_extract(basename(.x), "\\d{4}")))  |> 
  relocate(ano, .before = term) |> 
  mutate(ano = as.integer(ano))

# Visualizar o resultado
glimpse(dados)
dados2 <- dados |> 
  mutate(ano = ano+1)
# Somar +1 no ano, porque na verdade o resultado diz respeito ao desmamento
# no ano seguinte ao do que está no nome do arquivo

# Savar tabela de resultados
writexl::write_xlsx(dados2, "Finais/resultado_por_ano.xlsx")


### Dados de qualidade do matching
# Abrir arquivos salvos como 'balance'
b1 <- readRDS("Finais/balance_1985.rds")
glimpse(b1$reduction)
str(b1)

b1
class(b1$sum.all)
antes  <- as.data.frame(b1$sum.all)
head(antes)
names(antes)
antes$variavel  <- rownames(b1$sum.all)

# Listar todos os arquivos que seguem o padrão
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

# Reorganizar colunas
dados_balance <- dados_balance |> 
  select(ano, variavel, std_mean_diff_before, 
         std_mean_diff_after, reduction
         )
glimpse(dados_balance)

# Salvar o resultado
writexl::write_xlsx(dados_balance, "Finais/balance_por_ano.xlsx")


# Explorar os resultados ----------------------------------------------

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