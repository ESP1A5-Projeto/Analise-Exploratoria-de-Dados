# ==============================================================================
# Biblioteca necessária
library(ggplot2)

# ==============================================================================
# PRIMEIRA ANÁLISE
# Estimativa de probabilidade de vacinação 
# Filtrar apenas os casos com valores válidos (1 e 2)
data_valid <- subset(INFLUD13, VACINA %in% c(1, 2))

# Calcular as probabilidades
prob_vacinado <- prop.table(table(data_valid$VACINA))[1]  # Probabilidade de ter se vacinado (valor 1)
prob_nao_vacinado <- prop.table(table(data_valid$VACINA))[2]  # Probabilidade de não ter se vacinado (valor 2)

# Exibir as probabilidades
print(prob_vacinado)
print(prob_nao_vacinado)

# Dados das probabilidades
probabilidades <- data.frame(
  Categoria = c("Vacinação", "Não Vacinação"),
  Probabilidade = c(prob_vacinado, prob_nao_vacinado)
)

# Criar o gráfico de barras
grafico <- ggplot(probabilidades, aes(x = Categoria, y = Probabilidade)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(x = "Categoria", y = "Probabilidade") +
  ggtitle("Probabilidade de Vacinação contra Gripe") +
  theme_minimal()

# Exibir o gráfico
print(grafico)

# =======================================================================================
# SEGUNDA ANÁLISE
# Estimativa de probabilidade de vacinação entre pardos e pretos
# Filtrar apenas os casos com valores válidos (1 e 2) nas variáveis de interesse
data_valid <- subset(INFLUD13, VACINA %in% c(1, 2) & CS_RACA %in% c(2, 4))

# Calcular as probabilidades
total_pessoas <- nrow(data_valid)  # Total de pessoas pretas ou pardas com valores válidos
total_vacinados <- sum(data_valid$VACINA == 1)  # Total de pessoas pretas ou pardas vacinadas (valor 1)

prob_vacinado_preta_parda <- total_vacinados / total_pessoas  # Probabilidade de pessoa preta ou parda ter se vacinado

# Exibir a probabilidade
print(prob_vacinado_preta_parda)

# Dados do gráfico
dados_grafico <- c(prob_vacinado_preta_parda, 1 - prob_vacinado_preta_parda)
nomes_grafico <- c("Vacinado", "Não Vacinado")
cores_grafico <- c("green", "red")

# Gráfico de barras
barplot(dados_grafico, main = "Probabilidade de Vacinação em Pessoas Pretas ou Pardas",
        xlab = "Categoria", ylab = "Probabilidade", ylim = c(0, 1),
        col = cores_grafico, names.arg = nomes_grafico)

# =====================================================================================================
# TERCEIRA ANÁLISE
# Estimativa de probabilidade de vacinação entre pardos e pretos do sexo feminino
# Filtrar apenas os casos com valores válidos (1 e 2) nas variáveis de interesse
data_valid <- subset(INFLUD13, VACINA %in% c(1, 2) & CS_RACA %in% c(2, 4) & CS_SEXO == "F")

# Calcular as probabilidades
total_pessoas <- nrow(data_valid)  # Total de pessoas pretas ou pardas do sexo feminino com valores válidos
total_nao_vacinados <- sum(data_valid$VACINA == 2)  # Total de pessoas pretas ou pardas do sexo feminino não vacinadas (valor 2)

prob_nao_vacinado_preta_parda_feminino <- total_nao_vacinados / total_pessoas  # Probabilidade de pessoa preta ou parda do sexo feminino não ter se vacinado

# Exibir a probabilidade
print(prob_nao_vacinado_preta_parda_feminino)


# Filtrar apenas os casos com valores válidos (1 e 2) nas variáveis de interesse
data_valid <- subset(INFLUD13, VACINA %in% c(1, 2) & CS_RACA %in% c(2, 4) & CS_SEXO %in% c("M", "F"))

# Calcular as proporções
prop_nao_vacinado <- prop.table(table(data_valid$CS_RACA, data_valid$CS_SEXO, data_valid$VACINA))[2, , 2] # Proporção de não vacinados (valor 2)

# Criar um dataframe para o gráfico
df <- data.frame(CS_RACA = c("Preta", "Parda"), CS_SEXO = c("Masculino", "Feminino"), Proporcao_Nao_Vacinado = prop_nao_vacinado)

# Criar o gráfico de barras
ggplot(df, aes(x = CS_RACA, y = Proporcao_Nao_Vacinado, fill = CS_SEXO)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Raça/Cor", y = "Proporção de Não Vacinados", fill = "Sexo") +
  ggtitle("Proporção de Não Vacinados por Raça/Cor e Sexo") +
  theme_minimal()





