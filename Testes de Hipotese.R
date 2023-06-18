# TESTES DE HIPÓTESE

# ============================================================================================================
# PRIMEIRA HIPÓTESE
# Hipótese: Existe diferença na média de idade (NU_IDADE_N) entre pacientes com febre (FEBRE) e sem febre (FEBRE)?
# Hipótese nula (H0): Não há diferença na média de idade entre pacientes com febre e sem febre
# Hipótese alternativa (Ha): Existe diferença na média de idade entre pacientes com febre e sem febre

# Variaveis: 1. Idade, 2. Sinais e sintomas - Febre

# Filtrar as primeiras 1000 Amostras
amostra_idade <- head(INFLUD13$NU_IDADE_N, 1000)
amostra_febre <- head(INFLUD13$FEBRE, 1000)

# Filtrar os pacientes com febre e sem febre
idade_com_febre <- amostra_idade[amostra_febre == 1]
idade_sem_febre <- amostra_idade[amostra_febre == 2]

# Teste t de Student
Teste <- t.test(idade_com_febre, idade_sem_febre)
Teste


L# ============================================================================================================
# SEGUNDA HIPÓTESE
# Hipótese: Existe uma associação entre a presença de febre (FEBRE) e a presença de tosse (TOSSE)?
# Hipótese nula (H0): Não há associação entre a presença de febre (FEBRE) e a presença de tosse (TOSSE)
# Hipótese alternativa (Ha): A presença de febre (FEBRE) está associada à presença de tosse (TOSSE).

# Variaveis: 1. Sinais e sintomas - Febre, 2. Sinais e sintomas - Tosse

# Filtrar as primeiras 1000 Amostras
amostra_febre <- head(INFLUD13$FEBRE, 1000)
amostra_tosse <- head(INFLUD13$TOSSE, 1000)

# Tabela de Contingencia
Tabela <- table(amostra_febre, amostra_tosse)

# Teste qui-quadrado
Teste <- chisq.test(Tabela)
Teste


# ============================================================================================================
# TERCEIRA HIPÓTESE
# Hipótese: A média de idade (NU_IDADE_N) varia de acordo com a raça/cor (CS_RACA)?
# Hipótese nula (H0): Não há diferença na média de idade entre as diferentes categorias de raça/cor
# Hipótese alternativa (Ha): Pelo menos uma das médias de idade difere das outras categorias de raça/cor

# Variaveis: 1. Idade, 2. Sinais e sintomas - Febre

# Filtrar as primeiras 1000 Amostras
amostra_idade <- head(INFLUD13$NU_IDADE_N, 1000)
amostra_raca <- head(INFLUD13$CS_RACA, 1000)

# Converter a variável categórica em um fator
amostra_raca <- as.factor(amostra_raca)

# Teste ANOVA
Teste <- aov(amostra_idade ~ amostra_raca)

# Visualizando Resultado
summary(Teste)


# ============================================================================================================
# QUARTA HIPÓTESE
# Hipótese: A proporção de pacientes com tosse (TOSSE) é diferente entre os grupos de gestantes (CS_GESTANT)?
# Hipótese nula (H0): A proporção de pacientes com tosse é igual entre os grupos de gestantes
# Hipótese alternativa (Ha): A proporção de pacientes com tosse é diferente entre os grupos de gestantes

# Variaveis: 1. Sinais e sintomas - Tosse, 2. Gestante

# Filtrar as primeiras 1000 Amostras
amostra_tosse <- head(INFLUD13$TOSSE, 1000)
amostra_gestante <- head(INFLUD13$CS_GESTANT, 1000)

# Tabela de contingência
Tabela <- table(amostra_tosse, amostra_gestante)

# Teste do qui-quadrado
Teste <- chisq.test(Tabela)
Teste


# ============================================================================================================
# QUINTA HIPÓTESE
# Hipótese: Existe uma associação entre a presença de dor de garganta (GARGANTA) e a presença de febre (FEBRE)?
# Hipótese nula (H0): Não há associação entre a presença de dor de garganta (GARGANTA) e a presença de febre (FEBRE)
# Hipótese alternativa (Ha): A presença de dor de garganta (GARGANTA) está associada à presença de febre (FEBRE).

# Variaveis: 1. Sinais e sintomas - Dor de garganta, 2. Sinais e sintomas - Febre

# Filtrar as primeiras 1000 Amostras
amostra_garganta <- head(INFLUD13$GARGANTA, 1000)
amostra_febre <- head(INFLUD13$FEBRE, 1000)

# Tabela de Contingencia
Tabela <- table(amostra_garganta, amostra_febre)

# Teste qui-quadrado
Teste <- chisq.test(Tabela)
Teste


# ============================================================================================================
# SEXTA HIPÓTESE
# Hipótese: Existe diferença na proporção de pessoas que apresentaram dor de garganta (GARGANTA) entre pessoas de raça/cor branca (CS_RACA) e pessoas de raça/cor preta (CS_RACA)?
# Hipótese nula (H0): Não há diferença na proporção de pessoas que apresentaram dor de garganta entre pessoas de raça/cor branca e pessoas de raça/cor preta.
# Hipótese alternativa (Ha): Existe diferença na proporção de pessoas que apresentaram dor de garganta entre pessoas de raça/cor branca e pessoas de raça/cor preta.

# Variaveis: 1. Raça/Cor, 2. Sinais e sintomas - Dor de garganta

# Filtrar as primeiras 1000 Amostras
amostra_raca <- head(INFLUD13$CS_RACA, 1000)
amostra_garganta <- head(INFLUD13$GARGANTA, 1000)

# Criar Subconjunto de dados apenas para pessoas de raça/cor branca ou preta
amostra_branca_preta <- subset(INFLUD13, CS_RACA %in% c(1, 2))

# Tabela de Contingência
Tabela <- table(amostra_branca_preta$CS_RACA, amostra_branca_preta$GARGANTA)

# Teste qui-quadrado
Teste <- chisq.test(Tabela)
Teste


# ============================================================================================================
# SÉTIMA HIPÓTESE
# Hipótese: Existe uma relação entre a o Sexo da pessoa e a vacinação contra gripe?
# Hipótese nula (H0): Não há relação entre o sexo da pessoa (CS\_SEXO) e a vacinação contra gripe (VACINA)
# Hipótese alternativa (Ha): Existe uma relação entre o sexo da pessoa (CS\_SEXO) e a vacinação contra gripe (VACINA)

# Variaveis: 1. Sexo, 2. Recebeu vacina contra gripe

# Filtrar as primeiras 1000 Amostras
amostra_sexo <- head(INFLUD13$CS_SEXO, 1000)
amostra_vacina <- head(INFLUD13$VACINA, 1000)

# Tabela de Contingencia
Tabela <- table(amostra_sexo, amostra_vacina)

# Teste qui-quadrado
Teste <- chisq.test(Tabela)
Teste


# ============================================================================================================
# OITAVA HIPÓTESE
# Hipótese: Existe uma relação entre a vacinação contra gripe e a presença de tosse?
# Hipótese nula (H0): Não há relação entre a vacinação contra gripe (VACINA) e a presença de tosse (TOSSE)
# Hipótese alternativa (Ha): A vacinação contra gripe (VACINA) está relacionada à presença de tosse (TOSSE)

# Variaveis: 1. Recebeu vacina contra gripe, 2. Sinais e sintomas - Tosse

# Filtrar as primeiras 1000 Amostras
amostra_vacina <- head(INFLUD13$VACINA, 1000)
amostra_tosse <- head(INFLUD13$TOSSE, 1000)

# Tabela de Contingencia
Tabela <- table(amostra_vacina, amostra_tosse)

# Teste qui-quadrado
Teste <- chisq.test(Tabela)
Teste

# ============================================================================================================
# NONA HIPÓTESE
# Hipótese: Existe relação entre PIB per capita (uf_pibpc) e a evolução (EVOLUCAO) dos pacientes?
# Hipótese nula (H0): Não há relação significativa.
# Hipótese alternativa (Ha): Há relação significativa.

#Preparando dataframe do PIB per capita

uf_nome <- c("Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", "Distrito Federal", "Espírito Santo", "Goiás", "Maranhão", "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Pará", "Paraíba", "Paraná", "Pernambuco", "Piauí", "Rio Grande do Norte", "Rio Grande do Sul", "Rio de Janeiro", "Rondônia", "Roraima", "Santa Catarina", "São Paulo", "Sergipe", "Tocantins")
SG_UF_NOT <- c(12, 27, 16, 13, 29, 23, 53, 32, 52, 21, 51, 50, 31, 15, 25, 41, 26, 22, 24, 43, 33, 11, 14, 42, 35, 28, 17)
uf_pibpc <- c(18420.26, 18857.69, 21431.53, 27572.96, 20449.29, 18168.35, 87016.16, 34065.98, 31506.97, 15027.69, 50663.19, 43649.17, 32066.73, 24846.62, 17402.13, 42366.71, 20101.38, 17184.70, 20342.11, 41227.61, 43407.55, 28722.45, 25387.77, 48159.24, 51364.73, 19583.07, 27448.43)

#Categorizando PIB per capita
quartis <- quantile(uf_pibpc, probs = c(0.25, 0.5, 0.75))
limite_baixo <- min(uf_pibpc)
limite_medio <- quartis[1]
limite_alto <- quartis[2]

categoria <- cut(uf_pibpc, breaks = c(limite_baixo, limite_medio, limite_alto, Inf), labels = c(1, 2, 3), include.lowest = TRUE)
uf_pibcat <- c(18420.26, 18857.69, 21431.53, 27572.96, 20449.29, 18168.35, 87016.16, 34065.98, 31506.97, 15027.69, 50663.19, 43649.17, 32066.73, 24846.62, 17402.13, 42366.71, 20101.38, 17184.70, 20342.11, 41227.61, 43407.55, 28722.45, 25387.77, 48159.24, 51364.73, 19583.07, 27448.43)

#Criando dataframe do PIB per capita
uf <- data.frame(SG_UF_NOT, uf_nome, uf_pibpc, categoria)

#Criando amostra apenas com evolução 
amostra_evolucao <- subset(INFLUD13, EVOLUCAO > 0)
#Dando merge entre a amostra contendo dados de evolução e os dados das UF
merge_evolucao <- merge(amostra_obito, uf, by = "SG_UF_NOT")

#Realizando teste qui-quadrado
resultado_teste <- chisq.test(merge_evolucao$EVOLUCAO, merge_evolucao$categoria)




