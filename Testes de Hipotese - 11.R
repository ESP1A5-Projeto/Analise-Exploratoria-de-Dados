# TESTES DE HIPÓTESE

# ============================================================================================================
# DÉCIMA PRIMEIRA HIPÓTESE
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




