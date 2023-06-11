# Análise de Tendência Central

# Bibliotecas
library(ggplot2)

# Data dos primeiros sintomas / Diagnóstico 
mean(INFLUD13$NU_IDADE_N)

# Amostra utilizando idades maiores que 4000 
amostra <- subset(INFLUD13, INFLUD13$NU_IDADE_N > 4000)

# Cálculo da média
mean(amostra$NU_IDADE_N)

#Histograma
hist(amostra$NU_IDADE_N, breaks = 10, xlab = "Idade", ylab = "Frequência", main = "Distribuição de Idades")

#Boxplot
boxplot(amostra$NU_IDADE_N, main = "Distribuição de Idades", ylab = "Idade")
