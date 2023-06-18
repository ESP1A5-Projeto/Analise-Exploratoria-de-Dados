dados <- read.csv("INFLUD18.csv")

# sintomas mais comuns
# soma das frequências dos sintomas
tosse<-sum(INFLUD18$TOSSE==1, na.rm = TRUE)
febre<-sum(INFLUD18$FEBRE==1, na.rm = TRUE)
dispneia<-sum(INFLUD18$DISPNEIA==1, na.rm = TRUE)
garganta<-sum(INFLUD18$GARGANTA==1, na.rm = TRUE)
artralgia<-sum(INFLUD18$ARTRALGIA==1, na.rm = TRUE)
mialgia<-sum(INFLUD18$MIALGIA==1, na.rm = TRUE)
conjutiv<-sum(INFLUD18$CONJUTIV==1, na.rm = TRUE)

# data frame com os sintomas e suas frequências
sintomas <- c("Tosse", "Febre", "Dispneia", "Garganta", "Artralgia", "Mialgia", "Conjuntiv")
frequencias <- c(tosse, febre, dispneia, garganta, artralgia, mialgia, conjutiv)
sintomas_df <- data.frame(Sintoma = sintomas, Frequencia = frequencias)

# ordenar o data frame pela frequência decrescente
sintomas_df <- sintomas_df[order(-sintomas_df$Frequencia), ]

# gráfico de barras
ggplot(sintomas_df, aes(x = Sintoma, y = Frequencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Sintoma", y = "Frequência", title = "Sintomas mais comuns") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# verificar se a síndrome respiratória aguda é mais comum em pessoas vacinadas ou não vacinadas
# ocorrência de síndrome respiratória aguda em pessoas vacinadas e não vacinadas
recorrencia_vacinados <- sum(INFLUD18$VACINA==1, na.rm = TRUE)
recorrencia_nao_vacinados <- sum(INFLUD18$VACINA==2, na.rm = TRUE)

# data frame com os dados
resumo_vacina <- data.frame(Vacina = c("Vacinados", "Não Vacinados"),
                            Recorrência = c(recorrencia_vacinados, recorrencia_nao_vacinados))

# gráfico de barras
ggplot(resumo_vacina, aes(x = Vacina, y = Recorrência, fill = Vacina)) +
  geom_bar(stat = "identity") +
  labs(x = "Vacinação", y = "Recorrência", title = "Recorrência da Síndrome Respiratória Aguda por Vacinação") +
  theme_minimal()
