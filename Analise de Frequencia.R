# ANÁLISE DE FREQUÊNCIA

# Carregar pacote necessário
library(ggplot2)

# ============================================================================================================
# PRIMEIRA ANÁLISE
# Data dos primeiros sintomas / Diagnóstico 
table(dados$DT_SIN_PRI)

# Gráfico de Barras
ggplot(dados, aes(x = DT_SIN_PRI)) +
  geom_bar() +
  xlab("Data dos primeiros sintomas / Diagnóstico") +
  ylab("Frequência")

# ============================================================================================================
# SEGUNDA ANÁLISE
# Gestante
table(dados$CS_GESTANT)

# Gráfico de Barras
ggplot(dados, aes(x = CS_GESTANT)) +
  geom_bar() +
  xlab("Gestante") +
  ylab("Frequência")

# ============================================================================================================
# TERCEIRA ANÁLISE
# Sexo
table(dados$CS_SEXO)

# Gráfico de Barras
ggplot(dados, aes(x = CS_SEXO)) +
  geom_bar() +
  xlab("Sexo") +
  ylab("Frequência")

# ============================================================================================================
# QUARTA ANÁLISE
# Raça/Cor 
table(dados$CS_RACA)

# Gráfico de Barras
ggplot(dados, aes(x = CS_RACA)) +
  geom_bar() +
  xlab("Raça/Cor") +
  ylab("Frequência")

# ============================================================================================================
# QUINTA ANÁLISE
# Recebeu vacina contra gripe 
table(dados$VACINA)

# Gráfico de Barras
ggplot(dados, aes(x = VACINA)) +
  geom_bar() +
  xlab("Recebeu vacina contra gripe") +
  ylab("Frequência")

# ============================================================================================================
# SEXTA ANÁLISE
# Sinais e sintomas - febre 
table(dados$FEBRE)

# Gráfico de Barras
ggplot(dados, aes(x = FEBRE)) +
  geom_bar() +
  xlab("Sinais e sintomas - febre") +
  ylab("Frequência")

# ============================================================================================================
# SÉTIMA ANÁLISE
# Sinais e sintomas - tosse 
table(dados$TOSSE)

# Gráfico de Barras
ggplot(dados, aes(x = TOSSE)) +
  geom_bar() +
  xlab("Sinais e sintomas - tosse") +
  ylab("Frequência")

# ============================================================================================================
# OITAVA ANÁLISE
# Sinais e sintomas - dor de garganta
table(dados$GARGANTA)

# Gráfico de Barras
ggplot(dados, aes(x = GARGANTA)) +
  geom_bar() +
  xlab("Sinais e sintomas - dor de garganta") +
  ylab("Frequência")
