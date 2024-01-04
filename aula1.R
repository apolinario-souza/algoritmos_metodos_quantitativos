
########################### instalando pacotes ########################

# Opção 1
# Instalando através de packages no canto inferior direito (ex., car)

# Opção 2
install.packages("dplyr")


########################### carregando pacotes ########################

library(dplyr) #ou require (dplyr)

########################### carregando banco de dados ########################

# 1 selecionar o diretório de trabalho
# Opção 1: manualmente - Session > set working directory > choose directory
# Opção 2: via código - setwd ("diretorio")
setwd("/media/tercio/Arquivos/UFRGS/Disciplinas/Metodos_quantitativos/aulas/R")

# 2 carregar o bando de dados
dados = read.csv("Banco_de_Dados.csv") # Pode ser dados <- read.csv("Banco_de_Dados.csv")
View(dados)
glimpse(dados)


