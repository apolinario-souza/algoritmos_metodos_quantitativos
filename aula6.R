
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)                                
if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)
if(!require(corrplot)) install.packages("corrplot") 
library(corrplot)

# Carregar os dados
dados = read.csv2('Banco_de_Dados.csv',sep = ',', dec = ',') # Carregamento do arquivo csv
View(dados)                                # Visualização dos dados em janela separada
glimpse(dados)                             # Visualização de um resumo dos dados



# Criar um modelo de regressão linear
modelo <- lm(Peso ~ Idade + IMC, data = dados)

# Exibir o resumo do modelo
summary(modelo)

