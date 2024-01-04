
######################### Correlacao #########################


# Carregar os pacotes 
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)                                
if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)
if(!require(corrplot)) install.packages("corrplot") 
library(corrplot)

# Carregar o banco de dados

# Importante: selecionar o diretorio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
dados = read.csv2('Banco_de_Dados.csv',sep = ',', dec = ',') # Carregamento do arquivo csv
View(dados)                                # Visualização dos dados em janela separada
glimpse(dados)                             # Visualização de um resumo dos dados



# Verificar os pressupostos para a correlacao de Pearson

## Normalidade (Shapiro-Wilk):
shapiro.test(dados$Idade)
shapiro.test(dados$IMC)



# Passo 4: Realizar a correlacao

## Pearson (coeficiente = r):
cor.test(dados$Idade, dados$IMC, method = "pearson")


## Spearman (coeficiente = rh):
cor.test(dados$Idade, dados$IMC, method = "spearman")


## Tau de Kendall (coeficiente = tau):
cor.test(dados$Idade, dados$IMC, method = "kendall")


# Grafico
ggplot(dados, aes(x = Idade, y = IMC)) +
  labs(x = "Idade", y = "IMC") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() + 
  ggtitle("Correlação entre Idade e IMC")

