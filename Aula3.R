
# Passo 0: Carregar os pacotes que serão usados

library(dplyr)                                
library(psych)                                
library(RVAideMemoire) 
library(car)


# Passo 0.1: selecionar o diretório de trabalho

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de código abaixo:

setwd("/media/tercio/Arquivos/UFRGS/Disciplinas/Metodos_quantitativos/aulas/R/03")



################################# Teste t para uma amostra ################

# Passo 1: Carregar o banco de dados

dados = read.csv('Banco_de_Dados.csv', sep = ',', dec = ',') # Carregamento do arquivo csv
View(dados)               # Visualização dos dados em janela separada
glimpse(dados)            # Visualização de um resumo dos dados


# Passo 2: Verificação da normalidade dos dados


shapiro.test(dados$Altura)



# Passo 3: Realização do teste t para uma amostra

t.test(dados$Altura, mu = 1.67)


# Observação:
# O teste bicaudal é o default; caso deseje unicaudal, necessário incluir:
# alternative = "greater" ou alternative = "less"
# Exemplo: t.test(dados$Altura, mu = 167, alternative = "greater")
# Nesse caso, o teste verificará se é a média amostral é maior que a média testada

# Passo 3.1  Wilcoxon Signed Rank Test

wilcox.test(dados$Altura, mu = 1.67, correct = FALSE,  alternative = "two.sided")

# Passo 4 (opcional): Visualização da distribuição dos dados

boxplot(dados$Altura, ylab = "Altura (cm)")

################################# Teste t pareado ################
# Passo 1: Carregar o banco de dados

dados = read.csv('Banco_de_Dados2.csv', sep = ',', dec = ',')
View(dados)                                              # Visualização dos dados em janela separada
glimpse(dados)

# Passo 2: Verificação da normalidade dos dados

dados$Diferenca = dados$Pre_MD - dados$Pos_MD

shapiro.test(dados$Diferenca)


# Passo 3: Realização do teste t pareado

t.test(dados$Pre_MD, dados$Pos_MD, paired = TRUE)

# Passo 3.1 Wilcoxon Signed Rank Test

wilcox.test(dados$Pre_MD, dados$Pos_MD, correct = FALSE,  alternative = "two.sided")

######################### Teste t para Amostras Independentes #########################

# Passo 1: Carregar o banco de dados
dados = read.csv('Banco_de_Dados.csv', sep = ',', dec = ',') # Carregamento do arquivo csv
View(dados)               # Visualização dos dados em janela separada
glimpse(dados)            # Visualização de um resumo dos dados


# Passo 2: Verificação da normalidade dos dados
## Shapiro por grupo (pacote RVAideMemoire)

byf.shapiro(IMC ~ Sexo, dados)
byf.shapiro(Altura ~ Sexo, dados)
byf.shapiro(Peso ~ Sexo, dados)

# Passo 3: Verificação da homogeneidade de variâncias
## Teste de Levene (pacote car)

leveneTest(IMC ~ Sexo, dados, center=mean)
leveneTest(Altura ~ Sexo, dados, center=mean)
leveneTest(Peso ~ Sexo, dados, center=mean)

# Observação:
# Por default, o teste realizado pelo pacote car tem como base a mediana (median)
# O teste baseado na mediana é mais robusto
# Mudamos para ser baseado na média (comparável ao SPSS)


# Passo 4: Realização do teste t para amostras independentes

t.test(IMC ~ Sexo, dados, var.equal=TRUE)
t.test(Altura ~ Sexo, dados, var.equal=TRUE)
t.test(Peso ~ Sexo, dados, var.equal=TRUE)

# Passo 4.1 : Realização Teste de Mann-Whitney 
wilcox.test(IMC ~ Sexo, dados)
wilcox.test(Altura ~ Sexo, dados)
wilcox.test(Peso ~ Sexo, dados)



