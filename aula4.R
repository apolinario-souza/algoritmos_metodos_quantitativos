######################### ANOVA one_way (de uma via) #########################


# Passo 0: Carregar os pacotes que serão usados

library(dplyr)                                
library(RVAideMemoire)                                        
library(car)                                
library(psych)                                
library(rstatix)  
library(lsr)



# Passo 1: selecionar o diretório de trabalho

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de código abaixo:

setwd("/media/tercio/Arquivos/UFRGS/Disciplinas/Metodos_quantitativos/aulas/R/04")

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados = read.csv2('Banco_de_Dados.csv',sep = ',', dec = ',') # Carregamento do arquivo csv
View(dados)                                # Visualização dos dados em janela separada
glimpse(dados)                             # Visualização de um resumo dos dados


# Passo 3: Verificação da normalidade dos dados
## Shapiro por grupo (pacote RVAideMemoire)

byf.shapiro(Peso ~ Classe_social, dados)


# Passo 4: Verificação da homogeneidade de variâncias
## Teste de Levene (pacote car)

leveneTest(Peso ~ Classe_social, dados, center=mean)


# Observação:
# Por default, o teste realizado pelo pacote car tem como base a mediana (median)
# O teste baseado na mediana é mais robusto
# Mudamos para ser baseado na média (comparável ao SPSS)

# Passo 5: Realização da ANOVA
minha_anova = aov(Peso ~ Classe_social, dados)
summary(minha_anova)


etaSquared(x = minha_anova) #calculando o tamanho do efeito

# Passo 6: Análise post-hoc

TukeyHSD(minha_anova)

######################### ANOVA one_way (de uma via) com medidas repetidas #########################


#Passo 1: Carregar os pacotes que serão usados

library(dplyr)                                
library(ez)
library(reshape) 
library(rstatix)

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados = read.csv2('Banco_de_Dados2.csv', sep = ',', dec = ',') # Carregamento do arquivo csv
View(dados)                                # Visualização dos dados em janela separada
glimpse(dados)                             # Visualização de um resumo dos dados



# Passo 3: Alterar o formato do banco de dados de "wide" para "long" (pacote: reshape)

# Reestruturando o banco de dados

dadosl = melt(dados,
               id = "Suj",
               measured = c("m1", "m2", "m3"))

View(dadosl)

# Renomeando as colunas do novo banco
colnames(dadosl) = c("Suj", "Repeticao", "var_dep")

# Ordenando as colunas pelo sujeito experimental
dadosl = sort_df(dadosl, vars = "Suj")


glimpse(dadosl)

# Transformando a variável Suj em fator
dadosl$Suj = factor(dadosl$Suj)



# Passo 4: Checar os pressupostos de normalidade(pacote: rstatix)


# Verificando a normalidade por grupo
dadosl %>% group_by(Repeticao) %>% 
  shapiro_test(var_dep)


# Passo 5: Construção do modelo da ANOVA com medidas repetidas (pacote: ez)

minha_ANOVA = ezANOVA(data = dadosl,
                     dv = var_dep,
                     wid = Suj,
                     within = Repeticao,
                     detailed = TRUE,
                     type = 3)

# dv = variável dependente
# wid = variável de identificação do sujeito
# within = variável independente de medidas repetidas
# type = tipo da soma dos quadrados (default é o tipo II, tipo III é o padrão no SPSS)


# Passo 5: Analisando os resultados do modelo
minha_ANOVA

# Obs.: Libera o teste de esfericidade de Mauchly, e as correções
#       de Greenhouse-Geisser e Huynh-Feldt


# Passo 6: Testes de post-hoc
pairwise.t.test(dadosl$var_dep, dadosl$Repeticao, paired = TRUE,
                p.adjust.method = "bonferroni")

######################### ANOVA Two_way (de duas vias) #########################

# Passo 1: Carregar os pacotes que serão usados

library(dplyr)                                
library(car)                                
library(rstatix)                                
library(DescTools)
library(emmeans)
library(ggplot2)
library(RVAideMemoire) 

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados = read.csv2('Banco_de_Dados.csv',sep = ',', dec = ',') # Carregamento do arquivo csv
View(dados)                                # Visualização dos dados em janela separada
glimpse(dados)                             # Visualização de um resumo dos dados



summary(dados$Classe_social)

# Transformando a Coluna em fator
dados$Classe_social = factor(dados$Classe_social)
dados$Sexo = factor(dados$Sexo)


# Passo 3: Verificação dos pressupostos nos dados brutos

## Verificação da normalidade - Shapiro por grupo:
byf.shapiro(Peso ~ Classe_social, dados)
byf.shapiro(Peso ~ Sexo, dados)


## Verificação da homogeneidade de variâncias - teste de Levene (pacote car)
leveneTest(Peso ~ Classe_social, dados, center = mean)
leveneTest(Peso ~ Sexo, dados, center = mean)



# Passo 5: Realização da ANOVA

## Mudança no contraste para equivaler ao SPSS:
options(contrasts = c("contr.sum", "contr.poly"))

## Criação do modelo:
minha_ANOVA = aov(Peso ~ Classe_social*Sexo, dados)
summary(minha_ANOVA)


TukeyHSD(minha_ANOVA)


