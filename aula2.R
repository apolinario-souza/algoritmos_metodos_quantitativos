

# Carregando o banco de dados
setwd("/media/tercio/Arquivos/UFRGS/Disciplinas/Metodos_quantitativos/aulas/R/02/")

dados = read.csv("Banco_de_Dados.csv", sep = ',', dec = ',')


# Calcular Mínimo, Maximo e Amplitude
minimo_peso = min(dados$Peso) # mínimo
maximo_peso = max (dados$Peso) # máximo
amplitude_peso = maximo_peso - minimo_peso # amplitude


# Medidas de tendência central
media_idade = mean(dados$Idade) # média
median(dados$Peso) # mediana
table(dados$Grau_de_Instruçao) #Ver moda


# Medidas de variabilidade
var(dados$Peso) # variancia
sd(dados$Peso) # desvio padrão
desvio_idade = sd(dados$Idade)
cv_idade = desvio_idade/media_idade # Coeficiente de variação


# 1º e 3º quartil, intervalo interquartil
quantile(dados$Peso,0.25) # 1º quartil
quantile(dados$Peso,0.75) # 3º quartil
IQR(dados$Peso) # intervalo interquartil


# Resumo
summary(dados) #todos


# Mais detalhado: frequencia
install.packages("pastecs")
library(pastecs)
stat.desc(dados, norm = TRUE)


# Correlação entre variáveis
cor(dados$Altura, dados$IMC)

# Tabela de contingência
table(dados$Classe_social)
prop.table(table(dados$Classe_social))

table(dados$Sexo, dados$Grau_de_Instruçao) # valor absoluto
prop.table(table(dados$Sexo, dados$Grau_de_Instruçao)) # valor relativo

# plot gráficos
barplot(table(dados$Sexo))

install.packages("ggplot2")
library(ggplot2)
ggplot(dados) +  aes(x = Sexo) +   geom_bar()

# histograma
hist(dados$IMC)
ggplot(dados) +  aes(x = IMC) +   geom_histogram()

# Boxplot: permite observar os valores dos mediana, quartis, extremos e outlier 
boxplot(dados$IMC)
boxplot(dados$Altura ~ dados$Sexo) #plotar em função dos sexos

# Dotplot: semelhante ao boxplot, porém os pontos são separados
install.packages("lattice")
library(lattice)
dotplot(dados$Altura ~ dados$Sexo) #plotar em função dos sexos

# Scatterplot: perminte observar a relação entre duas variáveis
plot(dados$Altura, dados$IMC)
ggplot(dados) + aes(x = Altura, y = IMC, colour = Sexo) + geom_point() + scale_color_hue() #separar por sexo

# QQ-plot: checar distribuição normal - relação entre os quartis e a densidade da distribuição
qqnorm(dados$Altura)
qqline(dados$Altura)

# Densidade
plot(density(dados$Altura))




