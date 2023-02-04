#**********************************************************
#ANALISE DE CLUSTER - VAREJO
#**********************************************************

#**********************************************************
#Atencao: Alterar Diretorio
setwd("...")

#Retirar notacao cientifica no R
options(scipen = 999)

#**********************************************************
#Instalacao de pacotes (base realizar 1 vez)
install.packages("cluster")
install.packages("factoextra")
install.packages("gridExtra")

#**********************************************************
#Leitura da Base 
library(readxl)
varejo <- read_excel("varejo1.xlsx", sheet = 'Base de Dados')
nrow(varejo)
ncol(varejo)
#**********************************************************

#**********************************************************
# Faca uma analise exploratoria da base de dados 
# (obtenha as medidas de posicao e dispersao). 
summary(varejo[,-1]) #Min, Q1, Q2, Q3 e Max
apply(varejo[,-1], 2, sd) #Desvio Padrao

#Considerando o histograma das variaveis, as distribuicoes sao simetricas?
par(mfrow=c(1,3))
hist(varejo$qtde_compras, main="qtde_compras", col = "aquamarine")  
hist(varejo$valor_produtos, main="valor_produtos", col = "coral") 
hist(varejo$nota_revisao, main="nota_revisao", col = "orchid") 

#Existe outlier nas variaveis?
boxplot(varejo$qtde_compras, col = "aquamarine", main="qtde_compras")  
boxplot(varejo$valor_produtos, col = "coral", main="valor_produtos")
boxplot(varejo$nota_revisao, col = "orchid", main="nota_revisao")

#**********************************************************
#Padronize as variaveis.
varejo_z<-scale(varejo[,-1])
head(varejo_z)
varejo_z

#Calcule a matriz de distancias euclidianas 
distancia <- dist(varejo_z, method="euclidean") #Calculo das distancias euclidianas
distancia

#Faca a analise de agrupamento com as variaveis padronizadas
#usando os 2 metodos apresentados, escolha um dos metodos 
#e justifique a quantidade de grupos apos a analise do Dendrograma.

#Matriz de graficos de dimensao 1 linhas x 2 colunas
par(mfrow=c(1,2))

clust_single <- hclust(distancia, method="single") 
plot(clust_single, main="Metodo Single", hang=-1) #hang=-1 para deixar todos iniciando do zero
rect.hclust(clust_single, k=5, border=1:5)

clust_complete <- hclust(distancia, method="complete")
plot(clust_complete, main="Metodo Complete", hang=-1)
rect.hclust(clust_complete, k=5, border=1:5) 

#Analise as caracteristicas de cada grupo.
varejo$cluster <- as.factor(cutree(clust_complete, k=5))

#Tamanho dos Clusters
table(varejo$cluster)

#Faz BoxPlot para cada variavel e compara por cluster
#Distribuicao das variaveis por cluster
par(mfrow=c(1,3)) #coloca os graficos lado a lado
boxplot(varejo$qtde_compras ~ varejo$cluster, col="aquamarine", main="qtde_compras")
boxplot(varejo$valor_produtos ~ varejo$cluster, col="coral", main="valor_produtos")
boxplot(varejo$nota_revisao ~ varejo$cluster, col="orchid", main="nota_revisao")