#business analytics

#analise exploratória de skus

#carregando os dados:
data=read.table('DATA_2.01_SKU.csv', header = T,sep=',')

#conhecendo os tipos de dados no dataset: 
#quantitativas sendo a média de vendas diarias e o coeficiente de variação 
str(data) 

#resumo estatístico
summary(data)
#o coeficiente de variação do dataset está entre 0.050 e 0.960
# e com o boxplot a distribuição do coeficiente é alta, com a mediana de 0.4,
#75% dos dados tem o coeficiente de 0.5

#ja a media de vendas diária está entre 1 e 14, sendo que há alta variação dos dados
#a mediana em 5 representa melhor e os 25% dos dados apresentam média de 2 vendas diárias
#o bloxplot ajuda na interpretação visual da distribuição dos dados antes de qualquer analise

boxplot(data$CV)
boxplot(data$ADS)

#o grafico de dispersão ajuda a identificar agrupamentos visualmente
#como sao duas variaveis numericas (continua) não foi necessário nenhum tratamento dos dados para plotar o grafico:
plot(data$CV, data$ADS, main = "SKU Example", ylab="Average Daily Sales", xlab= "Coefficient of Variation")

#há um agrupamento dos dados com itens de alta média de vendas diárias e baixa taxa de variação
#ja o um segundo agrupamento com itens baixa média de vendas e alta taxa de variação
#e um ultimo mais disperso, de alta media de vendas bem como alta taxa de variação

#agrupando os itens com media diaria de vendas acima de itens e baixo coeficiente de variação
abline(v=0.2, col = "red")

#grupo dos itens com 2 vendas diarias em media mas alto coeficiente de variação
abline(h=4, col="red") 

#restaram no grafico de dispersão os itens com alta venda diaria em media mas tambem com coeficiente de variação das vendas muito disperso

#renomeio os agrupamentos por nomes de acordo com a estratégia de negócio:
text(0.15,9.7, "Horses", col = "red")
text(0.65,9, "Wild Bulls", col = "red")
text(0.8,2, "Crickets", col = "red")

#os Horses são


#agora utilizarei o agrupamento hierarquico, uma função que programa agrupamentos automaticos
#de acordo com a distancia entre os dados e ajuda a verificar se tenho um resultado parecido com a interpretação visual

testdata=data
testdata = scale(testdata)

d = dist(testdata, method = "euclidean")
hcward = hclust(d, method="ward.D")

#reduzo a hierarquical clustering em 3 clusters:
data$groups<-cutree(hcward,k=3) 

#ploto os tres clusters agrupados automaticamente
install.packages("lattice")
library("lattice")
xyplot(ADS~ CV,main = "After Clustering", type="p",group=groups,data=data, #diferencio os clusters
       auto.key=list(title="Group", space = "left", cex=1.0, just = 0.95), 
       par.settings = list(superpose.line=list(pch = 0:18, cex=1)), 
       col=c('blue','green','red'))
