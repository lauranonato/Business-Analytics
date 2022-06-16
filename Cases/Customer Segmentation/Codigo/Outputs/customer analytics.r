#customer segmentation: setor de telecomunicações
#esse comando limpa o environment no Rstudio
rm(list=ls(all=TRUE))

#carregando os dados:
data=read.table('DATA_2.03_Telco.csv', header = T,sep=',')

#conhecendo os tipos de dados no dataset: 5 
#quantitativas: discretas (tempo em chamada, tempo em chamada internacional, tempo em mensagens de texto, data)
#continua: idade
str(data) 

#resumo estatístico
summary(data)
#no total as chamadas tem baixa dispersão (0,8 a 6), com a media 2 e 25% dos dados concentrados em ate 1,49
#ja as chamadas internacionais tem alta dispersão nos dados, a media e mediana estão distantes (0,22 e 0,40) e 25% dos dados tem ate 0,1
#as chamadas de texto tem uma distribuição equilibrada de (17 min a 598) e a média é de 225
#a idade desses clientes estão entre 12 a 72 anos, sendo a média de 35 e mediana de 37, bem próximas

boxplot(data$Age)

#normalizo os dados
testdata=data
testdata = scale(testdata)
#qual é a distancia entre os dados?
d = dist(testdata, method = "euclidean")
#sabendo dessa dispersão, crio um agrupamento hierarquico (hiearchical clustering)
hcward = hclust(d, method="ward.D")
#reduzo o agrupamento para 8 clusters

data$groups=cutree(hcward,k=8)

#verifico as dimensões dos clusters
#qual a média em cada cluster?
aggdata= aggregate(.~ groups, data=data, FUN=mean)
#qual o total de linhas/registros por cluster
proptemp=aggregate(Calls~ groups, data=data, FUN=length)
#qual é o percentual de cada cluster?
aggdata$proportion=(proptemp$Calls)/sum(proptemp$Calls)
#ordeno os clusters
aggdata=aggdata[order(aggdata$proportion,decreasing=T),]


#a proporção de cada variavel nos 8 clusters
palette(rainbow(12, s = 0.6, v = 0.75))
stars(aggdata[,2:(ncol(data))], len = 0.6, key.loc = c(11, 6),xlim=c(2,12),main = "Segments", draw.segments = TRUE,nrow = 2, cex = .75,labels=aggdata$groups)

