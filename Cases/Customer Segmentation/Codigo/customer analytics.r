#analise exploratoria customer segmentation: setor de telecomunicações

#em um grande conjunto de dados como esse, vale a pena se perguntar como os grupos realmente se diferem? Em um vies de negocio, pode ser que nesse grupo de clientes apesar de serem ligeiramente diferentes, podem ser tratados de forma semelhante, dado que queremos reduzir o esforço necessário para agir de forma mais eficaz na clusterização dos clientes.


#esse comando limpa o environment no Rstudio
rm(list=ls(all=TRUE))

#carregando os dados:
data=read.table('DATA_2.03_Telco.csv', header = T,sep=',')

#nesse dataset temos
#o tempo médio de chamada nacional mensal
#o tempo gasto em chamadas internacionais, o número de mensagens de texto enviadas
#os dados utilizados e a idade do cliente. 

#conhecendo os tipos de dados no dataset 
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

#normalizo as variáveis pois elas tem unidades de medida diferentes.
testdata=data
testdata = scale(testdata)
#qual é a distancia entre os dados?
d = dist(testdata, method = "euclidean")

#sabendo dessa dispersão, crio um agrupamento hierarquico (hiearchical clustering) no dataset normalizado.
hcward = hclust(d, method="ward.D")
#reduzo o agrupamento para 8 clusters

data$groups=cutree(hcward,k=8)

#com esse tratamento, consigo calcular o valor médio para cada uma dessas variáveis por cluster
#algumas observações com a tabela criada com os clusters:
#o primeiro cluster não tem um alto nível de uso, mas é mais antigo do que os outros.
#o segundo cluster faz muitas chamadas nacionais e internacionais,  mas menos mensagens de texto. Identificarei esse cluster pelo nome “Pro”.
#entre outros nomes para os clusters: jovens adultos, 30 anos, 40 anos, adolescentes, baixo uso e heavy users 

#verifico as dimensões dos clusters
#qual a média em cada cluster?
aggdata= aggregate(.~ groups, data=data, FUN=mean)
#qual o total de linhas/registros por cluster
proptemp=aggregate(Calls~ groups, data=data, FUN=length)
#qual é o percentual de cada cluster?
aggdata$proportion=(proptemp$Calls)/sum(proptemp$Calls)
#ordeno os clusters
aggdata=aggdata[order(aggdata$proportion,decreasing=T),]

#Voltando ao inicio da questao dos clusters é a semelhança entre eles e a recomendação com o olhar do negócios.
#É possível perceber que os jovens adultos e os adolescentes têm usos semelhantes, com menos chamadas, mas muitas mensagens de texto e dados.
#Assim como, os trinta anos e os quarenta anos também são muito semelhantes entre si. 
#vale criar clusters de grupos similares a partir dessa analise.

#plotando a proporção de cada variavel nos 8 clusters
palette(rainbow(12, s = 0.6, v = 0.75))
stars(aggdata[,2:(ncol(data))], len = 0.6, key.loc = c(11, 6),xlim=c(2,12),main = "Segments", draw.segments = TRUE,nrow = 2, cex = .75,labels=aggdata$groups)

