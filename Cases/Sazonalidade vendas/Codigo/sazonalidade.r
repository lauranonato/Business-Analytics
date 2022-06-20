#analise sazonalidade de vendas de chocolate

#carregando os dados:
data=read.table('DATA_4.04_CHOC.csv',sep=',',header=TRUE)

#overviews dos dados:
str(data) #120 linhas e quatro colunas: time, sales, year e month
#resumo estatistico das vendas
summary(data$sales)
#36 foi o minimo de unidades vendidas ao mes e 1.608 o maximo, a média de unidades é 163 e a mediana 216
#vale ressaltar que o 3 quartil das vendas é de 221 unidades (75% dos dados) e 82 unidades no 1 quartil (25%)

#olhando as vendas ao longo do tempo
plot(data$time,data$sales,main="Chocolate sales over time",xlab="Time (in month)",ylab="Monthly sales",ylim=c(0,max(data$sales*1.2)),type='l')
#é possivel observar sazonalidade nas vendas, além do padrão de picos de vendas aproximadamente ao 12 mes, depois ao mes 24, 36, etc

#agora com um modelo de regressão linear simples 
#nesse modelo as vendas serão a variavel dependente e o tempo a variavel independente do dataset original
regressao=lm(sales~month,data=data)

#verificar o resumo estatistico do modelo
summary(regressao)

#                     Estimate Std.Error t value Pr(>|t|)    
#(Intercept)        156.211     18.306   8.533 9.78e-14 ***
#month02_February   116.377     25.889   4.495 1.75e-05 ***
#month03_March       -6.559     25.889  -0.253 0.800479    
#month04_April        4.846     25.889   0.187 0.851854    
#month05_May         24.245     25.889   0.937 0.351100    
#month06_June       -78.034     25.889  -3.014 0.003212 ** 
#month07_July       -80.262     25.889  -3.100 0.002466 ** 
#month08_August     -94.941     25.889  -3.667 0.000382 ***
#month09_September  -81.921     25.889  -3.164 0.002020 ** 
#month10_October     30.185     25.889   1.166 0.246208    
#month11_November   230.894     25.889   8.919 1.33e-14 ***
#month12_December   661.034     25.889  25.533  < 2e-16 ***

#com esse resultado, vemos que os meses com maior significancia estatistica para as vendas são: feveireiro, agosto e novembro. 
#com menor significancia em seguida temos os meses: junho, julho e setembro
#se olharmos o "coeficiente" e "t-value", os meses de novembro, dezembro e fev tem o maior efeito positivo nas vendas de chocolate.
#mas por que esses meses? sera que faz sentido pensando no negocio?
#considerando que é um dataset europeu,  muitas pessoas no hemisfério norte compram chocolate para as festas de fim de ano.
#mas se fosse no brasil, por conta da pascoa, esse pico de vendas poderia estar sendo indicado entre março e abril...

#agora com um boxplot, é possivel ver a distribuicao das vendas por mes, independente do ano

plot(data$month,data$sales,main="Chocolate sales by month",xlab="Month",ylab="Monthly sales",ylim=c(0,max(data$sales*1.2)))

#e possivel observar que dezembro tem alta variabilidade de vendas, enquanto julho tem um boxplot muito enxuto que representa a similaridade do comportamento das vendas em todos os anos registrados no dataset

#mas como as vendas se comportavam no passado?
#pra isso, irei plotar o mesmo de regressao linear simples com o historico das vendas
plot(data$time,data$sales,main="Chocolate sales over time",xlab="Time (in month)",ylab="Monthly sales",ylim=c(0,max(data$sales)*1.2),type='l')

lines(data$time,regressao$fitted.values,type='l',col='red',lty=2)

legend("topleft",c("Actual sales","Sales by the model"),lty=c(1,2),col=c('black','blue'))
#o topleft indica que quero que a legenda esteja no canto direito superior do grafico
#o c() para inserir o texto na legenda

#Resumo:
#ao verificar quais meses tem um efeito maior sobre as vendas, reduzimos uma incerteza de negócio de forma que pode ser recomendada ações mais eficientes em determinados períodos do ano, 
#maximizar as vendas ao mesmo tempo reduzir o custo de gerenciamento dos estoques ao longo do ano, por exemplo.
