#entendendo causa e consequencia
#score de crédito: o que impulsiona o score de crédito?

data=read.table('DATA_3.01_CREDIT.csv',sep=',',header=TRUE)

str(data)
#dataset tem 10 colunas e 3000 linhas
#dicionario dos dados:
#5 quantitativas (renda, score de credito, numero de cartoes ( número de cartões de crédito que o usuário possui), 
#cidade, balanço (o saldo que corresponde à dívida média do cartão de crédito para o indivíduo))
#e 4 qualitativas (educação, genero, status estudante, status civil, raça/cor)

#resumo estatistico do dataset
summary(data)
#o valor mínimo de renda é de $10.350. 
#O máximo é de US $186,63 mil, 
#e o valor médio é de US $44,05 mil. 
#O score de crédito do usuário tem um valor mínimo de 93, um valor máximo de 949 e um valor médio de 348,1


#das variáveis do dataset o rating depende de outras variaveis


#Por isso é válido escolher o Rating para ver a distribuição do histograma: 
hist(data$Rating)

#A distribuição se concentra na direita do gráfico. A maioria dos scores de credito estao entre 100 e aprox 400.

#mas o que influencia o score de credito? Para isso, a correlação ajuda a entender, sendo uma variavel numerica

#qual a correlação entre todas as variaveis numericas? colunas de 1 a 5
cor(data[,c(1:5,10)])

#               Income      Rating      Cards         Age   Education     Balance
#Income     1.00000000  0.77116741 0.02887452  0.12320067 -0.07095917  0.43232667
#Rating     0.77116741  1.00000000 0.09585441  0.04237663 -0.09543257  0.85982866
#Cards      0.02887452  0.09585441 1.00000000  0.05465525  0.01517640  0.12384602
#Age        0.12320067  0.04237663 0.05465525  1.00000000 -0.04617816 -0.05242587
#Education -0.07095917 -0.09543257 0.01517640 -0.04617816  1.00000000 -0.07316655
#Balance    0.43232667  0.85982866 0.12384602 -0.05242587 -0.07316655  1.00000000

#boa correlação entre renda e score de credito; estranho haver correlação negativa entre renda e educação

#estimo uma regressão linear para o score de credito
linreg=lm(Rating~.,data=data)

#para cada linha dotaset, o modelo de regressao linear estima o score de credito baseado nas outras variaveis e são chamadas de valores ajustados

#correlacao entre os valores do dataset e os valores do modelo
cor(linreg$fitted.values,data$Rating)

#[1] 0.9867324


plot(data$Rating,linreg$fitted.values)
#com o grafico fica mais facil visualizar que nos valores baixos de score de credito há maior dispersão

#com um bom coeficiente de correlação, vemos a correlação entre as variaveis:
summary(linreg)

#Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        140.881377   9.666167  14.575   <2e-16 ***
#Income               2.094703   0.048118  43.533   <2e-16 ***
#Cards               -0.762853   1.079874  -0.706   0.4805    
#Age                  0.144603   0.085872   1.684   0.0933 .  
#Education            0.179388   0.473743   0.379   0.7052    
#GenderFemale         1.770375   2.917842   0.607   0.5445    
#StudentYes         -98.804778   4.959789 -19.921   <2e-16 ***
#MarriedYes           3.176873   3.005535   1.057   0.2914    
#EthnicityAsian      -4.428289   4.006859  -1.105   0.2700    
#EthnicityCaucasian  -1.250612   3.533864  -0.354   0.7237    
#Balance              0.231363   0.003661  63.189   <2e-16 ***


#qual a relação entre score de credito e a divida média do usuario?
plot(data$Balance,data$Rating) 
#visualmente fica mais facil de verificar a correlação positiva entre as duas variaveis
#e quanto maior o score de credito maior é a divida media

#qual a relação entre score de credito e renda?
plot(data$Income,data$Rating)

