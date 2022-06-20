#analise preditiva e forecasting

#case analise de credito

#carregando os dados
data=read.table('DATA_3.01_CREDIT.csv',sep=',',header=TRUE) 
newdata = data=read.table('DATA_4.01_CREDIT2.csv',sep=',',header=TRUE) 

#overview dos dados
str(newdata) #100 linhas e 10 colunas
summary(newdata)

#renda mínima é de 10.730 dolares.
#O máximo é de 182.73 em mil dólares americanos e a média é de $48,71 em milhares de dólares americanos. 
#O score tem um valor mínimo de 112, máximo de 982 e um valor médio de 375,4 dolares. 

hist(newdata$Rating)


#em comparação com os dois datasets, crio o modelo que preve o score de credito para cada uma das linhas no dataset newdata
#assim como na analise do score de credito anterior, crio um modelo de regressao linear simples
#o score entra como uma variavel dependente de todas as outras variaveis e utilizando o dataset antigo no modelo
linreg=lm(Rating~.,data=data)

#agora testamos o modelo em dados fora da amostra.
predcreditscore = predict(linreg,newdata=newdata,type="response") 

#calculo a correlaçao entre as variaveis numericas dos dois datasets
cor(linreg$fitted.values,data$Rating) 
#[1] 0.9892838

#ploto os valores ajustados e os atuais
plot(data$Rating,linreg$fitted.values)

#agora verificarei a correlação com o dataset mais simples de 100 linhas. 
cor(predcreditscore,newdata$Rating) 
#[1] 0.9892838

plot(newdata$Rating,predcreditscore)

#em ambos os graficos que compara os dados do modelo e do dataset, é possivel observar que quanto mais baixo o score, menor a "acuracidade" do modelo;
#a ideia é que conforme ha o incremento de dados, com novos clientes solicitando credito, e conforme também temos todas as outras variaveis preenchidas do dataset valores para todas as variáveis explicativas, podemos gerar um score e avaliar se faz ou não sentido conceder a eles um empréstimo, do ponto de vista de negócio/estratégia.
