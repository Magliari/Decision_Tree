base<-read.csv("credit_data.csv")
base$clientid<- NULL
summary(base)

#Excluir os valores negativos
base<-abs(base)
#Excluir os valores faltantes
base$age=ifelse(is.na(base$age),mean(base$age,na.rm = TRUE),base$age)
#Encode da Classe
base$default<-factor(base$default,levels = c(0,1))
#Dividir a base de dados em treinamento e teste
library('caTools')
set.seed(1)
divisao<-sample.split(base$default,SplitRatio = 0.75)
base_treinamento<-subset(base, divisao==TRUE)
base_teste<-subset(base, divisao==FALSE)
#Árvore de Decisão
library(rpart)
classificador<-rpart(formula = default ~ ., data = base_treinamento)
library(rpart.plot)
rpart.plot(classificador)
previsoes<-predict(classificador, newdata = base_teste[-4],type = 'class')
matriz_confusao<-table(base_teste[,4],previsoes)
library(caret)
# Precisão de 97%
confusionMatrix(matriz_confusao)

