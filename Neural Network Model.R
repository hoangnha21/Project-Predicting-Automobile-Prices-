install.packages("readxl")
install.packages("dplyr")
install.packages("ROSE")
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrr)
library(nnet)
library(NeuralNetTools)
library(fastDummies)
library(caret)
library(neuralnet)
library(ROSE)
data <- read_excel("/Users/hoangnha218/Desktop/IDS572 Homework 4/Assignment4Data.xlsx")

mynormalization <- function(x)
{
  (x - min(x))/(max(x)-min(x))
}

data_new <- data %>% mutate_if(is.numeric, mynormalization)
summary(data_new)

indx <- sample(2,nrow(data_new), replace = T, prob = c(0.7,0.3))
train <- data_new[indx == 1,]
test <- data_new[indx ==2,]
View(data_new)


#sample<-ovun.sample(Target~.,data=data_new,method = "over",N=100)$data

#sapply(lapply(data_new, unique), length)


Model <- nnet(Price ~ HP + CC + Grs + Wght + AC + M_Rim , data = train, linout = F, size =10, decay = 0.01, maxit = 1000)


summary(Model)

library(NeuralNetTools)
plotnet(Model)


nn.preds <- (predict(Model, test))
CM <- table(nn.preds, test$Price, dnn = c("predicted", "actual"))
CM

error_metric <- function(CM){
  TN = CM[1,1]
  TP = CM[2,2]
  FN = CM[1,2]
  FP = CM[2,1]
  recall = (TP)/(TP+FN)
  precision = (TP)/(TP+FP)
  error = (FP+FN)/(TP+TN+FN+FP)
  modelPerf <- list("precision" = precision,
                    "recall" = recall,
                    "error" = error)
  return(modelPerf)
}
output <- error_metric(CM)
output
indx <- sample(2,nrow(train), replace = T, prob = c(0.5,0.3))
train2 <- train[indx == 1,]
validation <- train[indx ==2,]
err <- vector("numeric", 30)
d <- seq(0.0001, 1, length.out = 30)
k =1


for(i in d){
  mymodel <- nnet(Price~ HP + CC + Grs + Wght + AC + M_Rim , data = train2, decay=i, size=10, maxit=1000)
  pred.class <- (predict(mymodel, validation))
  err[k] <- mean(pred.class!=validation$Target)
  k <- k+1
}
plotnet(mymodel)

##Answers: Intepret NN Model
