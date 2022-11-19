library(readxl)
library(tidyverse)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(xlsx)
library (ggfortify)
library(forecast)
library(tseries)
library(lmtest)
library(urca)
library(xts)

setwd("/Users/andretunez/Downloads/proyecto fore")
bddfinal <- read_excel("bddproyecto.xlsx")
(bddfinal)
names(bddfinal)<-c("fecha","ingreso")
bddfinal$mes <- as.numeric(format(bddfinal$fecha,"%m"))
bddfinal$fecha <- as.numeric(format(bddfinal$fecha,"%Y"))

bddfinal=bddfinal[,c("fecha","mes","ingreso")]

ints2 <-ts(bddfinal$ingreso)
autoplot(ints2)+xlab("Período")+ylab("Ingresos")

#obtener el óptimo modelo de arima
autoarima1<-auto.arima(ints2)
forecast1<-forecast(autoarima1,h=30)

plot(forecast1)
plot(forecast1$residuals)


qqnorm(forecast1$residuals)

acf(forecast1$residuals)
pacf(forecast1$residuals)

summary(autoarima1)
accuracy(autoarima1)


newdata <- data.frame(fecha = c(2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015),
                      mes=c(4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4))                                                                                                                                                                                                                             

abril15<- data.frame(newdata,forecast1)
abril15
