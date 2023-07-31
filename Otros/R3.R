dd <- read.table(file="bcn_pisos.txt", header=TRUE)
sam <- sample(c(TRUE, FALSE), nrow(dd), replace=TRUE, prob=c(0.66,0.33))
train <- dd[sam, ]
test <- dd[!sam, ]
train
test

par(mfrow=c(4,2))
plot(train$Valor, train$Superf, main='Valor vs Superf')
plot(train$Valor, train$Dorm, main='Valor vs Dorm')
plot(train$Valor, train$Banys, main='Valor vs Banys')
plot(train$Valor, train$Edat, main='Valor vs Edat')

cor(train$Valor, train$Superf)
cor(train$Valor, train$Dorm)
cor(train$Valor, train$Banys)
cor(train$Valor, train$Edat)
cor(train$Valor, train$ValSol)

regval<-lm(Valor ~ Superf, data=dd)
n<-length(regval)
d<-lm(Valor ~ Superf+Dorm, data=dd)
summary(d)

library(olsrr)
dd <- read.table(file="bcn_pisos.txt", header=TRUE)
train2 = train[,1:10] 
names(train2)
regval6 <-lm (Valor ~ ., data=train2)
bestval<-ols_step_best_subset(regval6)
bestval

train = dd[1:1553,]
regval6 <-lm (Valor ~ ., data=train)
PRESS <-sum((regval6$residuals/(1-ls.diag(regval6)$hat))^2)
R2loo <-1 -PRESS/(var(train$Valor)*(nrow(train)-1))
R2loo

dd <- read.table(file="bcn_pisos.txt", header=TRUE)
train
names(train)
test= dd[1554:2329,]
regval6 <-lm (Valor ~ ., data=test)
pred_int<-predict(regval6, newdata=test, interval="predict")
pred_int

test= dd[1554:2329,]
regval6 <-lm (Valor ~ ., data=test)
pred<-predict(regval6, newdata=test)
predSSE<-sum((test$Valor-pred)^2)
R2test <-1 -predSSE/(var(test$Valor)*(nrow(test)-1))
R2test

regval6 <-lm (Valor ~ ., data=dd)
anova(regval6)
R2loo

regval6 <-lm (Valor ~ ., data=dd)
plot(density(regval6$residuals),col="red")
par(mfrow= c(2, 2))
plot(regval6)
par(mfrow= c(1, 1))

regval2<-lm(Valor ~ ., data=test)
pred_int<-predict(regval2, newdata=test, interval="predict")
pred_int
PRESS <- sum((regval2$residuals/(1-ls.diag(regval2)$hat))^2)
R2loo <- 1 - PRESS/(var(test$Valor)*(nrow()-1))
R2loo

regval2<-lm(Valor ~ ., data=test)
PRESS <- sum((regval2$residuals/(1-ls.diag(regval2)$hat))^2)
R2loo <- 1 - PRESS/(var(test$Valor)*(nrow(test)-1))
R2loo


pred <- predict(regval, newdata=dd)
head(pred)
predSSE <- sum((dd$Valor-pred)^2)
R2test <- 1 - predSSE/(var(dd$Valor)*(n-1))
R2test

PRESS <- sum((regval$residuals/(1-ls.diag(regval)$hat))^2)
R2loo <- 1 - PRESS/(var(train$Valor)*(n-1))
R2loo

regval2<-lm(Valor ~ Superf, data=test)
PRESS <- sum((regval2$residuals/(1-ls.diag(regval2)$hat))^2)
R2loo <- 1 - PRESS/(var(test$Valor)*(n-1))
R2loo

pred_int<-predict(regval2, newdata=test, interval="predict")
pred_int