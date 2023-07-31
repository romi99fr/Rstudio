#Exercise 1
#1
v = c(1:20)
v[2] = 20
v[20] = 2
v

#2
v2 =  rep(c(1,2,3,NA),5)
v2[is.na(v2)] = mean(v2, na.rm = TRUE)
v2

#3
Age=c(22, 25, 18,20)
Name=c("James", "Mathew", "Olivia", "Stella")
Gender=c("M", "M", "F", "F")
df = data.frame(Age=c(22, 25, 18,20),Name=c("James", "Mathew", "Olivia", "Stella"),Gender=c("M", "M", "F", "F"))
df

#4
df$Name[df$Age >= 21]

#5
!is.null(df$Name[df$Age >= 21])

#6
library(datasets)
data(iris)
summary(iris)
apply(iris[1:4], MARGIN=1, FUN = mean)
apply(iris[1:4], MARGIN=2, FUN = mean)

#7
function_iris <- function(iris){
  newiris = apply(iris[1:4], MARGIN=1, FUN = mean)
  newiris2 = apply(iris[1:4], MARGIN=2, FUN = mean)
  newiris2 = c(newiris2, 0)
  return(newiris + newiris2)
}
function_iris(iris)

#8
fib <- function(n) {
  if(n <= 1) {
    return(n)
  } else {
    return(fib(n-1) + fib(n-2))
  }
}
fib(30)

#9
setwd("c:/Program Files/RStudio")
data_frame = read.csv("Qualitat_Aire_Detall.csv")
head(data_frame)
data_frame$ESTACIO = as.factor(data_frame$ESTACIO)
plot(data_frame$ESTACIO, data_frame$CODI_CONTAMINANT, ylab="CodiContamiant", xlab="Estacio", col="red")
mean(data_frame$CODI_CONTAMINANT)

#Exercise 2
#1
setwd("c:/Program Files/RStudio")
data_frame2 = read.csv("Choices.csv", sep=";")
head(data_frame2)
dim(data_frame2)
names(data_frame2)
attach(data_frame2)
typeof(ID)
typeof(COUNTRY)
typeof(CHOICE_ID)
typeof(INFO)
typeof(MEASURE)

#2
conTable = table(MEASURE, INFO)
conTable

#3
addmargins(conTable)

#4
chisq.test(conTable)
#Al tener un p-value muy pequeño("2.2*10^-6") podemos confirmar que hay una relación 
#entre las variables de MEASURE y INFO.

#Trabajo realizado por: Joan Puigdomenech y Joel Romia
