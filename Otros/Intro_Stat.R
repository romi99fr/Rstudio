## Joan Puigdomenech ##
## Joel Romia        ##


install.packages("DescTools")
library(DescTools)

#1#

pisos = c(7.80, 12.60, 15.96, 13.50, 8.25, 31.29, 16.46)
mean(pisos)
median(pisos)
sd(pisos)
Q1 = summary(pisos)[2]
Q1
Q3 = summary(pisos)[5]
Q3
IQR = Q3 - Q1
IQR

boxplot(pisos, main = "Boxplot de pisos", horizontal = TRUE)

#2#

# Es preferible una muestra extraída al azar ya que es una muestra representativa         #
# del conjunto, en cambio si un experto hace una selección, sería una muestra intencional #
# del conjunto, por lo tanto, esta depende del sesgo (error) que introduce el experto     #
# y no lo podemos controlar.                                                              #


#3#

# en la variable de tiempo que tardo en ir de casa al trabajo podemos encontrar factores #
# sistemáticos como el despertarse tarde7trasnochar o la velocidad a la que ando en el   #
# trayecto, en cambio como factor aleatorio puede ser el clima, retraso en el trasporte  #   
# público...                                                                            #


#4#

# Se considera una distribución normal cuando muchas variables aleatorias continuas     #
# tienen una única moda que coincide con su media y mediana, una distribución           #
# simétrica.                                                                            #

#5#

df = read.table("bcn_pisos.txt", header=TRUE)
fix(df)
summary(df)
sum(duplicated(df)) #75 duplicados#
df = df[!duplicated(df),] 
nrow(df)
sum(duplicated(df))
summary(df)

# Existen registros duplicados, basicamente porque antiguamente cuantos mas registros
# hubiera, más dinero se cobraba, por lo tanto, los encargados de resgistrar los datos
# almacenaban datos duplicados.


#6#

fix(df)
summary(df)

## Variables continuas ##

#Valor#
par(mfrow = c(2, 2))

dx1 = density(df$Valor)
hist(df$Valor, main= "Histograma y densidad",freq = FALSE, xlab = "Valor", breaks = 35)
lines(dx1, lwd = 2, col = "red")

#ValSol#
dx2 = density(df$ValSol)
hist(df$ValSol, main= "Histograma y densidad",freq = FALSE, xlab = "ValorSol", breaks = 35)
lines(dx2, lwd = 2, col = "red")

#Superf#
dx3 = density(df$Superf)
hist(df$Superf, main= "Histograma y densidad",freq = FALSE, xlab = "Superf", breaks = 35)
lines(dx3, lwd = 2, col = "red")

#Edat#
dx4 = density(df$Edat)
hist(df$Edat, main= "Histograma y densidad",freq = FALSE, xlab = "Edat", breaks = 35)
lines(dx4, lwd = 2, col = "red")

## Variables Categoricas ##

#Dor#
par(mfrow=c(2,4))

df$Dorm = as.factor(df$Dorm)
plot(df$Dorm, main = "Diagrama de barras",
     xlab = "Dormitorios", ylab = "Frecuencia", 
     col = c("royalblue", "seagreen", "purple", "grey","yellow"))

#Planta#
df$Planta = as.factor(df$Planta)
plot(df$Planta, main = "Diagrama de barras",
     xlab = "Plantas", ylab = "Frecuencia", 
     col = c("royalblue", "seagreen", "purple") )

#Banys#
df$Banys = as.factor(df$Banys)
plot(df$Banys, main = "Diagrama de barras",
     xlab = "Banys", ylab = "Frecuencia", 
     col = c("royalblue", "seagreen", "purple", "grey") )

#Estat#
df$Estat = as.factor(df$Estat)
plot(df$Estat,
     xlab = "Estado", ylab = "Frecuencia", 
     col = c("royalblue", "seagreen", "purple", "grey", "yellow"))

#Dist#
df$Dist = as.factor(df$Dist)
plot(df$Dist,xlab = "Estado", ylab = "Frecuencia")

#Tipus#
df$Tipus = as.factor(df$Tipus)
plot(df$Tipus,xlab = "Tipus", ylab = "Frecuencia", col = c("red", "blue"))

#Ascen#
df$Ascens = as.factor(df$Ascens)
plot(df$Ascens,xlab = "Estado", ylab = "Frecuencia", col = c("red", "blue"))

#ExtInt#
df$ExtInt = as.factor(df$ExtInt)
plot(df$ExtInt, ylab = "Frecuencia", col = c("yellow", "orange"))

#7#

# Variables continuas: Valor, Valorsol, Superf, Edat. #

par( mfrow= c(1,1) )

dfvc = data.frame(df$Valor,df$Superf, df$Edat, df$ValSol)
fix(dfvc)

plot(df$Superf, df$Valor)
plot(df$Edat, df$Valor)
plot(df$ValSol, df$Valor)

outlier.scores = LOF(scale(dfvc), k=5)
plot(density(outlier.scores))
outliers = order(outlier.scores, decreasing=T)[1:3]
outliers

cbind(outlier.scores[outliers],dfvc[outliers,])
dfout = dfvc[-outliers,]

plot(dfout$df.Superf, dfout$df.Valor)
plot(dfout$df.Edat, dfout$df.Valor)
plot(dfout$df.ValSol, dfout$df.Valor)

#8#

plot(scale(dfout$df.Superf), scale(dfout$df.Valor))
cov(dfout$df.Superf,dfout$df.Valor)
cor(dfout$df.Superf,dfout$df.Valor)


#9#

unique(df$Dist)

plot(dfvc$df.Valor, df$Dist)
oneway.test(dfvc$df.Valor~df$Dist, var.equal = T)

#10#

chisq.test(df$Dist,df$Estat)
