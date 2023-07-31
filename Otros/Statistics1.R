#1.1
precio <- c(7.80, 12.60, 15.96, 13.50, 8.25, 31.29, 16.46)
summary(precio)
Q1 <- summary(precio)[2]
Q3 <- summary(precio)[5]
iqr <- Q3 - Q1
cat("RI:" ,iqr)
boxplot(precio, horizontal = T)

#1.2
#Cuanto más grande sea el tamaño de la muestra, las estimaciones serán más precisas.
#La diversidad de valores independientes que tendremos.
#La media obtenida es más representativa de la población, ya que ha sido obtenida con valores aleatorios.

#1.3
#Tiempo que tardo en ir de mi casa al trabajo

#Fatores sistematicos: Distancia, velocidad
#Factores aleatorios: Accidentes, Atascos, averias

#1.4
#Podemos asegurar que se acerca a una distribución normal cuando, hay muchos factores
#independientes escogidos al azar y todos ellos con similar importancia.

#1.5
dd <- read.table("bcn_pisos.txt", header=TRUE)
summary(dd)
duplicated(dd)
dd2 <- dd[duplicated(dd), ]
dd2

#1.6
library(ggplot2) 
dd <- read.table("bcn_pisos.txt", header=TRUE)
hist(c(dd$Valor, dd$Superf, dd$Dorm, dd$Banys, dd$Edat, dd$ValSol))
df <- data.frame(result = c(dd$Estat, dd$Planta, dd$Dist, dd$Tipus, dd$Ascens, dd$ExtInt, dd$Reforma))
ggplot(df, aes(x=result)) + geom_bar()

#1.7
dd <- read.table("bcn_pisos.txt", header=TRUE)
steps <- dd[dd$Superf < 400, "Superf"]
summary(steps)

#1.8
dd <- read.table("bcn_pisos.txt", header=TRUE)
plot(dd$Valor~dd$Superf, main="Relación entre Valor del piso y su superficie")
cov(dd$Valor,dd$Superf)
cor(dd$Valor,dd$Superf)

#1.9
dd <- read.table("bcn_pisos.txt", header=TRUE)
oneway.test(dd$Valor~dd$Dist,var.equal=T)


#1.10
dd <- read.table("bcn_pisos.txt", header=TRUE)
chisq.test(dd$Dist,dd$Estat)

