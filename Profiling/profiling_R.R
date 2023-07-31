setwd("C:/Users/USUARIO/OneDrive/Documentos/Master/Clase/Rstudio/exer Prof")

#Problema 1
dd <- read.table("churn.txt",header=T, check.names = F)
summary(dd)
names(dd)
#Problema 2
#La variable de respuesta en este caso es "Baja", que indica si un cliente 
#se ha dado de baja o no.

#Las variables explicativas son las restantes, que son características y 
#comportamientos de los clientes que esta relacionado con la probabilidad de que se den de baja.

#"edatcat": variable numérica discreta que indica la categoría de edad del cliente.
#"sexo": variable categórica que indica el género del cliente.
#"antig": variable numérica que indica la antigüedad del cliente en años.
#"Nomina", "Pension", "Debito_normal", "Debito_aff", "VISA", "VISA_aff", "MCard" y "Amex": variables binarias que indican si el cliente tiene un tipo de producto financiero concreto o no.
#"Total_activo", "Total_Plazo", "Total_Inversion", "Total_Seguros" y "Total_Vista": variables numéricas que indican el valor total que el cliente tiene en diferentes tipos de productos financieros.
#"dif_resid": variable numérica que indica la diferencia entre la dirección del cliente y la dirección de la empresa.
#"oper_caj_Libreta" y "oper_ven_Libreta": variables numéricas que indican el número de operaciones que el cliente ha realizado en una libreta de ahorros en cajeros y en ventanilla, respectivamente.
#"dif_CC", "dif_Libreta", "dif_Plazo", "dif_Ahorro", "dif_Largo_plazo", "dif_Fondos_inv", "dif_Seguros", "dif_Planes_pension" y "dif_Hipoteca": variables numéricas que indican la diferencia entre el valor que el cliente 
#tiene en un tipo de producto financiero y la media del valor que tienen los clientes de su categoría.


#Problema 3
tabla_sexo <- table(dd$sexo)
tabla_nomina <- table(dd$Nomina)
tabla_pension <- table(dd$Pension)
tabla_debito_normal <- table(dd$Debito_normal)
tabla_debito_aff <- table(dd$Debito_aff)
tabla_visa <- table(dd$VISA)
tabla_visa_aff <- table(dd$VISA_aff)
tabla_mcard <- table(dd$MCard)
tabla_amex <- table(dd$Amex)
tabla_oper_caj_libreta <- table(dd$oper_caj_Libreta)
tabla_oper_ven_libreta <- table(dd$oper_ven_Libreta)

# Diagrama de barras para la variable "sexo"
barplot(tabla_sexo, main="Diagrama de barras para la variable Sexo", xlab="Sexo", ylab="Frecuencia", col="blue")

# Diagrama de barras para la variable "Nomina"
barplot(tabla_nomina, main="Diagrama de barras para la variable Nómina", xlab="Nómina", ylab="Frecuencia", col="red")

# Diagrama de barras para la variable "Pension"
barplot(tabla_pension, main="Diagrama de barras para la variable Pensión", xlab="Pensión", ylab="Frecuencia", col="green")

# Diagrama de barras para la variable "Debito_normal"
barplot(tabla_debito_normal, main="Diagrama de barras para la variable Débito normal", xlab="Débito normal", ylab="Frecuencia", col="orange")

# Diagrama de barras para la variable "Debito_aff"
barplot(tabla_debito_aff, main="Diagrama de barras para la variable Débito afinidad", xlab="Débito afinidad", ylab="Frecuencia", col="purple")

# Diagrama de barras para la variable "VISA"
barplot(tabla_visa, main="Diagrama de barras para la variable VISA", xlab="VISA", ylab="Frecuencia", col="blue")

# Diagrama de barras para la variable "VISA_aff"
barplot(tabla_visa_aff, main="Diagrama de barras para la variable VISA afinidad", xlab="VISA afinidad", ylab="Frecuencia", col="red")

# Diagrama de barras para la variable "MCard"
barplot(tabla_mcard, main="Diagrama de barras para la variable MCard", xlab="MCard", ylab="Frecuencia", col="green")

# Diagrama de barras para la variable "Amex"
barplot(tabla_amex, main="Diagrama de barras para la variable Amex", xlab="Amex", ylab="Frecuencia", col="orange")

# Diagrama de barras para la variable "caj_libreta"
barplot(tabla_oper_caj_libreta, main="Diagrama de barras para la variable Amex", xlab="Amex", ylab="Frecuencia", col="orange")

# Diagrama de barras para la variable "ven_libreta"
barplot(tabla_oper_ven_libreta, main="Diagrama de barras para la variable Amex", xlab="Amex", ylab="Frecuencia", col="orange")

# Histograma para la variable "edatcat"
hist(dd$edatcat, main="Histograma para la variable Edad", xlab="Edad", col="blue")

# Histograma para la variable "antig"
hist(dd$antig, main="Histograma para la variable Antigüedad", xlab="Antigüedad", col="red")

# Histograma para la variable "Total_activo"
hist(dd$Total_activo, main="Histograma para la variable Total Activo", xlab="Total Activo", col="green")

# Histograma para la variable "Total_Plazo"
hist(dd$Total_Plazo, main="Histograma para la variable Total Plazo", xlab="Total Plazo", col="orange")

# Histograma para la variable "Total_Inversion"
hist(dd$Total_Inversion, main="Histograma para la variable Total Inversión", xlab="Total Inversión", col="purple")

# Histograma para la variable "Total_Seguros"
hist(dd$Total_Seguros, main="Histograma para la variable Total Seguros", xlab="Total Seguros", col="blue")

# Histograma para la variable "Total_Vista"
hist(dd$Total_Vista, main="Histograma para la variable Total Vista", xlab="Total Vista", col="red")

# Histograma para la variable "dif_resid"
hist(dd$dif_resid, main="Histograma para la variable Diferencia Residencia", xlab="Diferencia Residencia", col="green")

# Histograma para la variable "dif_CC"
hist(dd$dif_CC, main="Histograma para la variable Diferencia Cuenta Corriente", xlab="Diferencia Cuenta Corriente", col="orange")

# Histograma para la variable "dif_Libreta"
hist(dd$dif_Libreta, main="Histograma para la variable Diferencia Libreta", xlab="Diferencia Libreta", col="purple")

# Histograma para la variable "dif_Plazo"
hist(dd$dif_Plazo, main="Histograma para la variable Diferencia Plazo", xlab="Diferencia Plazo", col="blue")

# Histograma para la variable "dif_Ahorro"
hist(dd$dif_Ahorro, main="Histograma para la variable Diferencia Ahorro", xlab="Diferencia Ahorro", col="red")

# Histograma para la variable "dif_Largo_plazo"
hist(dd$dif_Largo_plazo, main="Histograma para la variable Diferencia Largo Plazo", xlab="Diferencia Largo Plazo", col="green")

# Histograma para la variable "dif_Fondos_inv"
hist(dd$dif_Fondos_inv, main="Histograma para la variable Diferencia Fondos de Inversión", xlab="Diferencia Fondos de Inversión", col="orange")

# Histograma para la variable "dif_Seguros"
hist(dd$dif_Seguros, main="Histograma para la variable Diferencia Seguros", xlab="DiferenciaSeguros", col="yellow")


#Problema 4
library(FactoMineR)
desc_Dict <- catdes(dd,num.var=1,proba = 0.01)
desc_Dict


plot(desc_Dict, barplot=T)

#Problema 5
# Recodificación de variables continuas
dd$Total_activo_cat <- cut(dd$Total_activo, breaks=c(0,0.0001,150,400,1000,3000,99000),include.lowest=T)
dd$Total_Plazo_cat <- cut(dd$Total_Plazo, breaks=c(0,0.0001,700,2000,4000,8000,99000),include.lowest=T)
dd$Total_Inversion_cat <- cut(dd$Total_Inversion,breaks=c(0,0.0001,700,2000,4000,8000,99000),include.lowest=T)
dd$Total_Seguros_cat <- cut(dd$Total_Seguros, breaks=c(0,0.0001,150,400,1000,3000,99000),include.lowest=T)
dd$Total_Vista_cat <- cut(dd$Total_Vista, breaks=c(0,0.0001,50,150,400,1000,99000),include.lowest=T)
dd$Rec_oper_caj_Libreta = cut(dd$oper_caj_Libreta, breaks=c(-9000,-100,-20,-0.0001,0,20,100,9000))
dd$Rec_oper_ven_Libreta = cut(dd$oper_ven_Libreta, breaks=c(-9000,-100,-20,-0.0001,0,20,100,9000))
dd$Rec_dif_CC= cut(dd$dif_CC, breaks=c(-99000,-100,-0.0001,0,20,200,1000,99000))
dd$Rec_dif_Libreta= cut(dd$dif_Libreta, breaks=c(-99000,-100,-0.0001,0,20,200,1000,99000))
dd$Rec_dif_Plazo= cut(dd$dif_Plazo, breaks=c(-99000,-100,-0.0001,0,20,200,1000,99000))
dd$Rec_dif_Ahorro= cut(dd$dif_Ahorro, breaks=c(-99000,-100,-0.0001,0,20,200,1000,99000))
dd$Rec_dif_Largo_plazo= cut(dd$dif_Largo_plazo, breaks=c(-99000,-100,-0.0001,0,20,200,1000,99000))
dd$Rec_dif_Fondos_inv= cut(dd$dif_Fondos_inv, breaks=c(-99000,-100,-0.0001,0,20,200,1000,99000))
dd$Rec_dif_Seguros= cut(dd$dif_Seguros, breaks=c(-99000,-100,-0.0001,0,20,200,1000,99000))
dd$Rec_dif_Planes_pension= cut(dd$dif_Planes_pension, breaks=c(-99000,-100,-0.0001,0,20,200,1000,99000))
dd$Rec_dif_Hipoteca= cut(dd$dif_Hipoteca, breaks=c(-99000,-100,-0.0001,0,20,200,1000,99000))
dd$Rec_dif_Prest_personales= cut(dd$dif_Prest_personales, breaks=c(-99000,-100,-0.0001,0,20,200,1000,99000))

# Gráfico de barras para las variables categóricas
par(mfrow=c(3,3))
barplot(prop.table(table(dd$Baja, dd$edatcat)), beside=TRUE, main="Edad")
barplot(prop.table(table(dd$Baja, dd$sexo)), beside=TRUE, main="Sexo")
barplot(prop.table(table(dd$Baja, dd$antig)), beside=TRUE, main="antig")
barplot(prop.table(table(dd$Baja, dd$Nomina)), beside=TRUE, main="Nomina")
barplot(prop.table(table(dd$Baja, dd$Pension)), beside=TRUE, main="Pension")
barplot(prop.table(table(dd$Baja, dd$Debito_normal)), beside=TRUE, main="Debito_normal")
barplot(prop.table(table(dd$Baja, dd$Debito_aff)), beside=TRUE, main="Debito_aff")
barplot(prop.table(table(dd$Baja, dd$VISA)), beside=TRUE, main="VISA")
barplot(prop.table(table(dd$Baja, dd$VISA_aff)), beside=TRUE, main="VISA_aff")
barplot(prop.table(table(dd$Baja, dd$MCard)), beside=TRUE, main="MCard")
barplot(prop.table(table(dd$Baja, dd$Amex)), beside=TRUE, main="Amex")

# Gráfico de barras para las variables continuas recodificadas
par(mfrow=c(4,4))
barplot(prop.table(table(dd$Baja, dd$Total_activo_cat)), beside=TRUE, main="Total_activo")
barplot(prop.table(table(dd$Baja, dd$Total_Plazo_cat)), beside=TRUE, main="Total_Plazo")
barplot(prop.table(table(dd$Baja, dd$Total_Inversion_cat)), beside=TRUE, main="Total_Inversion")
barplot(prop.table(table(dd$Baja, dd$Total_Seguros_cat)), beside=TRUE, main="Total_Seguros")
barplot(prop.table(table(dd$Baja, dd$Total_Vista_cat)), beside=TRUE, main="Total_Vista")
barplot(prop.table(table(dd$Baja, dd$Rec_oper_caj_Libreta)), beside=TRUE, main="Caj Libreta")
barplot(prop.table(table(dd$Baja, dd$Rec_oper_ven_Libreta)), beside=TRUE, main="ven Libreta")
barplot(prop.table(table(dd$Baja, dd$Rec_dif_CC)), beside=TRUE, main="DIF CC")
barplot(prop.table(table(dd$Baja, dd$Rec_dif_Libreta)), beside=TRUE, main="Diferencia Libreta")
barplot(prop.table(table(dd$Baja, dd$Rec_dif_Plazo)), beside=TRUE, main="Diferencia Plazo")
barplot(prop.table(table(dd$Baja, dd$Rec_dif_Ahorro)), beside=TRUE, main="Diferencia Ahorro")
barplot(prop.table(table(dd$Baja, dd$Rec_dif_Largo_plazo)), beside=TRUE, main="Diferencia Largo Plazo")
barplot(prop.table(table(dd$Baja, dd$Rec_dif_Fondos_inv)), beside=TRUE, main="Diferencia Fondos Inv")
barplot(prop.table(table(dd$Baja, dd$Rec_dif_Seguros)), beside=TRUE, main="Diferencia Seguros")
barplot(prop.table(table(dd$Baja, dd$Rec_dif_Planes_pension)), beside=TRUE, main="Diferencia Planes Pension")
barplot(prop.table(table(dd$Baja, dd$Rec_dif_Hipoteca)), beside=TRUE, main="Diferencia Hipoteca")
barplot(prop.table(table(dd$Baja, dd$Rec_dif_Prest_personales)), beside=TRUE, main="Diferencia Prestamos Pers")

#Problema 6
#Sí, el poder adquisitivo del cliente parece tener cierta influencia sobre la compra del producto, 
#ya que la proporción de clientes que compraron el producto es mayor en la clase alta que en la clase baja.

#Para calcular las probabilidades, podemos dividir el número de compras por el total de clientes en cada clase:
  
#Probabilidad de compra en la clase alta: 20/393 = 0.051
#Probabilidad de no compra en la clase alta: 1 - 0.051 = 0.949
#Probabilidad de compra en la clase baja: 6/322 = 0.019
#Probabilidad de no compra en la clase baja: 1 - 0.019 = 0.981


#Podemos calcular las proporciones de compras y no compras para cada grupo:

#Para los adultos:
  
#Probabilidad de compra en la clase alta: 3/179 = 0.017
#Probabilidad de no compra en la clase alta: 1 - 0.017 = 0.983
#Probabilidad de compra en la clase baja: 4/297 = 0.013
#Probabilidad de no compra en la clase baja: 1 - 0.013 = 0.987

#Para los jóvenes:
  
#Probabilidad de compra en la clase alta: 17/214 = 0.079
#Probabilidad de no compra en la clase alta: 1 - 0.079 = 0.921
#Probabilidad de compra en la clase baja: 2/25 = 0.08
#Probabilidad de no compra en la clase baja: 1 - 0.08 = 0.92

#Observamos que la proporción de compras es mayor en los jóvenes que en los adultos, 
#tanto en la clase alta como en la clase baja. Esto sugiere que la edad es un factor 
#determinante en la compra del producto en cuestión.

#En cuanto a si el barrio de residencia también es un factor determinante, 
#podemos observar que las diferencias en las proporciones de compras y no compras 
#entre las clases alta y baja son mayores en los adultos que en los jóvenes. 
#Esto sugiere que el barrio de residencia puede ser un factor determinante para los adultos, 
#pero no tanto para los jóvenes.