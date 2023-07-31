## Joan Puigdomenech Moreno ##
## Joel Romia               ##

#  LEEMOS LOS DATOS

df_churn = read.table("churn.txt", header = TRUE)
summary(df_churn)

# ELIMINAMOS INCONSISTENCIAS

unique(df_churn$edatcat)
unique(df_churn$antig)

df_edat_antig = data.frame(df_churn["antig"],df_churn["edatcat"])
subset(df_edat_antig, edatcat!= "edatcat 66.." & antig==99)

df_churn = df_churn[-c(385,430,683,802,1288,1289,1290,1291),]
nrow(df_churn)

df_churn$sexo = replace(df_churn$sexo, df_churn$sexo == "No informado", "MUJER")

# ELIMINAMOS MISSING VALUES

which(is.na(df_churn))
sum(is.na(df_churn))
dim(df_churn)
df_churn = na.omit(df_churn)
dim(df_churn)


library(FactoMineR)

names(df_churn)
prove = df_churn[df_churn$Baja == 'Baja SI', ] #996 Baja SI
1985 - 996 #989 Baja NO

unique(df_churn$sexo)
unique(df_churn$edatcat)
unique(df_churn$Nomina)
unique(df_churn$Pension)
unique(df_churn$Debito_normal)
unique(df_churn$Debito_aff)
unique(df_churn$VISA)
unique(df_churn$VISA_aff)
unique(df_churn$MCard)
unique(df_churn$Amex)
unique(df_churn$dif_resid)

names(df_churn)
summary(df_churn)


var_quali = c(1,2,3,5,6,7,8,9,10,11,12,18)
var_quanti = c(4,19,20,21,22,23,24,25,26,27,28,29,30)

pca = PCA(df_churn, quanti.sup = var_quanti,  quali.sup = var_quali, scale.unit=TRUE)
pca

#Problema 1
nd = 5

Psi = pca$ind$coord[,1:nd]
# CALCULO DE LA MATRIZ DE DISTANCIAS ENTRE COCHES A PARTIR DE LAS LAS COMPONENTES SIGNFICATIVAS
pca.nd <- dist(Psi)
# CLUSTERING JERARQUICO, METODO DE Ward
hclus.pca <- hclust(pca.nd,method="ward.D2")    

# PLOT DEL ARBOL JERAQUICO OBTENIDO
plot(hclus.pca,cex=1)

#se trata de un procedimiento general donde el criterio para la elección del par de 
#clusters a mezclar en cada paso se basa en el valor óptimo de una función objetivo.

#Problema 2
# DIAGRAMA DE BARRAS DEL INDICE DE AGREGACION DE LAS ULTIMAS 29 AGREGACIONES FORMADAS
barplot(hclus.pca$height[(nrow(df_churn)-29):(nrow(df_churn)-1)])


#Problema 3
# CORTE DEL ARBOL DE AGREGACION EN nc CLASES
nc = 5
cut6 <- cutree(hclus.pca,nc)

# GRAFICO DE LAS nc CLASES EN EL PRIMER PLANO FACTORIAL
plot(Psi[,1],Psi[,2],type="n",main="Clustering of cars in 5 classes")
text(Psi[,1],Psi[,2],col=cut6,labels=row.names(df_churn),cex = 0.6)
abline(h=0,v=0,col="gray")
legend("topleft",c("c1","c2","c3","c4","c5"),pch=20,col=c(1:5))

# NUMERO DE COCHES POR CLASE
table(cut6)

# CALIDAD DEL CORTE DEL ARBOL JERARQUICO

cdg <- aggregate(as.data.frame(Psi),list(cut6),mean)[,2:(nd+1)]
cdg

Bss <- sum(rowSums(cdg^2)*as.numeric(table(cut6)))
Tss <- sum(Psi^2)

100*Bss/Tss

#Problema 4
#Consiste en unir o combinar dos o más particiones, de manera que se obtenga una partición más grande y más general,
#la operación simplifica la estructura de la partición y elimina redundacnias en la información.
# CONSOLIDACION DE LA PARTICION

# CALCULO DE LOS CENTROIDES DE LAS nc CLASES OBTENIDAS POR CORTE DEL ARBOL JERARQUICO
cdg.nc <- aggregate(as.data.frame(Psi),list(cut6),mean)[,2:(nd+1)]


# ALGORITMO kmeans CON CENTROS INICIALES EN LOS CENTROIDES cdg.nc
kmeans <- kmeans(Psi,centers=cdg.nc)

# NUMERO DE COCHES POR CLASE FINAL
kmeans$size


#Problema 5
# CALIDAD DE LA PARTICION FINAL EN 5 CLASSES

100*kmeans$betweenss/kmeans$totss

# VISUALIZACION DE LAS nc CLASES FINALES EN EL PRIMER PLANO FACTORIAL
plot(Psi[,1],Psi[,2],type="n",main="Clustering of cars in 5 classes")
text(Psi[,1],Psi[,2],col=kmeans$cluster,labels=row.names(df_churn),cex = 0.6)
abline(h=0,v=0,col="gray")
legend("topleft",c("c1","c2","c3","c4","c5"),pch=20,col=c(1:5))


#Problema 6
# INTERPRETACION DE LAS nc CLASES FINALES OBTENIDAS
catdes(cbind(as.factor(kmeans$cluster),df_churn),num.var=1,proba=0.001)

#Problema 7
library(class)
pred_sup<-knn1(kmeans$centers, Psi, cl=c("c1","c2","c3","c4","c5"))
pred_sup

#Problema 8
table(cut6, df_churn$Baja)
prop_matrix <- prop.table(table(cut6, df_churn$Baja), margin = 1)
barplot(prop_matrix, beside = TRUE, legend.text = TRUE, 
        xlab = "Cluster", ylab = "Proporción de clientes", 
        main = "Proporción de clientes por cluster y estado de baja")
