## Joan Puigdomenech Moreno ##
## Joel Romia               ##

setwd("C:/Users/joanp/OneDrive/Escritorio/Master Big data/AN/PCA/exer_pca")

########## 1 ##########

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

########## 2 ##########

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

plot(pca$eig[,"eigenvalue"], type='l', main="Screeplot")
pca$eig

########## 3 ##########

pc$eig

plot(pc$eig[,"eigenvalue"], type='l', main="Screeplot")


########## 4 ##########

# CUANTAS COMPONETES SON SIGNFICATIVAS?
nd = 1

# ROTACION DE LAS COMPONENTES PARA BUSCAR COMPONENTES MAS INTERPRETABLES

pca_cor = varimax(pca$var$cor)
pca_cor

########## 5 ##########

# GRAFICO DE LOS INDIVIDUOS EN LAS DIMENSIONES 1 Y 2
plot(pca, axes = c(1, 2), choix = c("ind"), habillage=1, label="quali", title="Plot of individuals", cex=0.7)


























