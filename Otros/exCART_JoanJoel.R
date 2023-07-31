
##############################################################
# SETTING THE WORKING PATH 
setwd("C:/Users/joanp/OneDrive/Escritorio/Master Big data/AN/Decision Trees/exer_cart")

# CARGAMOS LAs LIBRERIAS NECESARIAS 

library(rpart)
library(rpart.plot)
library(rattle)

#  READING Data
dd = read.table("churn.txt",header=T)

# DIMENSIONS AND SUMMARY OF DATA
dim(dd)
summary(dd)

############################################################### CART DECISION TREE 

n = nrow(dd)

learn = sample(1:n, round(n))
learn

# SELECTION OF THE DECISION TREE BY CROSSVALIDATION
# FIRST WE BUILD A MAXIMAL TREE (WITH LOW VALUE OF cp) WITH xval=10

p2 = rpart(dd$Baja ~ ., data=dd[learn,], control=rpart.control(cp=0.0001, xval=10))

# THE OBTAINED MAXIMAL TREE
p2
plot(p2)

# THE SEQUENCE OF TREES WITH THEIR COMPLEXITY PARAMETER AND COMPUTED ERROR IN THE TRAINING SAMPLE AND BY CROSSVALIDATION
printcp(p2)
plotcp(p2)


#4. / 5.#

# OBTENEMOS EL CP PARA EL XERROR ( PROMEDIO DE LAS 10 CROSSV ) MINIMO
p2$cptable = as.data.frame(p2$cptable)
ind = which.min(p2$cptable$xerror)
ind #12

xerr <- p2$cptable$xerror[ind] # 0.967
xstd <- p2$cptable$xstd[ind] # 0.022348

# BUSCAMOS EL EXERROR MAS PEQUEÑO QUE LA SUMA DE XERR + XSTD Y COGEMOS EL CP
i = 1
while (p2$cptable$xerror[i] > xerr+xstd) i = i+1

alfa = p2$cptable$CP[i]
alfa # 0.005

# PRUNE TREE ALPHA 0.005 ARBOL OPTIMO
p1 <- prune(p2,cp=alfa)

p1
rpart.plot(p1, main="Optimal Tree p2")
summary(p1)

# IMPORTANCE OF VARIABLES IN THE TREE DEFINITION
barplot(p1$variable.importance, main="Variable importance")

# RULES with Rattle
asRules(p1)

#GRAFICO CON LAS REGLAS DE DECISION
rpart.plot(p1, type = 0, extra = 101, under = TRUE, cex = 0.8, main = "Decision Tree with Rules")


# TABLE OF RESULTS IN TRAIN
p1.leaf=subset(p1$frame, var=="<leaf>",select=c(n,yval2))
num_leaf = row.names(p1.leaf)
p1.leaf=data.frame(p1.leaf$n,p1.leaf$yval2)  # concatenating the two outputs
names(p1.leaf) = c("n_train","class_train","n1_train","n2_train","p1_train","p2_train","probnode_train")
row.names(p1.leaf) = num_leaf
p1.leaf=p1.leaf[order(-p1.leaf$p2_train),] # ordering by decreasing positive probabilities

p1.leaf$cum_n1 <- cumsum(p1.leaf$n1_train)/sum(p1.leaf$n1_train)
p1.leaf$cum_n2 <- cumsum(p1.leaf$n2_train)/sum(p1.leaf$n2_train)
p1.leaf$dif_cum <- p1.leaf$cum_n2 - p1.leaf$cum_n1

print(p1.leaf)


#7. #

pred_learn=as.data.frame(predict(p1,data=dd[learn,],type="prob"))

threshold = 0.5

dec.learn=NULL
dec.learn[pred_learn$`Baja SI` < threshold] = "pred_neg.learn"
dec.learn[pred_learn$`Baja SI` >=threshold] = "pred_pos.learn"
table(dd$Baja[learn],dec.learn)
aa <- table(dd$Baja[learn],dec.learn)
accuracy.learn <- 100*(aa[1,1]+aa[2,2])/nlearn 
accuracy.learn

precision.learn = 100*aa[2,2]/(aa[1,2]+aa[2,2])
precision.learn

recall.learn = 100*aa[2,2]/(aa[2,1]+aa[2,2])
recall.learn


#8.#

library(ROCR)
pred <- prediction(c(pred_learn$`Baja SI`), c(dd$Baja[learn]))
roc <- performance(pred,measure="tpr",x.measure="fpr")
plot(roc, main="ROC curve")
abline(0,1,col="blue")

auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc











