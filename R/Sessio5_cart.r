
##############################################################
# SETTING THE WORKING PATH 
setwd("D:/docent/curs Big Data/2020-2021/Sessio5 cart/R/")

# CARGAMOS LAs LIBRERIAS NECESARIAS 

library(rpart)
library(rpart.plot)
library(rattle)

#  READING CREDSCO.TXT
dd <- read.table("credsco2.txt",header=T)

# DIMENSIONS AND SUMMARY OF DATA
dim(dd)
summary(dd)


############################################################### CART DECISION TREE 

n <- nrow(dd)

# WE USE 2/3 OF THE DATA TO ASCERTAIN THE OPTIMAL DECISION TREE AND THE REMAINING 1/3 TO ASSESS THE QUALITY OF THE DECISION TREE

# LEARN CONTAINS THE VECTOR OF TRAINING INDIVIDUALS

set.seed(7)
learn <- sample(1:n, round(0.67*n))

(nlearn <- length(learn))
(ntest <- n - nlearn)

# SELECTION OF THE DECISION TREE BY CROSSVALIDATION
# FIRST WE BUILD A MAXIMAL TREE (WITH LOW VALUE OF cp) WITH xval=10

set.seed(27)
p2 = rpart(Dictamen ~ ., data=dd[learn,], control=rpart.control(cp=0.001, xval=10))

# THE OBTAINED MAXIMAL TREE
p2
plot(p2)

# THE SEQUENCE OF TREES WITH THEIR COMPLEXITY PARAMETER AND COMPUTED ERROR IN THE TRAINING SAMPLE AND BY CROSSVALIDATION
printcp(p2)

plot(p2$cptable[,2],p2$cptable[,3],type="l",xlab="size of the tree",ylab="Relative impurity",main="R(t)")
lines(p2$cptable[,2],p2$cptable[,4],col="blue")
legend("topright",c("R(T)training","R(T)cv"),col=c("black","blue"),lty=1)

plotcp(p2)

# LETS TAKE THE COMPLEXITY PARAMETER CORRESPONDING TO THE MINIMUM CV. ERROR + 1 SD
p2$cptable = as.data.frame(p2$cptable)
ind = which.min(p2$cptable$xerror)

xerr <- p2$cptable$xerror[ind]
xstd <- p2$cptable$xstd[ind]

i = 1
while (p2$cptable$xerror[i] > xerr+xstd) i = i+1

alfa = p2$cptable$CP[i]

# SELECTED VALUE OF THE COMPLEXITY PARAMETER
alfa

# AND PRUNE THE TREE ACCORDINGLY
p1 <- prune(p2,cp=alfa)

p1
rpart.plot(p1, main="Optimal Tree p2")

# RESULTS OF THE TREE
summary(p1)

# IMPORTANCE OF VARIABLES IN THE TREE DEFINITION
barplot(p1$variable.importance, main="Variable importance")

# A NICER PLOT with Rattle
fancyRpartPlot(p1)

# RULES with Rattle
asRules(p1)

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

############################################################### ASSESSING THE QUALITY OF THE TREE

# WE TAKE THE TEST DATA AND LET THEM DROP DOWN THE TREE
# (WE HAVE TO CHEAT THE TREE, WE CHANGE THE RESPONSE FOR THE LEAF NUMBER TO KNOW THE LEAF FOR EACH INDIVIDUAL IN THE TEST DATA)

t1 = p1
t1$frame$yval = as.numeric(row.names(t1$frame))

pt1 = predict(t1, newdata=dd[-learn,],type="vector")
head(pt1)

pt1=cbind(pt1,dd$Dictamen[-learn]) # adding the true dictamen
head(pt1)

# POSITIVES AND NEGATIVES PER LEAF IN TEST DATA
t1.leaf = as.data.frame.matrix(table(pt1[,1],pt1[,2]))
names(t1.leaf)=c("n1_test","n2_test")
t1.leaf

# MERGING THE RESULTS OF THE TRAIN DATA WITH THE TEST DATA
pt1.leaf = merge(p1.leaf,t1.leaf, by="row.names", all=T)

# ADDING NEW COLUMNS WITH THE PROPORTION OF POSITIVES IN TEST
pt1.leaf$n_test  = pt1.leaf$n1_test + pt1.leaf$n2_test
pt1.leaf$p2_test = pt1.leaf$n2_test/pt1.leaf$n_test

# SELECTING THE RESULTS TO EXPORT
pt1.leaf = subset(pt1.leaf, select=c(Row.names, n_train, n1_train, n2_train, p2_train, n_test, n1_test, n2_test, p2_test))
pt1.leaf <- pt1.leaf[order(pt1.leaf$p2_train,decreasing=T),]
pt1.leaf

# EXPORTING THE RESULTS TO EXCEL (TRAINING + TEST)

tab_results = data.frame(matrix(NA, nrow=nrow(pt1.leaf), ncol=4))
row.names(tab_results) = pt1.leaf$Row.names
tab_results[,1] = pt1.leaf$n_train + pt1.leaf$n_test
tab_results[,2] = pt1.leaf$n1_train + pt1.leaf$n1_test
tab_results[,3] = pt1.leaf$n2_train + pt1.leaf$n2_test
tab_results[,4] = tab_results[,3]/tab_results[,1]
names(tab_results) = c("n","n1","n2","p2")

# RANKING THE LEAVES BY DECREASING PROBABILITIES
tab_results = tab_results[order(-tab_results$p2),]
print(tab_results)

library(xlsx)
write.xlsx(tab_results, "results.xlsx") 


#############################################################
#############################################################

# EVALUATING PERFORMANCE, ROC CURVE, AUC
# WE CONSIDER BOTH LEARN AND TEST SAMPLES TOGETHER

pred_learn <- as.data.frame(predict(p1, data=dd[learn,],type="prob"))
pred_test  <- as.data.frame(predict(p1, newdata=dd[-learn,],type="prob"))

library(ROCR)
pred <- prediction(c(pred_learn$positiu,pred_test$positiu), c(dd$Dictamen[learn],dd$Dictamen[-learn]))
roc <- performance(pred,measure="tpr",x.measure="fpr")
plot(roc, main="ROC curve")
abline(0,1,col="blue")

auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc

con <- performance(pred,measure="tpr",x.measure="rpp")
plot(con, main="Concentration curve")
abline(0,1,col="blue")


#############################################################

# ERROR RATE IN THE LEARNING SAMPLE

pred_learn=as.data.frame(predict(p1,data=dd[learn,],type="prob"))

threshold = 0.5

dec.learn=NULL
dec.learn[pred_learn$positiu < threshold] = "pred_neg.learn"
dec.learn[pred_learn$positiu >=threshold] = "pred_pos.learn"
table(dd$Dictamen[learn],dec.learn)
aa <- table(dd$Dictamen[learn],dec.learn)
accuracy.learn <- 100*(aa[1,1]+aa[2,2])/nlearn 
accuracy.learn

precision.learn = 100*aa[2,2]/(aa[1,2]+aa[2,2])
precision.learn

recall.learn = 100*aa[2,2]/(aa[2,1]+aa[2,2])
recall.learn
  
# ERROR RATE IN THE TEST SAMPLE

dec.test=NULL
dec.test[pred_test$positiu < threshold] = "pred_neg.test"
dec.test[pred_test$positiu >=threshold] = "pred_pos.test"
table(dd$Dictamen[-learn],dec.test)
aa <- table(dd$Dictamen[-learn],dec.test)
accuracy.test <- 100*(aa[1,1]+aa[2,2])/ntest 
accuracy.test

precision.test = 100*aa[2,2]/(aa[1,2]+aa[2,2])
precision.test

recall.test = 100*aa[2,2]/(aa[2,1]+aa[2,2])
recall.test


##############################################################
##############################################################
#   RANDOM FOREST

library(randomForest)

set.seed(17)
p1.rf <- randomForest(Dictamen ~ ., data=dd[learn,], mtry=3, importance=TRUE, xtest=dd[-learn,-1], ytest=dd[-learn,1], nodesize=50, maxnodes=14 )

print(p1.rf)

varImpPlot(p1.rf)



