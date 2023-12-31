##########################################################
# UPC School - Big Data Management and Analytics
# Laboratory 5: Using a Random Forest
##########################################################


###################################################
## Example 1: Random Forest for spam mail detection
###################################################

library(kernlab)  

data(spam)

## We do some basic preprocessing

spam[,55:57] <- as.matrix(log10(spam[,55:57]+1))

spam2 <- spam[spam$george==0,]
spam2 <- spam2[spam2$num650==0,]
spam2 <- spam2[spam2$hp==0,]
spam2 <- spam2[spam2$hpl==0,]

george.vars <- 25:28
spam2 <- spam2[,-george.vars]

moneys.vars <- c(16,17,20,24)
spam3 <- data.frame( spam2[,-moneys.vars], spam2[,16]+spam2[,17]+spam2[,20]+spam2[,24])

colnames(spam3)[51] <- "about.money"

dim(spam3)
summary(spam3)

## We do not need to split the available data into learning and test sets, because the OBB is already a honest estimation of prediction performance

# However, since we want to compare different models, we do it, exactly as we have been doing it so far: we split the available data into learning and test sets, selecting randomly 2/3 and 1/3 of the data

set.seed (4321)

N <- nrow(spam3)                                                                                              
learn <- sample(1:N, round(0.67*N))
nlearn <- length(learn)

## First try a standard classification tree (CART)

library(tree)

model.tree <- tree (type ~ ., data=spam3[learn,])

summary(model.tree)

# so training error rate is 9%

model.tree

plot (model.tree)
text (model.tree,pretty=0)

# let's make it predict the leftout test set

pred.tree <- predict (model.tree, spam3[-learn,], type="class")

(ct <- table(Truth=spam3[-learn,]$type, Pred=pred.tree))

## To better study the results, we define a convenience function (the harmonic mean), to compute the F1 accuracy:

harm <- function (a,b) { 2/(1/a+1/b) }

# percent by class
prop.table(ct, 1)
# total percent correct
sum(diag(ct))/sum(ct)

# test error is 11.21%

round(100*(1-sum(diag(ct))/sum(ct)),2)

# the errors are quite well balanced, although someone could claim that we should make an effort to reduce the nonspam classified as spam

(F1 <- harm (prop.table(ct,1)[1,1], prop.table(ct,1)[2,2]))


## Now a random Forest
library(randomForest)

model.rf1 <- randomForest(type ~ ., data=spam3[learn,], ntree=100, proximity=FALSE)

model.rf1

## We get an estimated test error (OOB) of 6.82%, so better, and both errors are quite well balanced; let's compute the real test error:

pred.rf1 <- predict (model.rf1, spam3[-learn,], type="class")

(ct <- table(Truth=spam3[-learn,]$type, Pred=pred.rf1))

# percent by class
prop.table(ct, 1)
# total percent correct
sum(diag(ct))/sum(ct)

# real test error is 

round(100*(1-sum(diag(ct))/sum(ct)),2)

(F1 <- harm (prop.table(ct,1)[1,1], prop.table(ct,1)[2,2]))

## So the prediction error of the RF is much better than that of a single tree; 
## however, there is still an issue in the unbalanced errors

## We should try to lower the probability of predicting spam when it is not
# We can do this (at the expense of increasing the converse probability)

# One way to deal with this is to stratify the sampling in the boostrap resamples:

# 'nonspam' is the less represented class, so we upsample it

(N.nonspam <- table(spam3[learn,]$type)["nonspam"])
(N.spam <- table(spam3[learn,]$type)["spam"])

set.seed(1234)

model.rf2 <- randomForest(type ~ ., data=spam3[learn,], ntree=100, proximity=FALSE, 
                          sampsize=c(nonspam=750, spam=500), strata=spam3[learn,]$type)

model.rf2

## We get an estimated test error (OOB) of 7.12%, and now nonspam is better detected; let's compute the real test error:

pred.rf2 <- predict (model.rf2, spam3[-learn,], type="class")

(ct <- table(Truth=spam3[-learn,]$type, Pred=pred.rf2))

# percent by class
prop.table(ct, 1)
# total percent correct
sum(diag(ct))/sum(ct)

# real test error is 

round(100*(1-sum(diag(ct))/sum(ct)),2)

(F1 <- harm (prop.table(ct,1)[1,1], prop.table(ct,1)[2,2]))

## So we get a much better spam filter

## Now we can try to optimize the number of trees, guided by OOB:

(ntrees <- round(10^seq(1,3,by=0.2)))

# prepare the structure to store the partial results

rf.results <- matrix (rep(0,2*length(ntrees)),nrow=length(ntrees))
colnames (rf.results) <- c("ntrees", "OOB")
rf.results[,"ntrees"] <- ntrees
rf.results[,"OOB"] <- 0

ii <- 1

for (nt in ntrees)
{ 
  print(nt)
  
  model.rf <- randomForest(type ~ ., data=spam3[learn,], ntree=nt, proximity=FALSE, 
                          sampsize=c(nonspam=850, spam=500), strata=spam3[learn,]$type)
  
  # get the OOB
  rf.results[ii,"OOB"] <- model.rf$err.rate[nt,1]
  
  ii <- ii+1
}

rf.results

# choose best value of 'ntrees'

lowest.OOB.error <- as.integer(which.min(rf.results[,"OOB"]))
(ntrees.best <- rf.results[lowest.OOB.error,"ntrees"])

## Now refit the RF with the best value of 'ntrees'

model.rf3 <- randomForest(type ~ ., data=spam3[learn,], ntree=ntrees.best, proximity=FALSE, 
                         sampsize=c(nonspam=750, spam=500), strata=spam3[learn,]$type)

# let's compute the final test error:

pred.rf3 <- predict (model.rf3, spam3[-learn,], type="class")

(ct <- table(Truth=spam3[-learn,]$type, Pred=pred.rf3))

# percent by class
prop.table(ct, 1)
# total percent correct
sum(diag(ct))/sum(ct)

# real test error is 

round(100*(1-sum(diag(ct))/sum(ct)),2)

(F1 <- harm (prop.table(ct,1)[1,1], prop.table(ct,1)[2,2]))

# we can now have a closer look at our model:

summary(model.rf3)
print(model.rf3)

model.rf3$oob.times
model.rf3$confusion

# The importance of variables
importance(model.rf3)
varImpPlot(model.rf3)


## 'about.money' is the most important variable, then 'charDollar', etc

# plot error rate as a function of the number of trees used: 
# black = out of bag (OOB), red = label 1 ('nonspam'), green  = label 2 ('spam')

plot(model.rf3)

legend("topright", legend=c("OOB", "nonspam", "spam"),    
       pch=c(1,1), col=c("black","red","green"))

# What variables are being used in the forest (their total counts)

varUsed(model.rf3, by.tree=FALSE, count = TRUE)

