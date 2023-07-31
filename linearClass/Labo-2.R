##########################################################
# UPC School - Big Data Management, Technologies and Analytics
# Data Analytics Laboratory 2: Classification with LDA/QDA/RDA and LogReg
##########################################################

library(MASS)

####################################################################
## Example 1: Visualizing and classifying wines with LDA and QDA
####################################################################

## We have the results of an analysis on wines grown in a region in Italy but derived from three different cultivars.
## The analysis determined the quantities of 13 chemical constituents found in each of the three types of wines. 
## The goal is to separate the three types of wines:

wine <- read.table("wine.data", sep=",", dec=".", header=FALSE)

dim(wine)

colnames(wine) <- c('Wine.type','Alcohol','Malic.acid','Ash','Alcalinity.of.ash','Magnesium','Total.phenols',
                    'Flavanoids','Nonflavanoid.phenols','Proanthocyanins','Color.intensity','Hue','OD280/OD315','Proline')

wine$Wine.type <- as.factor(wine$Wine.type)

summary(wine)

pairs (subset(wine,select=-Wine.type), main="Wine types", pch=21,
       bg=c("red", "yellow", "blue")[unclass(wine$Wine.type)], cex.labels = 0.7)

## For this example we call lda() using a formula; this is most useful
## when our data is in a dataframe format: 

(lda.model <- lda (Wine.type ~ ., data = wine))

### LDA can be used for dimension reduction by plotting the data as given by the discriminant functions; these are the g_k(x) as seen in class

## LDA prints discriminant functions based on centered (but not standardized) variables
## The "proportion of trace" that is printed is the proportion of between-class variance that is explained by successive discriminant functions
## the list element 'class' gives the predicted class 
## The list element 'posterior' holds posterior probabilities  

## We can see that neither Magnesium or Proline seem useful to separate the wines; while
## Flavanoids and Nonflavanoid.phenols do. Ash is mainly used in the LD2.

# Now we project the data in two new axes (scatter plot using the first two discriminant dimensions or LDs)
# These new dimensions are linear combinations of the original 4 measurements

plot(lda.model)

# alternatively, we can do it ourselves, with more control on color and text (wine number)

wine.pred <- predict(lda.model)
plot(wine.pred$x,type="n")
text(wine.pred$x,labels=as.character(rownames(wine.pred$x)),col=as.integer(wine$Wine.type))
legend('bottomright', c("Cultivar 1","Cultivar 2","Cultivar 3"), lty=1, col=c('black', 'red', 'green'), bty='n', cex=.75)

# If need be, we can add the (projected) means to the plot

plot.mean <- function (class)
{
  m1 <- mean(subset(wine.pred$x[,1],wine$Wine.type==class))
  m2 <- mean(subset(wine.pred$x[,2],wine$Wine.type==class))
  print(c(m1,m2))
  points(m1,m2,pch=16,cex=2,col=as.integer(class))
}

plot.mean ('1')
plot.mean ('2')
plot.mean ('3')

# indeed apparently classification is perfect

table(Truth=wine$Wine.type, Pred=wine.pred$class)

# Let us switch to leave-one-out cross-validation

wine.predcv <- update(lda.model,CV=TRUE)
head(wine.predcv$posterior)
print(table(Truth=wine$Wine.type, Pred=wine.predcv$class))

# 2 mistakes (on 178 observations): 1.12% error

## Quadratic Discriminant Analysis is the same, replacing 'lda' by 'qda'
## problems may arise if for some class there are less (or equal) observations than dimensions
## (is not the case for the wine data)

qda.model <- qda (Wine.type ~ ., data = wine)

qda.model

## There is no projection this time (because projection is a linear operator and the QDA boundaries are quadratic ones)

# but let's have a look at classification:

wine.pred <- predict(qda.model)
table(Truth=wine$Wine.type, Pred=wine.pred$class)

# Let us switch to leave-one-out cross-validation

wine.predcv <- update(qda.model,CV=TRUE)
head(wine.predcv$posterior)

print(table(Truth=wine$Wine.type, Pred=wine.predcv$class))

# 1 mistake (on 178 observations): 0.56% error

# it would be nice to ascertain which wine is the "stubborn" one: it is a wine of type '2' classified
# as class '1'. Maybe there is something strange with this wine ...

####################################################################
### Example 2: Logistic Regression for admission into graduate school
####################################################################

## Supppose we are interested in how variables, such as 

## GRE (Graduate Record Exam scores, admission tests)
## GPA (Grade Point Average) and 
## rank (prestige of the undergraduate institution),
## affect admission into graduate school.

## BTW, GPA is total amount of grade points earned by the total amount of credit hours attempted (from 0.0 to 4.0)

## The target variable, admit/don't admit, is a binary variable, which we want to characterize
## and, if possible, to predict (a model)

set.seed(1111)

Admis <- read.csv("Admissions.csv")

## view the first few rows of the data
head(Admis)
summary(Admis)

## We will treat the variables gre and gpa as continuous. The variable rank takes on the values 1 through 4
## we treat it as categorical

Admis$admit <- factor(Admis$admit, labels=c("No","Yes"))
Admis$rank <- factor(Admis$rank)

summary(Admis)

## two-way contingency table of outcome and rank,
## we want to make sure the data is OK (no zeros or something strange)
xtabs(~admit + rank, data = Admis)

N <- nrow(Admis)

## We first split the available data into learning and test sets, selecting randomly 2/3 and 1/3 of the data
## We do this for a honest estimation of prediction performance

learn <- sample(1:N, round(2*N/3))

nlearn <- length(learn)
ntest <- N - nlearn

# First we build a maximal model using the learn data

Admis.logreg <- glm (admit ~ gre + gpa + rank, data = Admis[learn,], family = "binomial")
summary(Admis.logreg)

## gre is not statistically significant (although gpa is but by a small margin); the three terms for rank
## are all statistically significant. Guess why rank1 is not considered in the model?

## Then we try to simplify the model by eliminating the least important variables progressively 
## using the step() algorithm which penalizes models based on the AIC value
## This value is the sum of the error plus twice the number of parameters

Admis.logreg.step <- step (Admis.logreg)

# in this case no variable is removed!, but here is the general code to refit the model:

Admis.logreg <- glm (Admis.logreg.step$formula, data = Admis[learn,], family=binomial)
summary(Admis.logreg)

### INFERENCE PART

## We can interpret the coefficients
exp(Admis.logreg$coefficients)

## The exp(coefficients) give the change in the odds of the target ('admit') for a one unit increase 
## in every predictor variable considered in isolation

## Explanation:

# For every one unit change in gre, the odds of admission (versus non-admission) increases a 0.23%
# For every one unit change in gpa, the odds of admission (versus non-admission) increases a 133.4%

# The indicator variables for rank have a different interpretation. 
# For example, having attended an undergraduate institution with rank of 2, versus an institution with a rank of 1, 
# decreases the odds of admission by a 63.8% ...
# whereas having attended an undergraduate institution with rank of 3, versus an institution with a rank of 2, 
# decreases the odds of admission by a 77%, and so on ...
# All these conclusions apply "everything else being equal"

# We now plot the linear predictor and the estimated probabilities
# The colors represent the actual values

plot (Admis.logreg$linear.predictors, Admis.logreg$fitted.values, col=Admis[,"admit"])

# we can see that there is trouble ahead ... ideally we should be looking at a plot split in two by color ...

### PREDICTION PART

## All this is very nice for presentation/explanation purposes, but says nothing about the quality of the model

## let us first calculate the prediction error in the learn data
glfpred<-NULL
glfpred[Admis.logreg$fitted.values<0.5]<-0
glfpred[Admis.logreg$fitted.values>=0.5]<-1
(tab <- with(Admis, table(Truth=admit[learn], Pred=glfpred)))

(error.learn <- 100*(1-sum(diag(tab))/nlearn))

# the error is 29.2%, quite high, although the model does something; most of the errors
# are committed by predicting that many people are not going to be admitted (and they are)

## do the same in the leftout data (test data)
glft <- predict(Admis.logreg, newdata=Admis[-learn,], type="response") 

glfpredt <- NULL
glfpredt[glft<0.5]<-0
glfpredt[glft>=0.5]<-1
(tab <- with(Admis, table(Truth=admit[-learn], Pred=glfpredt)))

error.test <- 100*(1-sum(diag(tab))/ntest)
error.test

# the prediction error is quite high (30.08%), basically for the same reason
# the model is unacceptable, because the error is roughly equal to the one 
# that always predicts the majority class (would have an error of 31.75%)

table(Admis$admit)[2]/N

# the only good news is that both errors (learn and test) are very similar,
# which suggest that there is no over- or under-fitting.
# basically we have a very interpretable model that is a poor predictor
# the solution is to switch to non-linear modeling techniques
# but then we will probably loose interpretability ... life is hard

####################################################################
## Example 3: Logistic regression for spam mail detection
####################################################################

## This last example will illustrate how to change the 'cut point' for prediction, when there is an 
# interest in minimizing a particular source of errors

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

set.seed (4321)
N <- nrow(spam3)                                                                                              
learn <- sample(1:N, round(0.67*N))
nlearn <- length(learn)
ntest <- N - nlearn

## Fit a logistic regression in the learning data
spamM1 <- glm (type ~ ., data=spam3[learn,], family=binomial)

# (do not worry about these warnings: they are fitted probabilities numerically very close to 0 or 1)

## Simplify it using the AIC (this may take a while, since there are many variables)
spamM1.AIC <- step (spamM1)


## We define now a convenience function:

# 'P' is a parameter; whenever our filter assigns spam with probability at least P then we predict spam
spam.accs <- function (P=0.5)
{
  ## Compute accuracy in learning data
  
  spamM1.AICpred <- NULL
  spamM1.AICpred[spamM1.AIC$fitted.values<P] <- 0
  spamM1.AICpred[spamM1.AIC$fitted.values>=P] <- 1
  
  spamM1.AICpred <- factor(spamM1.AICpred, labels=c("nonspam","spam"))
  
  cat("TRAINING ERROR:")
  print(M1.TRtable <- table(Truth=spam3[learn,]$type,Pred=spamM1.AICpred))
  
  print(100*(1-sum(diag(M1.TRtable))/nlearn))
   
  ## Compute accuracy in test data
  
  gl1t <- predict(spamM1.AIC, newdata=spam3[-learn,],type="response")
  gl1predt <- NULL
  gl1predt[gl1t<P] <- 0
  gl1predt[gl1t>=P] <- 1
  
  gl1predt <- factor(gl1predt, labels=c("nonspam","spam"))
  
  cat("TESTING ERROR:")
  print(M1.TEtable <- table(Truth=spam3[-learn,]$type,Pred=gl1predt))
  
  print(100*(1-sum(diag(M1.TEtable))/ntest))
}

spam.accs()
# gives 7.21% TRAINING ERROR and 7.07% TESTING ERROR

## Although the errors are quite low still one could argue that we should try to lower the probability of predicting spam when it is not
# We can do this (at the expense of increasing the converse probability) by:

spam.accs(0.7)

# gives 9.66% TRAINING ERROR and 10.3% TESTING ERROR

## So we get a much better spam filter; notice that the filter has a very low probability of 
## predicting spam when it is not (which is the delicate case)


# Let's compare to LDA and QDA

Spam.LDA.predcv <- lda(type ~ ., data=spam3[learn,],CV=TRUE)

(tab <- table(Truth=spam3[learn,]$type, Pred=Spam.LDA.predcv$class))
(error.LOOCV <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))

Spam.QDA.predcv <- qda(type ~ ., data=spam3[learn,],CV=TRUE)

### ooops!

### let's switch to RDA

library(klaR)

Spam.RDA.predcv <- rda (type ~ ., data=spam3[learn,], crossval = TRUE)

Spam.RDA.predcv$error.rate*100
Spam.RDA.predcv$regularization

RDA.preds <- predict (Spam.RDA.predcv, spam3[-learn,])
(tab <- table(Truth=RDA.preds$class, Pred=spam3[-learn,]$type))
(1-sum(tab[row(tab)==col(tab)])/sum(tab))*100

# You can take your conclusions ...
