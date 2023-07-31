##########################################################
# UPC School - Big Data Management, Technologies and Analytics
# Data Analytics Laboratory 3: Neural Networks (I)
##########################################################

library(MASS)
library(nnet)
set.seed (1233)

####################################################################
## Multilayer Perceptron Example: digit recognition
####################################################################

zip_train <- read.csv("zips/optdigits.tra", header = FALSE, sep=",")
zip_test <- read.table("zips/optdigits.tes", header = FALSE, sep=",")

colnames(zip_train) <- c(paste("d", 1:64, sep=""), "digit")
colnames(zip_test) <- c(paste("d", 1:64, sep=""), "digit")

zip_train$digit <- as.factor(zip_train$digit)
zip_test$digit <- as.factor(zip_test$digit)

summary(zip_train)
summary(zip_test)

dim(zip_train)
dim(zip_test)

options(digits = 2)

# This will build a multinomial regression (generalizes logistic regression for more than two classes)

train_test_multinom <- function (lambda=0, epochs=100)
{
  multinom.model <- multinom (digit ~ ., data=zip_train, maxit=epochs, decay=lambda)
  
  # Training error
  multinom.train <- apply(multinom.model$fitted.values, 1, which.max)-1
  
  (multinom.train_ct <- table(Truth=zip_train$digit, Pred=multinom.train))
  cat("\n\nTraining error", (1-sum(diag(multinom.train_ct))/sum(multinom.train_ct))*100,"%\n")
  
  # Test error
  multinom.test <- predict(multinom.model, zip_test)
  
  (multinom.test_ct <- table(Truth=zip_test$digit, Pred=multinom.test))
  cat("Test error", (1-sum(diag(multinom.test_ct))/sum(multinom.test_ct))*100,"%\n")
}

train_test_multinom ()
train_test_multinom (epochs = 500)   # overfits the training set
train_test_multinom (lambda=1)       # no difference
train_test_multinom (lambda=1000)    # starts to underfit

# We typically use CV to choose the best lambda

### Now we switch to non-linear modelling with a MLP

## The nnet() function is quite powerful and very reliable from the optimization
## point of view, including a L2-regularization mechanism. 
## From the computational point of view, it has two drawbacks:

## 1- it does not have a built-in mechanism for multiple runs or cross-validation
## 2- it only admits networks of one hidden layer (of size 'size')

## Please have a look at nnet before going any further

?nnet

## The basic parameters are 'size' and 'decay' (the regularization constant = lambda)
## R typically detects the kind of task and adjust the linout, entropy, softmax parameters (?)
## In this case it builds a MLP with then output neurons, with the softmax function
## and uses the generalized cross-entropy as error function to be minimized

## In general it is a good idea to start by scaling the inputs; this is important to avoid network 'stagnation' (premature convergence); in this case it brings trouble because of the data distribution (e.g., there are columns of '0's that should be removed, but this is not clear)

## To illustrate the first results, we just fit a MLP with 2 hidden neurons

model.nnet <- nnet(digit ~., data = zip_train, size=2, maxit=200, decay=0)

## Take your time to understand the output
model.nnet 

## In particular, understand why the total number of weights is 160, what 'initial  value' and 'final  value' are and what does 'converged' mean

# (64+1)*2(input->hidden) + (2+1)*10(hidden->output) = 160

# This is the final value of the error function (also known as fitting criterion)
model.nnet$value

#  fitted values for the training data
model.nnet$fitted.values

# and the residuals
model.nnet$residuals

## Now look at the weights
model.nnet$wts

sqrt(model.nnet$wts %*% model.nnet$wts)

## I think this way is clearer:

summary(model.nnet)

## i1,i2,i3,... are the inputs, h1, h2, ... are the hidden neuron's outputs, b is the bias (offset)

## As you can see, some weights are large (two orders of magnitude larger then others)
## This is no good, since it makes the model unstable (i.e., small changes in some inputs may
## entail significant changes in the network, because of the large weights).

# Now let's fit a larger network and compute the training and test errors
model.nnet <- nnet(digit ~., data = zip_train, size=20, maxit=200, decay=0)
# error!

model.nnet <- nnet(digit ~., data = zip_train, size=20, maxit=200, decay=0, MaxNWts=1510)

summary(model.nnet)

# Now let's compute the training error (less than 1%)

p1 <- as.factor(predict (model.nnet, type="class"))

t1 <- table(Truth=zip_train$digit, Pred=p1)
(1-sum(diag(t1))/sum(t1))*100

# And the corresponding test error

p2 <- as.factor(predict (model.nnet, newdata=zip_test, type="class"))

t2 <- table(Truth=zip_test$digit, Pred=p2)
(1-sum(diag(t2))/sum(t2))*100

## We get 9.5%, so it seems that this MLP is overfitted; we need to work harder ...

## One way to avoid this is by regularizing the learning process:

model.nnet <- nnet(digit ~., data = zip_train, size=20, maxit=200, decay=0.05, MaxNWts=1510)

## notice the difference
model.nnet$wts

sqrt(model.nnet$wts %*% model.nnet$wts)

summary(model.nnet)

## We are going to do the modelling in a principled way now. Using repeated CV to select the best
## combination of 'size' and 'decay'

## Just by curiosity, let me show you that we can fit almost any dataset (in the sense of reducing the training error):

model.nnet <- nnet(digit ~., data = zip_train, size=50, maxit=200, decay=0, MaxNWts=10000)

# Now let's compute the training error

p1 <- as.factor(predict (model.nnet, type="class"))

t1 <- table(Truth=zip_train$digit, Pred=p1)
(1-sum(diag(t1))/sum(t1))*100

## That's it: we got a null training error, but it is illusory ... 

## {caret} is an excellent package for training control, once you know what all these concepts are

## WARNING: if the package is not installed in your computer, installation needs some previous packages
library(caret)

## For a specific model, in our case the neural network, the function train() in {caret} uses a "grid" of model parameters and trains the network using a given resampling method (in our case we will be using 10x10 CV). 
## All combinations are evaluated, and the best one (according to 10x10 CV) is chosen and used to construct a final model, which is then refit using the whole training set

## Thus train() returns the constructed model (exactly as a direct call to nnet() would)

## In order to find the best network architecture, fix a large number of hidden units in one hidden layer, and explore different regularization values (recommended)

## doing both (explore different numbers of hidden units AND regularization values) is usually a waste of computing 
## resources (but notice that train() would admit it)

## specify 10x10 CV
trc <- trainControl (method="repeatedcv", number=2, repeats=5)

(decays <- 10^seq(-2,0.5,by=0.25))

## WARNING: this takes some time (around 10')
model.5x2CV <- train (digit ~., data = zip_train, method='nnet', maxit = 100, trace = FALSE,
                      tuneGrid = expand.grid(.size=20,.decay=decays), trControl=trc, MaxNWts=1510)

### A more realistic experimental setting (with more computational power, but only if really needed) a larger size and set of decays (maybe by refinement)

####################################
save(model.5x2CV, file = "5x2-CV.mod")

load ("5x2-CV.mod")
####################################

## We can inspect the full results
model.5x2CV$results

## and the best model found
model.5x2CV$bestTune

## We should choose the model with the lowest CV error overall

## So what remains is to predict the test set with our final model

p2 <- as.factor(predict (model.5x2CV, newdata=zip_test, type="raw"))

t2 <- table(Truth=zip_test$digit, Pred=p2)
(1-sum(diag(t2))/sum(t2))*100

## We get 5.5% after all this work; it seems that the use of a more sophisticated learner does is not justified (or is it? what do you think?)
