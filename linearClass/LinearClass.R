##Joan Puigdomenech i Joel Romia
library(MASS)
musk <- read.csv2("Musk2.data", sep=",", header=TRUE)
names(musk) <- c("molecule_name", "conformation_name", paste0("f", 1:162), "OXY-DIS", "OXY-X", "OXY-Y", "OXY-Z", "class")
musk$class <- factor(musk$class)
summary(musk)

musk <- musk[, -c(1:2)]
musk <- data.frame(musk)

# Split the data into training and testing sets
set.seed(123)
train_idx <- sample(nrow(musk), nrow(musk)*0.67)
musk_train <- musk[train_idx, ]
musk_test <- musk[-train_idx, ]

#LDA
# Train an LDA model
lda.model <- lda(class ~ ., data = musk_train)
lda.model
plot(lda.model)

musk.pred <- predict(lda.model, musk_test)
plot(musk.pred$x,type="n")
text(musk.pred$x,labels=as.character(rownames(musk.pred$x)),col=as.integer(musk$class))
legend('bottomright', c("non musk","musk"), lty=1, col=c('red' ,'black'), bty='n', cex=.75)

# Evaluate the performance of the model
table(Truth=musk_test$class, Pred=musk.pred$class)
lda_accuracy <- mean(musk.pred$class == musk_test$class)

#Logistic Regression
# Load the required libraries
library(dplyr)
library(tidyr)
library(caret)

# Fit a logistic regression in the learning data
muskM1 <- glm(class ~ ., data=musk_train, family=binomial)

# Simplify it using the AIC
muskM1.AIC <- step(muskM1)

# Define a function for computing the accuracy
musk.accs <- function(P=0.5) {
  
  # Compute accuracy in learning data
  muskM1.AICpred <- NULL
  muskM1.AICpred[muskM1.AIC$fitted.values < P] <- 0
  muskM1.AICpred[muskM1.AIC$fitted.values >= P] <- 1
  muskM1.AICpred <- factor(muskM1.AICpred, labels=c("non musk","musk"))
  
  cat("TRAINING ERROR:")
  print(M1.TRtable <- table(Truth=musk_train$class, Pred=muskM1.AICpred))
  print(100*(1-sum(diag(M1.TRtable))/nrow(musk_train)))
  
  # Compute accuracy in test data
  muskM1predt <- predict(muskM1.AIC, newdata=musk_test, type="response")
  muskM1predt[muskM1predt < P] <- 0
  muskM1predt[muskM1predt >= P] <- 1
  muskM1predt <- factor(muskM1predt, labels=c("non musk","musk"))
  
  cat("TESTING ERROR:")
  print(M1.TEtable <- table(Truth=musk_test$class, Pred=muskM1predt))
  print(100*(1-sum(diag(M1.TEtable))/nrow(musk_test)))
}

# Compute the accuracy
musk.accs()

