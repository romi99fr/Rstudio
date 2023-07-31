###                                 ###
###   Association rules: APriori    ###
###                                 ###


########################################################################
# Exercise: analyze the Groceries dataset
########################################################################

#Install packages

library(arules)

#Data preprocessing

data ("Groceries")
summary(Groceries)
Groceries
size(Groceries)
dim(Groceries)

# first 5 transactions
inspect (Groceries[1:5])

# try to visualize the data
image(Groceries)
Groceries.short = Groceries[1:1000,]
dim(Groceries.short)
image(Groceries.short, xlab = "Items (Products)", ylab = " Transactions")

# get those transactions with more than X products
#transactionInfo (Groceries[size(Groceries) > 40])

# item Frequency Plot
itemFrequencyPlot(Groceries, support=0.03, cex.names = 0.5)

# obtaining rules
rules = apriori (Groceries, parameter = list (support=0.009, confidence=0.55))
rules # gives 10 rules
summary(rules)
inspect(rules)

# filter the "best" 10 rules
myrules <- sort(rules, by = "lift")
inspect(myrules)

########################################################################
# Exercise: analyze the Groceries dataset
# 1. Changing the pre-processing
########################################################################


#1.1 Filter out items like Whole milk

Groceries_filtered = subset(Groceries, !items %in% c("whole milk"))
summary(Groceries_filtered)
itemFrequencyPlot(Groceries_filtered, support = 0.03, cex.names = 0.5)

#Obtaining rules
rules_without_wholemilk = apriori (Groceries_filtered, parameter = list (support=0.0022, confidence=0.60))
rules_without_wholemilk # gives 7 rules
summary(rules_without_wholemilk)
inspect(rules_without_wholemilk)
rules_without_wholemilk = sort(rules_without_wholemilk, by = "lift")
inspect(rules_without_wholemilk)


#1.3 Let's try removing NAs and filter out whole milk and other vegetables

Groceries_na = na.omit(Groceries)
dim(Groceries_na)
itemFrequencyPlot(Groceries_na, support=0.03, cex.names = 0.5)
Groceries_filtered = subset(Groceries_na, !items %in% c("whole milk","other vegetables"))

summary(Groceries_filtered)
itemFrequencyPlot(Groceries_filtered, support = 0.03, cex.names = 0.5)

#Obtaining rules
rules_without_wholemilk_vege = apriori (Groceries_filtered, parameter = list (support=0.0015, confidence=0.57))
rules_without_wholemilk_vege # gives 10 rules
summary(rules_without_wholemilk_vege)
inspect(rules_without_wholemilk_vege)
rules_without_wholemilk_vege = sort(rules_without_wholemilk_vege, by = "lift")
inspect(rules_without_wholemilk_vege)

# RHS newspaper
rules_without_wholemilk_vege = apriori (Groceries_filtered, parameter = list (support=0.001, confidence=0.57))
rules_without_wholemilk_vege # gives 52 rules
rules_newspaper <- subset(rules_without_wholemilk_vege, subset = rhs %in% "newspapers")
inspect(rules_newspaper)


########################################################################
# Exercise: analyze the Groceries dataset
# 2. Grouping some items
########################################################################

#2.1 Group some items
items_to_group <- c("whole milk", "UHT-milk", "condensed milk")
new_item_label <- "Milk"
Groceries_filtered2 <- subset(Groceries, items %in% items_to_group, replace = list(items = new_item_label))
itemFrequencyPlot(Groceries_filtered2, support=0.05, cex.names = 0.5)
itemFrequencyPlot(Groceries, support=0.05, cex.names = 0.5)


rules_group = apriori (Groceries_filtered2, parameter = list (support=0.11, confidence=0.60))
rules_group # gives 10 rules
summary(rules_group)
inspect(rules_group)
rules_grouped = sort(rules_group, by = "lift")
inspect(rules_grouped)

#2.2 Limiting the size of the LHS
rules2 <- apriori(Groceries_filtered2, parameter = list(support = 0.075, confidence = 0.65, minlen = 3))
summary(rules2)
inspect(rules2)

