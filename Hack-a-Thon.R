setwd("~/Desktop/Hack-a-Thon/materials for public release") ## Set's Working Directory

library(tidyverse)
library(data.table)
library(haven)
library(rpart)

# Read Data
data <- read_sav("2017 Pew Research Center STEM survey.sav")

# Create Data Table
data.dt <- as.data.table(data)

# Split for men and women/check harassment history
men <- data.dt[PPGENDER == 1]
women <- data.dt[PPGENDER == 2]

nrow(men[HARASS3 == 1])
nrow(men[HARASS3 == 2])
nrow(men[HARASS3 == 9])

nrow(women[HARASS3 == 1])
nrow(women[HARASS3 == 2])
nrow(women[HARASS3 == 9])

# Remove NA's and No response
harassYN <- data.dt[HARASS2 == 1 | HARASS2 == 2 | HARASS2 == 3]

# Create Data sets for induvisual responses for PYTHON Code
harassOrder <- harassYN[order(HARASS2)]

h1 <- harassOrder[HARASS2 == 1, .(var1 = PPT017_t, var2 = HH_INCOME_col, var3 = IDEO, var4 = TECH3, var5 = TECH6)]
h2 <- harassOrder[HARASS2 == 2, .(var1 = PPT017_t, var2 = HH_INCOME_col, var3 = IDEO, var4 = TECH3, var5 = TECH6)]
h3 <- harassOrder[HARASS2 == 3, .(var1 = PPT017_t, var2 = HH_INCOME_col, var3 = IDEO, var4 = TECH3, var5 = TECH6)]

write.csv(h1,"harass1.csv")
write.csv(h2,"harass2.csv")
write.csv(h3,"harass3.csv")


# Create Tree Model with same Predictors as PYTHON Model
sameTree <- harassOrder[, .(var0 = HARASS2, var1 = PPT017_t, var2 = HH_INCOME_col, var3 = IDEO, var4 = TECH3, var5 = TECH6)]

x <- 1:nrow(sameTree)
testnum <- sample(x,nrow(sameTree)/5)

test <- sameTree[testnum]
train <- sameTree[!testnum]

unique(train$var0)

control <- rpart.control(minbucket = 1, cp = 0.001, maxsurrogate = 0, usesurrogate = 0, xval = 10)
FGL.tr <- rpart(var0 ~ ., train, method = "class", control = control)
plotcp(FGL.tr)
printcp(FGL.tr)  #same info in FGL.tr$cptable
#prune to optimal size
pruned <- prune(FGL.tr, cp=0.006)  #approximately the cp corresponding to the optimal size
pruned
par(cex=.5); plot(pruned, uniform=F); text(pruned, use.n = F); par(cex=2)
pruned$variable.importance
pruned$cptable[nrow(FGL.tr1$cptable),] 
#calculate training and CV misclass rates

pruned$cptable[nrow(pruned$cptable),] 
yhat<-predict(pruned, test, type="class"); sum(yhat != test$var0)/nrow(test) #check test misclass rate


FGL.tr1$cptable[nrow(FGL.tr1$cptable),c(3,4)]*min(table(FGL$type_bin)/nrow(FGL))  #training and cv misclass rates
yhat<-predict(FGL.tr1, type="class")
sum(yhat != FGL$type_bin)/nrow(FGL) #check the training misclass rate




