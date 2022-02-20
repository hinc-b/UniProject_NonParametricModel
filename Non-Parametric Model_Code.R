#*****************************************************************************#
# Classification trees                                                        #
#*****************************************************************************#
dane <- read.xlsx(xlsxFile="bank.xlsx", 
                  sheet = "churn")
library(readxl)
library(tree)
library(rpart)
library(rpart.plot)

# change to factor / numeric
dane$Exited=as.factor(dane$Exited)
dane$Balance=as.numeric(dane$Balance)
dane$EstimatedSalary=as.numeric(dane$EstimatedSalary)
dane$Geography=as.factor(dane$Geography)
dane$Gender=as.factor(dane$Gender)

#*****************************************************************************#
# Charts                                                                     #
#*****************************************************************************#

# CreditScore
summary(dane$CreditScore)
boxplot(dane$CreditScore, col="blue",horizontal = TRUE)
# Geography
table(dane$Geography)
barplot(table(dane$Geography),las=1)
# Gender
table(dane$Gender)
barplot(table(dane$Gender),las=1)
# Age
summary(dane$Age)
boxplot(dane$Age, col="blue",horizontal = TRUE)
# Tenure
summary(dane$Tenure)
boxplot(dane$Tenure, col="blue",horizontal = TRUE)
# Balance
summary(dane$Balance)
boxplot(dane$Balance, col="blue",horizontal = TRUE)
# NumOfProducts
summary(dane$NumOfProducts)
boxplot(dane$NumOfProducts, col="blue",horizontal = TRUE)
# HasCrCard
table(dane$HasCrCard)
barplot(table(dane$HasCrCard),las=1)
# IsActiveMember
table(dane$IsActiveMember)
barplot(table(dane$IsActiveMember),las=1)
# EstimatedSalary
summary(dane$EstimatedSalary)
boxplot(dane$EstimatedSalary, col="blue",horizontal = TRUE)
# Exited
table(dane$Exited)
barplot(table(dane$Exited),las=1)


#*****************************************************************************#
# Further part                                                                #
#*****************************************************************************#
# Division into sets
table(dane$Exited)

testowe.nr <- sample(x = nrow(dane), size = nrow(dane)/4, replace = F)

dane.ucz <- dane[-testowe.nr, ]
dane.test <- dane[testowe.nr, ]

### Construction of a classification tree (target variable: Exited)
drzewo.dane <- rpart(Exited ~ CreditScore + Geography + Gender + Age + Tenure + 
                       Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary, 
                     data = dane.ucz)
drzewo.dane$cptable

rpart.rules(drzewo.dane, style = "tallw")
summary(drzewo.dane)

# Drawing a tree diagram
rpart.plot(drzewo.dane, box.palette = "Blues")

### Sequence based on cross validation
bledy <- printcp(drzewo.dane) # tree sequence for optimal pruning

matplot(x = bledy[, "nsplit"],
        y = bledy[, c("rel error",  # training sample error (relative to error for root)
                      "xerror")],  # cross validation error
        type = "l",
        xlab = "tree size",
        ylab = "error")
legend(x = "topright", legend = c("training sample error", 
                                  "cross validation error"),
       col = c("black", "red"),
       lty = 1:2)

tmp1 <- which.min(bledy[, "xerror"])  # min error in cross validation
tmp2 <- sum(bledy[tmp1, c("xerror", "xstd")]) # min error + standard deviation
optymalny <- which(bledy[, "xerror"] < tmp2)[1] # optimal tree number

drzewo.dane.p <- prune(drzewo.dane, cp = bledy[optymalny, "CP"]) # tree pruning
plot(drzewo.dane.p)



### Classification error in the test set
predykcja <- predict(object = drzewo.dane.p,
                     newdata = dane.test,
                     type = "class")
sum(predykcja != dane.test$Exited) / nrow(dane.test)  # Classification error

### Misclassification matrix
table(rzeczywiste = dane.test$Exited, predykcja)



# Determining the optimal parameters of the model
drzewo.dane$cptable

drzewo.dane <- rpart(Exited ~ CreditScore + Geography + Gender + Age + Tenure + 
                       Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary, 
                     data = dane.ucz,
                     control = rpart.control(cp = 0)) # cp argument , it will build the maximal tree
drzewo.dane$cptable # re-executing cptable after building the max tree

# I check the smallest xerror and xstd
0.6965699 + 0.01986918 # the values change (not much) because there is a random division into training and test sets

drzewo.dane <- prune(drzewo.dane, cp = 0.005) # pruning
drzewo.dane$cptable

rpart.plot(drzewo.dane, box.palette = "Blues")

# Readout of importance of variables
cbind(drzewo.dane$variable.importance)

dotchart(rev(drzewo.dane$variable.importance),
         pch = 16, lcolor = "blue", main = "Waznosc zmiennych")

# Checking the model accuracy on a test set
predykcja <- predict(drzewo.dane, newdata = dane.test, type = "class")
predykcja[1:100]

mean(dane.test$Exited != predykcja) # classification error

table(rzeczywiste = dane.test$Exited, predykcja)

predykcje.prob <- predict(drzewo.dane, newdata = dane.test, type = "prob" )
predykcje.prob[1:10, ]


#*****************************************************************************#
# Classification trees - the multi-model approach                             #
#*****************************************************************************#
library(adabag)  # (boosting and bagging)
library(randomForestSRC)  # (random forest)
library(xgboost)  # XGBoost


# Dividing the data randomly into the training, validation and test sets
grupy <- sample(x = c("ucz¹cy", "walidacyjny", "testowy"), 
                size = nrow(dane),
                replace = T,
                prob = c(0.6, 0.2, 0.2))

dane.ucz <- dane[grupy == "ucz¹cy",]
dane.walid <- dane[grupy == "walidacyjny",]
dane.test <- dane[grupy == "testowy",]

#*****************************************************************************#
# Building the BAGGING model                                                  #
#*****************************************************************************#
m.bagg <- bagging(Exited ~ CreditScore + Geography + Gender + Age + Tenure + 
                    Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary, 
                  data = dane.ucz, mfinal = 10)

summary(m.bagg)

# Analyzing the distribution of votes
class(m.bagg$votes)
dim(m.bagg$votes) # the size of the matrix
head(m.bagg$votes) # start of dataset
levels(dane.ucz$Exited)

table(m.bagg$votes[, 2]) # votes for yes by unit
table(m.bagg$votes[, 2], dane.ucz$Exited)

barplot(table(dane.ucz$Exited, m.bagg$votes[, 2]),
        legend = T,
        xlab = "G³osy na 'Klasê 1'",
        ylab = "Liczba jednostek",
        args.legend = list(title = "Faktyczna klasa"))

# Readout of importance of variables
m.bagg$importance
dotchart(sort(m.bagg$importance),
         pch = 16, lcolor = "blue", main = "Waznosc zmiennych")
cbind(sort(m.bagg$importance))

# Calculation of model error using cross-validation
m.bagg.cv <- bagging.cv(Exited ~ CreditScore + Geography + Gender + Age + Tenure + 
                          Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary, 
                        data = dane.ucz, mfinal = 10, v = 5)
m.bagg.cv$confusion
m.bagg.cv$error

## Drawing error charts (on training and validation sets) depending on the number of trees
plot.errorevol(errorevol(m.bagg, dane.walid),
               errorevol(m.bagg, dane.ucz))
grid(ny = 10, nx = 0)

## Determining the optimal number of base models
# It can be determined on the basis of the graph, whether the model, for example, is overfitting, if so, the error on the validation set grows

# Checking the model accuracy on a test set
predykcje.bagg <- predict(m.bagg, newdata = dane.test)
predykcje.bagg$confusion
predykcje.bagg$error

#*****************************************************************************#
# Building a BOOSTING model                                                   #
#*****************************************************************************#
m.boost <- boosting(Exited ~ CreditScore + Geography + Gender + Age + Tenure + 
                      Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary, 
                    data = dane.ucz, mfinal = 10)

# Analyzing voice distribution
head(m.boost$votes)
hist(m.boost$votes[,2], 50)

hist(m.boost$votes[dane.ucz$Exited == "1",2],30,
     freq = F,
     col = "grey20", 
     main = "Histogram of", 
     xlim = c(0,3), ylim = c(0,1),
     xlab = "Glosy na 'Klasê 1'", ylab = "Gestosc")

hist(m.boost$votes[dane.ucz$Exited == "0",2],30, 
     add = T,
     freq = F)

abline(v = sum(m.boost$weights)/2)

legend(x = "topright", legend = c("Rzeczywiste '1'", "Rzeczywiste '0'"),
       fill = c("grey20", "lightgrey"))

m.boost$class[1:10]

predykcje.boost.ucz <- predict(m.boost, newdata = dane.ucz)
predykcje.boost.ucz$error
predykcje.boost.ucz$confusion

# Readout of importance of the variables
m.boost$importance

dotchart(sort(m.boost$importance),
         pch = 16, lcolor = "blue", main = "Waznosc zmiennych")

cbind(sort(m.boost$importance))

# Drawing error charts (on training and validation sets) depending on the number of trees
plot.errorevol(errorevol(m.boost, dane.walid),
               errorevol(m.boost, dane.ucz))

grid(ny = 10, nx = 0)

# Checking the model accuracy on a test set
predykcje.boost <- predict(m.boost, newdata = dane.test)
predykcje.boost$error
predykcje.boost$confusion


#*****************************************************************************#
# Building a RANDOM FOREST model                                              #
#*****************************************************************************#
m.rf <- rfsrc(Exited ~ CreditScore + Geography + Gender + Age + Tenure + 
                Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary, 
              data = dane.ucz,
              block.size = 1,
              importance = T,
              ntree = 100)
plot(m.rf)

# Readout of importance of the variables
head(m.rf$importance)
m.rf$importance

dotchart(sort(m.rf$importance))

cbind(sort(m.rf$importance))

# Determining the optimal number of base models
predykcje.rf.walid <- predict(m.rf, newdata = dane.walid,
                              block.size = 1)
predykcje.rf.walid$err.rate


matplot(predykcje.rf.walid$err.rate, 
        type = "l", 
        lty = 1)
grid(ny = 10, nx = 0)

matplot(m.rf$err.rate, 
        type = "l",
        lty = 2, 
        add = T)

# Checking the model accuracy on a test set
predykcje.rf <- predict(m.rf, newdata = dane.test)
predykcje.rf$err.rate

# Aggregation of all models
glosy <- cbind(predict (m.bagg, newdata = dane.test)$class,
               predict (m.boost, newdata = dane.test)$class,
               as.character (predict (m.rf, newdata = dane.test)$class))

head(glosy)

predykcja.razem <- apply (glosy, 1,
                          function(x) names (sort (table (x), decreasing = T) [1]))

mean(predykcja.razem != dane.test$Exited)



#*****************************************************************************#
# GAM - generalized additive models                                           #
#*****************************************************************************#
library(mgcv)


 #### MODEL 1 ####
gam1 <- gam(EstimatedSalary ~ s(CreditScore) + s(Balance) + Age, # the function s () is a smoothing function
            data = dane)
gam1

gam.check(gam1) # checks if the parameter 'k' is well matched
# a graph is generated as well - if it is exactly 45 degrees it will be very good

## the most important statistics that summarize the model
summary(gam1)

## drawing partial residuals chart
plot(gam1, pages = 1,
     residuals = T,
     scheme = 1,
     all.terms = T)

plot(gam1, select = 1,
     residuals = F,
     scheme = 1)

plot(gam1, select = 2,
     residuals = T,
     scheme = 1)

## Save the values of the measures of adjustment (AIC, GCV)
gam1$gcv.ubre
gam1$aic # the smaller the measures the better

 #### LINEAR MODEL ####
 #### Linear model estimation with the same variables as in Model 1 (Model 1a)
liniowy1 <- gam(EstimatedSalary ~ CreditScore + Balance + Age,
                data = dane)
summary(liniowy1)

plot(liniowy1, pages = 1,
     residuals = T,
     scheme = 1,
     all.terms = T)

plot(liniowy1, select = 1,
     scheme = 1,
     all.terms = T)

## calculate coefficient of determination (AIC, GCV) of linear model and  a comparison with measures for Model 1.
liniowy1$gcv.ubre
liniowy1$aic # the linear model is worse in terms of predictive abilities because its values are higher than the values of model 1.

## Compare the prediction accuracy of the linear model and Model 1 by dividing the data set
#  to training and test sample
testowe.nr <- sample(nrow(dane), nrow(dane)/4)
dane.test <- dane[testowe.nr, ]
dane.ucz <- dane[-testowe.nr, ]

gam1 <- gam(EstimatedSalary ~ s(CreditScore) + s(Balance) + Age,
            data = dane.ucz)
liniowy1 <- gam(EstimatedSalary ~ CreditScore + Balance + Age,
                data = dane.ucz)

# mean square error - simple comparison. The linear model is still the weaker model
mean((predict(gam1, newdata = dane.test) - dane.test$EstimatedSalary)^2)
mean((predict(liniowy1, newdata = dane.test) - dane.test$EstimatedSalary)^2)

 #### MODEL 2 ####
 #### Model 2 estimation and quality evaluation
gam2 <- gam(EstimatedSalary ~ te(CreditScore, Balance) + Age,
            data = dane)

summary(gam2)
gam2$gcv.ubre

plot(gam2, pages = 1,
     residuals = T,
     scheme = 1,
     all.terms = T)

plot(gam2, select = 1,
     residuals = T,
     scheme = 1)

library(manipulate)

manipulate({
  plot(gam2, select = 1,
       residuals = T,
       scheme = 1,
       theta = poziom, phi = pion)},
  poziom = slider(min = -180, max = 180, initial = 0, step = 10),
  pion = slider(min = -180, max = 180, initial = 0, step = 10))

vis.gam(gam2, view = c("Balance", "CreditScore"),cond = list(x0=.75),theta = 200, phi = 30, too.far=.07)