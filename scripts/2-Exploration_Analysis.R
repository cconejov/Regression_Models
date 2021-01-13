load("rda/df.rda")
#load('rda/dfLightVersion.rda')

#######################
#1. structure
#######################

anyNA(df)

str(df)
summary(df)
glimpse(df)
table(df$RESPONSE)


# 1 Family of variables (Factors)

## 1.a Applicants Social Background
AGE
NUM_DEPENDENTS
MALE_STATUS
PRESENT_RESIDENT
FOREIGN
JOB
TELEPHONE

## 1.b Applicants Economic background
CHK_ACCT
SAV_ACCT
EMPLOYMENT
PROP_RSTATE
RESIDENCE

## 1.c Credit Product
DURATION
AMOUNT
INSTALL_RATE
PURPOSE_CREDIT
GUARANTEES

## 1.d Credit History
HISTORY
NUM_CREDITS
OTHER_INSTALL


# good credit: 1 (700)
# bad credit: 0 (300)

## credit scoring rule that can be used to determine if a new applicant is a good credit risk or a bad credit risk,
 
library(caret)
library(dplyr)
library(ggplot2)
library(GGally)
library(skimr)

############################
# 2. exploratory analysis
############################
set.seed(42)
trainRowNos <- createDataPartition(df$RESPONSE, p = 0.8, list = FALSE)
trainData <- df[trainRowNos,]
testData <- df[-trainRowNos,]
rm(df,trainRowNos)


#skimmed <- skim(trainData)



######################################
## 2.1 Continuous vs categorical
######################################




colnames(trainData)


# All plots (do it beautiful)
featurePlot(x = trainData[,c(1:5)], 
            y = trainData$RESPONSE, 
            plot = "box",
            strip = strip.custom(par.strip.text = list(cex = 0.7)), 
            scales = list(x = list(relation = "free"),
                          y = list(relation = "free")
            )
)

## anova test for relation between categorical/quantitative
# https://community.rstudio.com/t/strength-of-the-correlation-between-a-qualitative-and-a-quantitative-variable/20437

# Correlations between the continuous variables
ggcorr(trainData[,c(1:5)], label = T)

######################################
## 2.2 Categorical vs categorical
######################################

### a) HISTORY
#ggplot(trainData, aes(x=RESPONSE, fill = HISTORY)) + geom_bar()

### a)
ggplot(trainData, aes(x=HISTORY,fill = RESPONSE)) + geom_bar()
# a long-term contract (1-2 years) decreases chance of leaving

ggplot(trainData,aes(x=HISTORY,fill= RESPONSE)) +
  geom_bar(stat="count") +
  labs(title = "Distribution of Credit History") +
  scale_fill_manual(values=c("red3", "steelblue")) +
  theme_minimal()


ggplot(trainData,aes(x=NUM_CREDITS,fill= RESPONSE)) +
  geom_bar(stat="count") +
  labs(title = "Distribution of Number of Credit") +
  scale_fill_manual(values=c("red3", "steelblue")) +
  theme_minimal()





### b) EMPLOYMENT
ggplot(trainData, aes(x=EMPLOYMENT,fill = RESPONSE)) + geom_bar()
# a long-term contract (1-2 years) decreases chance of leaving

### c) SAV_ACCT
ggplot(trainData, aes(x=SAV_ACCT,fill = RESPONSE)) + geom_bar()
# a long-term contract (1-2 years) decreases chance of leaving

### d) CHK_ACCT 
ggplot(trainData, aes(x=CHK_ACCT,fill = RESPONSE)) + geom_bar()
# a long-term contract (1-2 years) decreases chance of leaving


#***********************************
## Categorical variables (Response and predictor): Chi-square

ggplot(trainData, aes(x=CHK_ACCT,fill = as.factor(RESPONSE))) + geom_bar()

ggplot(trainData, aes(x=RESPONSE,fill = CHK_ACCT)) + geom_bar()


tbl_CHK_ACCT <- table(df$RESPONSE, df$CHK_ACCT)


prop.table(tbl_CHK_ACCT)
#Marginal
rowSums(tbl_CHK_ACCT)
colSums(tbl_CHK_ACCT)

#Joint probability distribution
prop.table(tbl_CHK_ACCT)*100


# check account 0,1 is greater for bad credit
prop.table(table(df$CHK_ACCT, df$RESPONSE), margin=2)*100


# reference:
## https://www.datacamp.com/community/tutorials/contingency-analysis-r
## https://www.datacamp.com/community/tutorials/contingency-tables-r

#H0:two variables are independent
#H1:two variables are not independent

# p-value < alpha: Reject HO (Variable are independent)
# Conclusion: there is a statistically significant relationship between the two categorical variables
##             ie. they are not independent

chisq.test(df$CHK_ACCT, df$RESPONSE)


#***********************************



######################################
# 4 . Benchmark
######################################

## 4.1 Set control
ctrl <- trainControl(method = "repeatedcv", number = 5,
                     classProbs = TRUE, 
                     verboseIter=T)

## 4.2 Fit the model on training We have many predictors, hence use penalized logistic regression
lrFit <- train(RESPONSE ~ ., 
               method = "glmnet",
               #tuneGrid = expand.grid(alpha = seq(0, 1, 0.1), lambda = seq(0, .1, 0.02)),
               metric = "Kappa",
               data = trainData,
               preProcess = c("center", "scale"),
               trControl = ctrl)
print(lrFit)


## 4.3 Testing the fitted values
lrPred = predict(lrFit, testData)
confusionMatrix(lrPred, testData$RESPONSE)


table(testData$RESPONSE)
table(lrPred)


## 4.4 Variable importance
lr_imp <- varImp(lrFit, scale = F)
plot(lr_imp, scales = list(y = list(cex = .95)))


