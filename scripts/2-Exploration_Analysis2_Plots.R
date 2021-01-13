load("rda/df.rda")
#load('rda/dfLightVersion.rda')

library(caret)
library(dplyr)
library(ggplot2)
library(GGally)

set.seed(42)
trainRowNos <- createDataPartition(df$RESPONSE, p = 0.8, list = FALSE)
trainData <- df[trainRowNos,]
testData <- df[-trainRowNos,]
rm(df,trainRowNos)


#######################
#1. variables family
#######################



# 1 Family of variables (Factors)

## 1.a Applicants Social Background
1AGE
2NUM_DEPENDENTS
3MALE_STATUS
4PRESENT_RESIDENT
5FOREIGN
6JOB
7TELEPHONE

## 1.b Applicants Economic background
8CHK_ACCT
9SAV_ACCT
0EMPLOYMENT
1PROP_RSTATE
2RESIDENCE

## 1.c Credit Product
3DURATION
4AMOUNT
5INSTALL_RATE
6PURPOSE_CREDIT
7GUARANTEES

## 1.d Credit History
8HISTORY
9NUM_CREDITS
0OTHER_INSTALL


colnames(testData)

# CREDIT HISTORY

ggplot(trainData,aes(x=HISTORY,fill= RESPONSE)) +
  geom_bar(stat="count") +
  labs(title = "Distribution of Credit History") +
  scale_fill_manual(values=c("red3", "steelblue")) +
  theme_minimal()

ggplot(trainData, aes(x=HISTORY, fill = RESPONSE)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent)  +
  labs(title = "Distribution of Credit History") +
  scale_fill_manual(values=c("red3", "steelblue")) +
  theme_minimal()


ggplot(trainData,aes(x=NUM_CREDITS,fill= RESPONSE)) +
  geom_bar(stat="count") +
  labs(title = "Distribution of Number of Credits") +
  scale_fill_manual(values=c("red3", "steelblue")) +
  theme_minimal()


#########################
## 1.b Applicants Economic background
ggplot(trainData,aes(x=CHK_ACCT,fill= RESPONSE)) +
  geom_bar(stat="count") +
  labs(title = "Distribution of CHK_ACCT") +
  scale_fill_manual(values=c("red3", "steelblue")) +
  theme_minimal()

ggplot(trainData,aes(x=SAV_ACCT,fill= RESPONSE)) +
  geom_bar(stat="count") +
  labs(title = "Distribution of SAV_ACCT") +
  scale_fill_manual(values=c("red3", "steelblue")) +
  theme_minimal()

ggplot(trainData, aes(x=SAV_ACCT, fill = RESPONSE)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent)  +
  labs(title = "Distribution of SAV_ACCT") +
  scale_fill_manual(values=c("red3", "steelblue")) +
  theme_minimal()


#######################
# SOCIAL BACKGROUND

ggplot(trainData,aes(x=PRESENT_RESIDENT,fill= RESPONSE)) +
  geom_bar(stat="count") +
  labs(title = "Distribution of Credit History") +
  scale_fill_manual(values=c("red3", "steelblue")) +
  theme_minimal()


ggplot(trainData, aes(x=PRESENT_RESIDENT, fill = RESPONSE)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent)  +
  labs(title = "Distribution of Credit History") +
  scale_fill_manual(values=c("red3", "steelblue")) +
  theme_minimal()
