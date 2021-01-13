##################################
# 1. LOAD DATA SET AND LIBRARIES
##################################

load("rda/df.rda")
#load('rda/dfLightVersion.rda')

# loaf the variable iteraction 
load("rda/models_AIC_LRT.rda")

# load functions

source('functions/draw_functions.R')


library(MASS)
library(ggplot2)
library(caret)
library(dplyr)

##################################
# 2.  SPLIT TRAIN AND TESTING
##################################


set.seed(42)
trainRowNos <- createDataPartition(df$RESPONSE, p = 0.8, list = FALSE)
trainData <- df[trainRowNos,]
testData <- df[-trainRowNos,]
rm(df,trainRowNos)


###
# Naive_Model Credit_History
# Base_Line: with interactions

## Improvements
## Ridge
## AIC
## LRT

names(trainData)

##################################
# DEFINE THE VARIABLES AND INTERACTIONS USING AIC/LRT
##################################

# USE ALL THE TRAINDATA FOR FIXING AIC/LRT

models_AIC_LRT <- list()

model_baseline <-  glm(RESPONSE ~ .,
                       family = 'binomial',
                       trainData)


## AIC

#model_AIC <- stepAIC(model_baseline, 
#                     scope = . ~ .^2, 
#                     direction = 'both',
#                     trace = FALSE)

#models_AIC_LRT[[1]] <- model_AIC
model_AIC <- models_AIC_LRT[[1]]

model_AIC

## LRT

p <- 0.05

model_LRT <- stepAIC(model_baseline, 
                     scope = . ~ .^2, 
                     direction = 'both',
                     k = qchisq(1-p,df = 1),
                     trace = FALSE)

#models_AIC_LRT[[2]] <- model_LRT

model_LRT <- models_AIC_LRT[[2]]
model_LRT

#save(models_AIC_LRT, file = "rda/models_AIC_LRT.rda")

#load("rda/models_AIC_LRT.rda")


##################################
# TRAINING
# WITH THE DEFINED PARAMETERS, CREATE THE FOUR MODELS (USING CROSS-VALIDATION)
##################################

##*******************************
## Model 0-1 (Naive-Baseline)
##*******************************

# control (for all iteractions)  # always use 5/10 for calibration models (1/5 are for playing)

ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 5,
                     number = 10)


model1_baseline <- train(RESPONSE ~ ., 
                      method = "glm",
                      #tuneGrid = expand.grid(alpha = seq(0, 1, 0.1), lambda = seq(0, .1, 0.02)),
                      metric = "Kappa",
                      data = trainData,
                      preProcess = c("center", "scale"),
                      trControl = ctrl)

##*******************************
## Model 2: Ridge
##*******************************

model2_ridge <- train(RESPONSE ~ ., 
               method = "glmnet",
               #tuneGrid = expand.grid(alpha = seq(0, 1, 0.1), lambda = seq(0, .1, 0.02)),
               metric = "Kappa",
               data = trainData,
               preProcess = c("center", "scale"),
               trControl = ctrl)

##*******************************
## Model 3: Fitted with AIC (DID CONVERGE)
##*******************************

models_AIC_LRT[[1]]

model_AIC$call


# convergence problems
model3_AIC <- train(RESPONSE ~ DURATION + AMOUNT + INSTALL_RATE + AGE + 
                        NUM_CREDITS + NUM_DEPENDENTS + CHK_ACCT + HISTORY + PURPOSE_CREDIT + 
                        SAV_ACCT + EMPLOYMENT + MALE_STATUS + GUARANTEES + PRESENT_RESIDENT + 
                        PROP_RSTATE + OTHER_INSTALL + RESIDENCE + TELEPHONE + FOREIGN + 
                        NUM_CREDITS:SAV_ACCT + EMPLOYMENT:RESIDENCE + CHK_ACCT:PROP_RSTATE + 
                        MALE_STATUS:PRESENT_RESIDENT + AGE:PRESENT_RESIDENT + DURATION:PROP_RSTATE + 
                        DURATION:NUM_CREDITS + EMPLOYMENT:MALE_STATUS + MALE_STATUS:RESIDENCE + 
                        AMOUNT:PROP_RSTATE + HISTORY:SAV_ACCT + INSTALL_RATE:PRESENT_RESIDENT + 
                        HISTORY:PRESENT_RESIDENT + AMOUNT:TELEPHONE + MALE_STATUS:PROP_RSTATE + 
                        CHK_ACCT:OTHER_INSTALL + SAV_ACCT:EMPLOYMENT + SAV_ACCT:GUARANTEES + 
                        CHK_ACCT:MALE_STATUS + NUM_DEPENDENTS:CHK_ACCT + GUARANTEES:RESIDENCE + 
                        HISTORY:EMPLOYMENT + PURPOSE_CREDIT:PRESENT_RESIDENT + NUM_DEPENDENTS:EMPLOYMENT + 
                        AMOUNT:PURPOSE_CREDIT + AMOUNT:CHK_ACCT + INSTALL_RATE:NUM_CREDITS, 
                      method = "glm",
                      #tuneGrid = expand.grid(alpha = seq(0, 1, 0.1), lambda = seq(0, .1, 0.02)),
                      metric = "Kappa",
                      data = trainData,
                      preProcess = c("center", "scale"),
                      trControl = ctrl)


model3_AIC <- glm(formula = RESPONSE ~ DURATION + AMOUNT + INSTALL_RATE + AGE + 
      NUM_CREDITS + NUM_DEPENDENTS + CHK_ACCT + HISTORY + PURPOSE_CREDIT + 
      SAV_ACCT + EMPLOYMENT + MALE_STATUS + GUARANTEES + PRESENT_RESIDENT + 
      PROP_RSTATE + OTHER_INSTALL + RESIDENCE + TELEPHONE + FOREIGN + 
      NUM_CREDITS:SAV_ACCT + EMPLOYMENT:RESIDENCE + CHK_ACCT:PROP_RSTATE + 
        MALE_STATUS:PRESENT_RESIDENT + AGE:PRESENT_RESIDENT + DURATION:PROP_RSTATE +
        DURATION:NUM_CREDITS  + EMPLOYMENT:MALE_STATUS + MALE_STATUS:RESIDENCE + 
        AMOUNT:PROP_RSTATE + HISTORY:SAV_ACCT, 
    family = "binomial", 
    data = trainData)


summary(model3_AIC)

##*******************************
# Model 4: Fitted with LRT
##*******************************
model_LRT$call

# IT WORKS FINE:
model_LRT <- glm(formula = RESPONSE ~ DURATION + AMOUNT + INSTALL_RATE + NUM_DEPENDENTS + 
      CHK_ACCT + SAV_ACCT + GUARANTEES + PRESENT_RESIDENT + PROP_RSTATE + 
      OTHER_INSTALL + RESIDENCE + TELEPHONE + AMOUNT:TELEPHONE + 
      NUM_DEPENDENTS:TELEPHONE + INSTALL_RATE:PRESENT_RESIDENT + 
      INSTALL_RATE:PROP_RSTATE + NUM_DEPENDENTS:GUARANTEES, family = "binomial", 
    data = trainData)

summary(model_LRT)

model4_LRT <- train(RESPONSE ~ DURATION + AMOUNT + INSTALL_RATE + NUM_DEPENDENTS + 
                      CHK_ACCT + SAV_ACCT + GUARANTEES + PRESENT_RESIDENT + PROP_RSTATE + 
                      OTHER_INSTALL + RESIDENCE + TELEPHONE + AMOUNT:TELEPHONE + 
                      NUM_DEPENDENTS:TELEPHONE + INSTALL_RATE:PRESENT_RESIDENT + 
                      INSTALL_RATE:PROP_RSTATE + NUM_DEPENDENTS:GUARANTEES, 
                      method = "glm",
                      #tuneGrid = expand.grid(alpha = seq(0, 1, 0.1), lambda = seq(0, .1, 0.02)),
                      metric = "Kappa",
                      data = trainData,
                      preProcess = c("center", "scale"),
                      trControl = ctrl)


# save the calibrated models

calibrated_models <- list()
calibrated_models[[1]] <- model1_baseline
calibrated_models[[2]] <- model2_ridge
calibrated_models[[3]] <- model4_LRT

save(calibrated_models, file = "rda/calibrated_models.rda")

################################################
## predicting power training set
###############################################

## importance of each variable
model1_baseline_imp <- varImp(model1_baseline, scale = F)
plot(model1_baseline_imp, scales = list(y = list(cex = .95)), main = "Feature importanve varibles")


##*******************************
# Model 1. bASElINE
##*******************************
model1_baseline.Pred.train <- predict(model1_baseline, trainData)
cm.model1_baseline.train <- confusionMatrix(model1_baseline.Pred.train, trainData$RESPONSE)




##*******************************
# Model 2. ridge
##*******************************
model2_ridge.Pred.train <- predict(model2_ridge, trainData)
cm.model2_ridge.train <- confusionMatrix(model2_ridge.Pred.train, trainData$RESPONSE)


##*******************************
## Model 3. AIC
##*******************************
model3_AIC.Pred.train <- predict(model3_AIC, trainData)
cm.model3_AIC.train <- confusionMatrix(model3_AIC.Pred.train, trainData$RESPONSE)





##*******************************
## Model 4. LRT
##*******************************
model4_LRT.Pred.train <- predict(model4_LRT, trainData)
cm.model4_LRT.train <- confusionMatrix(model4_LRT.Pred.train, trainData$RESPONSE)



draw_confusion_matrix(cm = cm.model1_baseline.train, Class1 = "Bad", Class2 = "Good",title_def = 'Confusion Matrix: Baseline')
draw_confusion_matrix(cm = cm.model2_ridge.train, Class1 = "Bad", Class2 = "Good",title_def = 'Confusion Matrix: Ridge')
draw_confusion_matrix(cm = cm.model4_LRT.train, Class1 = "Bad", Class2 = "Good",title_def = 'Confusion Matrix: Subset LRT')




##################################
# TESTING
# 
##################################

##*******************************
## Model 1: Baseline
##*******************************


model1_baseline.Pred.test <- predict(model1_baseline, testData)
cm.model1_baseline.test <- confusionMatrix(model1_baseline.Pred.test, testData$RESPONSE)


##*******************************
## Model 2: Ridge
##*******************************

model2_ridge.Pred.test <- predict(model2_ridge, testData)
cm.model2_ridge.test <- confusionMatrix(model2_ridge.Pred.test, testData$RESPONSE)

##*******************************
# Model 4: Fitted with LRT
##*******************************

model4_LRT.Pred.test <- predict(model4_LRT, testData)
cm.model4_LRT.test <- confusionMatrix(model4_LRT.Pred.test, testData$RESPONSE)

draw_confusion_matrix(cm = cm.model1_baseline.test, Class1 = "Bad", Class2 = "Good", title_def = 'Confusion Matrix: Ridge Test')
draw_confusion_matrix(cm = cm.model2_ridge.test, Class1 = "Bad", Class2 = "Good", title_def = 'Confusion Matrix: Ridge Test')
draw_confusion_matrix(cm = cm.model4_LRT.test, Class1 = "Bad", Class2 = "Good", title_def = 'Confusion Matrix: LRT Test')




