# 1) Load libraries

library(readxl)
library(dplyr)

# 2) Load Dataset

# a) xls table
df <- read_excel("data/credit.xls",
                      sheet = 1)

# Delete observation variable
df <- df[,-1]

# Change column name
colnames(df) <- c("CHK_ACCT","DURATION","HISTORY","NEW_CAR","USED_CAR","FURNITURE","RADIO_TV","EDUCATION","RETRAINING","AMOUNT","SAV_ACCT","EMPLOYMENT","INSTALL_RATE","MALE_DIV","MALE_SINGLE","MALE_MAR_WID","CO_APPLICANT","GUARANTOR","PRESENT_RESIDENT","REAL_ESTATE","PROP_UNKN_NONE","AGE","OTHER_INSTALL","RENT","OWN_RES","NUM_CREDITS","JOB","NUM_DEPENDENTS","TELEPHONE","FOREIGN","RESPONSE")

# Structure / Null values
anyNA(df)
str(df)

summary(df$CHK_ACCT)
table(df$CO_APPLICANT)

df <-  df %>% 
  mutate(CHK_ACCT = factor(CHK_ACCT, levels = c(0,1,2,3)),
         HISTORY = factor(HISTORY, levels = c(0,1,2,3,4)),
         NEW_CAR = factor(ifelse(NEW_CAR == 0, "No","Yes"),levels = c("No","Yes")),
         USED_CAR = factor(ifelse(USED_CAR == 0, "No","Yes"), levels = c("No","Yes")),
         FURNITURE = factor(ifelse(FURNITURE == 0, "No","Yes"),levels = c("No","Yes")),
         RADIO_TV = factor(ifelse(RADIO_TV == 0, "No","Yes"),levels = c("No","Yes")),
         EDUCATION = factor(ifelse(EDUCATION == 0, "No","Yes"),levels = c("No","Yes")),
         RETRAINING = factor(ifelse(RETRAINING == 0, "No","Yes"),levels = c("No","Yes")),
         SAV_ACCT = factor(SAV_ACCT,levels = c(0,1,2,3,4)),
         EMPLOYMENT = factor(EMPLOYMENT,levels = c(0,1,2,3,4)),
         MALE_DIV = factor(ifelse(MALE_DIV == 0, "No","Yes"),levels = c("No","Yes")),
         MALE_SINGLE = factor(ifelse(MALE_SINGLE == 0, "No","Yes"),levels = c("No","Yes")),
         MALE_MAR_WID = factor(ifelse(MALE_MAR_WID == 0, "No","Yes"),levels = c("No","Yes")),
         CO_APPLICANT = factor(ifelse(CO_APPLICANT == 0, "No","Yes"),levels = c("No","Yes")),
         GUARANTOR = factor(ifelse(GUARANTOR == 0, "No","Yes"),levels = c("No","Yes")),
         PRESENT_RESIDENT = factor((PRESENT_RESIDENT-1),levels = c(0,1,2,3)),
         REAL_ESTATE = factor(ifelse(REAL_ESTATE == 0, "No","Yes"),levels = c("No","Yes")),
         PROP_UNKN_NONE = factor(ifelse(PROP_UNKN_NONE == 0, "No","Yes"),levels = c("No","Yes")),
         OTHER_INSTALL = factor(ifelse(OTHER_INSTALL == 0, "No","Yes"),levels = c("No","Yes")),
         RENT = factor(ifelse(RENT == 0, "No","Yes"),levels = c("No","Yes")),
         OWN_RES = factor(ifelse(OWN_RES == 0, "No","Yes"),levels = c("No","Yes")),
         JOB = factor(JOB,levels = c(0,1,2,3)),
         TELEPHONE = factor(ifelse(TELEPHONE == 0, "No","Yes"),levels = c("No","Yes")),
         FOREIGN = factor(ifelse(FOREIGN == 0, "No","Yes"),levels = c("No","Yes")),
         RESPONSE = factor(ifelse(RESPONSE == 0, "Bad","Good"),levels = c("Bad","Good"))
         )

anyNA(df)
summary(df)

## Statistics marital status

# Define a New column 
## 0 Other
## 1: SINGLE
## 2: Married/Widover
## 3: Divorced

df$MALE_STATUS <- factor(ifelse(df$MALE_SINGLE == "Yes" & 
                            df$MALE_MAR_WID == "No" & 
                            df$MALE_DIV == "No",
                            "SINGLE",
                      ifelse(df$MALE_SINGLE == "No" & 
                               df$MALE_MAR_WID == "Yes" & 
                               df$MALE_DIV == "No",
                             "MAR_WID",
                      ifelse(df$MALE_SINGLE == "No" & 
                               df$MALE_MAR_WID == "No" & 
                               df$MALE_DIV == "Yes",
                             "DIVORCED",
                             "OTHER"))),
                      levels = c("OTHER","SINGLE","MAR_WID", "DIVORCED")
                      )
df %>% count(MALE_SINGLE,MALE_MAR_WID,MALE_DIV,sort = TRUE)
summary(df$MALE_STATUS)

df$MALE_DIV <- NULL
df$MALE_MAR_WID <- NULL
df$MALE_SINGLE <- NULL


### Statistics Car
#df %>% count(NEW_CAR,USED_CAR,sort = TRUE)
#df$CAR = factor(ifelse(df$NEW_CAR == "No" & df$USED_CAR == "No", 
#                             "No",
#                             ifelse(df$NEW_CAR == "Yes" & df$USED_CAR == "No",
#                                    "New",
#                                    "Used")), 
#                      levels = c("No","New","Used"))

#df$NEW_CAR <- NULL
#df$USED_CAR <- NULL



# Purpose of Credit

df$PURPOSE_CREDIT <- factor(ifelse(df$NEW_CAR == "Yes" & 
                                   df$USED_CAR == "No" &
                                   df$FURNITURE == "No" &
                                   df$RADIO_TV == "No" &
                                   df$EDUCATION == "No" &
                                   df$RETRAINING == "No",
                                   "NEW_CAR",
                             ifelse(df$NEW_CAR == "No" & 
                                    df$USED_CAR == "Yes" &
                                    df$FURNITURE == "No" &
                                    df$RADIO_TV == "No" &
                                    df$EDUCATION == "No" &
                                    df$RETRAINING == "No",
                                    "USED_CAR",
                            ifelse(df$NEW_CAR == "No" & 
                                   df$USED_CAR == "No" &
                                   df$FURNITURE == "Yes" &
                                   df$RADIO_TV == "No" &
                                   df$EDUCATION == "No" &
                                   df$RETRAINING == "No",
                                   "FURNITURE",
                            ifelse(df$NEW_CAR == "No" & 
                                   df$USED_CAR == "No" &
                                   df$FURNITURE == "No" &
                                   df$RADIO_TV == "Yes" &
                                   df$EDUCATION == "No" &
                                   df$RETRAINING == "No",
                                   "RADIO_TV",
                            ifelse(df$NEW_CAR == "No" & 
                                   df$USED_CAR == "No" &
                                   df$FURNITURE == "No" &
                                   df$RADIO_TV == "No" &
                                   df$EDUCATION == "Yes" &
                                   df$RETRAINING == "No",
                                   "EDUCATION",
                            ifelse(df$NEW_CAR == "No" & 
                                   df$USED_CAR == "No" &
                                   df$FURNITURE == "No" &
                                   df$RADIO_TV == "No" &
                                   df$EDUCATION == "No" &
                                   df$RETRAINING == "Yes",
                                   "RETRAINING", 
                                   "OTHER")))))),
                            levels = c("OTHER","RADIO_TV", "NEW_CAR", "USED_CAR","FURNITURE", "EDUCATION", "RETRAINING")
                            )

summary(df$PURPOSE_CREDIT)

df %>% count(NEW_CAR, USED_CAR, FURNITURE,RADIO_TV,EDUCATION,RETRAINING ,sort = TRUE)

df$NEW_CAR <- NULL
df$USED_CAR <- NULL
df$FURNITURE <- NULL
df$RADIO_TV <- NULL
df$EDUCATION <- NULL
df$RETRAINING  <- NULL

## Statistics Car

df$GUARANTEES <- factor(ifelse(df$GUARANTOR == "Yes" & df$CO_APPLICANT == "No",
                              "GUARANTOR",
                       ifelse(df$GUARANTOR == "No" & df$CO_APPLICANT == "Yes",
                              "CO_APPLICANT",
                              "NONE")),
                       levels = c("NONE", "CO_APPLICANT", "GUARANTOR")
                       )



summary(df$GUARANTEES)

df %>% count(CO_APPLICANT,GUARANTOR,sort = TRUE)

df$CO_APPLICANT <- NULL
df$GUARANTOR <- NULL

###### Real/STATE_PROP_UNKNOW

df$PROP_RSTATE <- factor(ifelse(df$REAL_ESTATE == "Yes" & df$PROP_UNKN_NONE == "No",
                               "OWNS_RS",
                               ifelse(df$REAL_ESTATE == "No" & df$PROP_UNKN_NONE == "Yes",
                                      "NO_OWNS_PROP",
                                      "OTHER")),
                        levels = c("OTHER", "NO_OWNS_PROP", "OWNS_RS")
)



summary(df$PROP_RSTATE)

df %>% count(REAL_ESTATE, PROP_UNKN_NONE,sort = TRUE)

df$REAL_ESTATE <- NULL
df$PROP_UNKN_NONE <- NULL

################ RENT/OWS_RESIDENCE

df$RESIDENCE <- factor(ifelse(df$RENT == "Yes" & df$OWN_RES == "No",
                                "RENT",
                                ifelse(df$RENT == "No" & df$OWN_RES == "Yes",
                                       "OWN_RESID",
                                       "OTHER")),
                         levels = c("OTHER", "RENT", "OWN_RESID")
)



summary(df$RESIDENCE)

df %>% count(RENT, OWN_RES,sort = TRUE)

df$RENT <- NULL
df$OWN_RES <- NULL

######
summary(df)







df <- df[,c("DURATION","AMOUNT","INSTALL_RATE","AGE","NUM_CREDITS","NUM_DEPENDENTS","CHK_ACCT","HISTORY","PURPOSE_CREDIT","SAV_ACCT","EMPLOYMENT","MALE_STATUS","GUARANTEES","PRESENT_RESIDENT","PROP_RSTATE","OTHER_INSTALL","RESIDENCE","JOB","TELEPHONE","FOREIGN","RESPONSE"
)]

summary(df)
rm(df2)
summary(df2)

save(df, file = "rda/df.rda")

## LIGHT VERSION: FOR CREATE ALL THE PROJECT

library(caret)
set.seed(42)
spl = createDataPartition(df$RESPONSE, p = 0.2, list = FALSE)  # 11% for evaluating
df <- df[spl,]
rm(spl)

summary(df)
save(df, file = 'rda/dfLightVersion.rda')


## code list
code_list <- read_excel("data/credit.xls",
                        sheet = 2)
