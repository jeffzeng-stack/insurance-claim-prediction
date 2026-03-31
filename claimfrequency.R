install.packages("pROC")

library(ggplot2)
library(dplyr)
library(pROC)

#choose data set
data <- read.csv(file.choose())

#
data$CLAIM <- as.numeric(as.character(data$OUTCOME))                  


#convert variables to appropriate types 
data$AGE <- factor(
  data$AGE, 
  levels = c("16-25", "26-39", "40-64", "65+"),
)

data$GENDER <- factor(
  data$GENDER,
  levels = c("female", "male")
)

data$RACE <- as.factor(data$RACE)

data$DRIVING_EXPERIENCE <- factor(
  data$DRIVING_EXPERIENCE,
  levels = c("0-9y", "10-19y", "20-29y", "30y+"),
)

data$EDUCATION <- factor(
  data$EDUCATION,
  levels = c("none", "high school", "university")
)

data$INCOME <- factor(
  data$INCOME,
  levels = c("poverty", "working class", "middle class", "upper class"),
)

data$VEHICLE_OWNERSHIP <- factor(
  data$VEHICLE_OWNERSHIP,
  levels = c(0,1)
)

data$VEHICLE_YEAR <- factor(
  data$VEHICLE_YEAR,
  levels = c("before 2015", "after 2015")
)

data$MARRIED <- factor(
  data$MARRIED,
  levels = c(0,1)
)

data$CHILDREN <- as.numeric(data$CHILDREN)

data$VEHICLE_TYPE <- as.factor(data$VEHICLE_TYPE)

data$OUTCOME <- factor(
  data$OUTCOME,
  levels = c(0,1)
)

#replace missing values for credit_score and annual_mileage with median for each

data$CREDIT_SCORE[is.na(data$CREDIT_SCORE)] <-  median(data$CREDIT_SCORE, na.rm = TRUE)
data$ANNUAL_MILEAGE[is.na(data$ANNUAL_MILEAGE)] <- median(data$ANNUAL_MILEAGE, na.rm = TRUE)

#verify conversions and structure
str(data)
summary(data)

#summarize data and compare claim rates 
#between gender, vehicle type, and driving experience
table(data$OUTCOME)
mean(data$CLAIM)

#GENDER
data %>%
  group_by(GENDER) %>%
  summarise(ClaimRate = mean(CLAIM), Count = n())

ggplot(data,aes(x = GENDER, fill = OUTCOME)) +
  geom_bar(position = "fill") +
  labs(
    title = "Claim Proportion by Gender",
    x = "Gender",
    y = "Proportion"
  )

#VEHICLE TYPE
data %>%
  group_by(VEHICLE_TYPE) %>%
  summarise(ClaimRate = mean(CLAIM), Count = n())

ggplot(data, aes(x = VEHICLE_TYPE, fill = OUTCOME)) +
  geom_bar(position = "fill") +
  labs(
    title = "Claim Proportion by Vehicle Type",
    x = "Vehicle Type",
    y = "Proportion"
  )

#DRIVING EXPERIENCE
data %>%
  group_by(DRIVING_EXPERIENCE) %>%
  summarise(ClaimRate = mean(CLAIM), Count = n())

ggplot(data, aes(x = DRIVING_EXPERIENCE, fill = OUTCOME)) +
  geom_bar(position = "fill") +
  labs(
    title = "Claim Proportion by Driving Experience",
    x = "Driving Experience",
    y = "Proportion"
  )

#Logistic regression model 
model <- glm(
  CLAIM ~ AGE + GENDER + RACE + DRIVING_EXPERIENCE + EDUCATION
  + INCOME + CREDIT_SCORE + VEHICLE_OWNERSHIP + VEHICLE_YEAR
  + MARRIED + CHILDREN + ANNUAL_MILEAGE + VEHICLE_TYPE + SPEEDING_VIOLATIONS + DUIS
  + PAST_ACCIDENTS,
  family = binomial(link = "logit"),
  data= data
)

summary(model)   #prints regression results and shows which variables are most associated with claim probability

#odds ratios + confidence intervals
exp(cbind(
  "Odds Ratio" = coef(model),
  confint(model)
))

#predictions
data$PredictedProb <- predict(model, type = "response")
data$PredictedClass <- ifelse(data$PredictedProb > 0.5, 1, 0)

head(data[, c("CLAIM", "PredictedProb", "PredictedClass")])    #check if first few predictions look reasonable

#accuracy
mean(data$PredictedClass == data$CLAIM)   #this computes accuray = # of correct predictions/# total observations

#Model fit metrics
AIC(model)

null_model <- glm(CLAIM ~ 1, family = binomial(link = "logit"),
                  data = data)
mcfadden_r2 <- 1 - (model$deviance / null_model$deviance)
mcfadden_r2

roc_obj <- roc(data$CLAIM, data$PredictedProb)
auc(roc_obj)

plot(roc_obj, main = "ROC Curve for Claim Prediction Model")

#Decile lift and calibration
lift_table <- data %>%
  mutate(decile = ntile(desc(PredictedProb),10)) %>%
  group_by(decile) %>%
  summarise(
    Count = n(),
    Claims = sum(CLAIM),
    AvgPredictedProb = mean(PredictedProb),
    ActualClaimRate = mean(CLAIM),
    .groups = "drop"
  ) %>%
  arrange(decile) %>%
  mutate(
    OverallClaimRate = mean(data$CLAIM),
    Lift = ActualClaimRate / OverallClaimRate
  )

print(lift_table)

#lift chart
ggplot(lift_table, aes(x = factor(decile), y = Lift, group = 1))+
  geom_line() +
  geom_point() +
  labs(
    title = "Decile Lift Chart",
    x = "Prediction Decile (1 = Highest Risk)",
    y = "Lift"
  )


# Calibration plot: predicted vs actual claim rate by decile
ggplot(lift_table, aes(x = factor(decile), group = 1)) + geom_line(aes(y = AvgPredictedProb, linetype = "Predicted")) +
         geom_point(aes(y = AvgPredictedProb, shape = "Predicted")) +
         geom_line(aes(y = ActualClaimRate, linetype = "Actual")) +
         geom_point(aes(y = ActualClaimRate, shape = "Actual"))+
         labs(
           title = "Predicted vs Actual Claim Rate by Decile",
           x = "Prediction Decile (1 = Highest Risk)",
           y = "Claim Rate",
           linetype = "",
           shape = ""
         )
         
                                                                
summary(model)







