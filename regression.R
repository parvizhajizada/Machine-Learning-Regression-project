# Load necessary packages
library(readr)
library(dplyr)
library(tibble)
library(corrplot)
library(ggplot2)
library(AER)
library(lmtest)
library(nnet)
library(MASS)
library(caret)
library(verification)
library(e1071)
library(janitor)
library(class)
library(kernlab)
library(verification)
library(tidyverse)
library(gmodels)
library(vcd)
library(lubridate)

# Load data
house <- read_csv("kc_house_data.csv")

# Summary of the data
any(is.na(house)) # no missing data
summary(house) 
str(house) 

# Feature engineering
table(house$yr_renovated) # 20699 house has never been renovated 
                          # out of 21613 which is around 96 %.
                          # We could feature engineer a column 
                          # such as time passed since renovation
                          # but we will not because most of the 
                          # house have been renovated. 
                          # So we will feature engineer a column 
                          # which shows if the house has been
                          # renovated or not regardless of year and
                          # we drop the yr_renovated column afterwards

house <- house %>% mutate(year = year(date),     # year at which the house is sold
                          month = month(date),   # month at which the house is sold
                          age = year - yr_built, # time interval between year build 
                          # and year sold (i.e. age of house)
                          renovated = factor(ifelse(yr_renovated != 0, 1,0))) # 1 if the 
                          # house is renovated, 0 if it is not

house$yr_renovated <- NULL # dropping the yr_renovated column

# Distribution of certain variable
house$year <- NULL # there are only two years so it is not likely to give much information 
                   # and for the sake of model simplicity and for not making out of sample
                   # prediction infeasible, we decide to forego the marginal information gain
                   # Month variable stays because it has seasonal variations and it does not
                   # make out of sample prediction infeasible unlike year variable


# Drop certain columns
house$id <- NULL # id variable gives no information
house$date <- NULL # date column enables us to see how price changes over time
                   # but once the year and month columns were created, we do not 
                   # need the date column itself. 

# latitude and longitude variables are good for making heat map of price over a map
# which is done below but it is not so intiutive to expect the price being dependent on
# those variables explicitly. For that reason, we find the zipcode variable as a better
# location factor affecting the price and use it in estimation instead of latitude,longitude.

table(house$zipcode) # kind of uniformaly distributed or at least not inbalanced.
                     # not likely to overfit.
table(house$bedrooms) # 0 bedroom does not make much sense, hard to say if it is 
                      # wrong data entry or there really are houses with 0 bedrooms.
                      # but we can safely drop these values because there are only
                      # 13 of them. Number of houses with 7 and more bedrooms are 
                      # quite little we will aggregate them under 7+ bedrooms level.
                      # we drop the house with 33 bedrooms because it is an outlier

house <- house %>% filter(bedrooms !=0, bedrooms !=33) # drop observations with 0 or 
                                                       # 33 bedrooms

# values above 7 will be coded as 7
house$bedrooms[house$bedrooms > 7] <- 7
house$bedrooms <- factor(house$bedrooms)

# Changing last level from 7 to 6+
levels(house$bedrooms)
levels(house$bedrooms)[7] <- "6+"

# Do the similar data processing to bathrooms column also
table(house$bathrooms)

house <- house %>% filter(bathrooms !=0, bathrooms !=0.5,
                          bathrooms !=0.75, bathrooms !=1.25) # drop observations with 
                                                              # rare number of bathrooms

# values above 3.75 will be coded as 3.75
house$bathrooms[house$bathrooms > 3.75] <- 3.75
house$bathrooms <- factor(house$bathrooms)

# Changing last level from 3.75 to 3.5+
levels(house$bathrooms)
levels(house$bathrooms)[11] <- "3.5+"

# Do the similar data processing to floors column also
table(house$floors)
house <- house %>% filter(floors !=3.5) # drop observations with 
                                        # rare number of floors

# values above 2.5 will be coded as 2.5
house$floors[house$floors > 2.5] <- 2.5
house$floors <- factor(house$floors)

# Changing last level from 2.5 to 2+
levels(house$floors)
levels(house$floors)[4] <- "2+"

# Do the similar data processing to grade column also
table(house$grade)

# values above 12 will be coded as 12
house$grade[house$grade > 12] <- 12
house$grade <- factor(house$grade)

# Changing last level from 12 to 12+
levels(house$grade)
levels(house$grade)[9] <- "11+"

# Do the similar data processing to condition column also
table(house$condition)

# values above 12 will be coded as 12
house$condition[house$condition < 2] <- 2
house$condition <- factor(house$condition)

# Changing last level from 12 to 12+
levels(house$condition)
levels(house$condition)[1] <- "3-"

# Drop observations with age being equal to -1 (Data entry mistake)
table(house$age)
house <- house %>% filter(age !=-1)

# Drop yr_built column because it is unnecassary once we have age column
cor(house$yr_built, house$age) # validation of claim by calculating correlation
                               # -0.999 correlation or by the fact that age is
                               # linear transformation of yr_built.
house$yr_built <- NULL

# Changing class of certain variables 
vars_factor <- c("waterfront", "view", "zipcode", "month")
house[vars_factor] <- lapply(house[vars_factor], factor)

# Modelling
# Data Partitioning
set.seed(1)
which_train <- createDataPartition(house$price, 
                                   p = 0.8, 
                                   list = FALSE) 

# Split data into training and test test 
train <- house[which_train,]
test <- house[-which_train,]

# Comparison of the distribution of the dependent variable in both samples
tabyl(train$price) # looks fine
tabyl(test$price) # looks fine

ctrl_cv5x3a <- trainControl(method = "repeatedcv",
                            number = 5,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary,
                            repeats = 3)

# Logistic Regression
set.seed(1)

linear.model <- lm(price ~ bedrooms + bathrooms + sqft_living +
                     sqft_lot + floors + waterfront + view + condition + 
                     grade + sqft_above + sqft_basement + zipcode +
                     sqft_living15 + sqft_lot15 + month + age +
                     renovated,  data = house)

linear.train <- 
  train(price ~ bedrooms + bathrooms + sqft_living +
          sqft_lot + floors + waterfront + view + condition + 
          grade + sqft_above + sqft_basement + zipcode +
          sqft_living15 + sqft_lot15 + month + age +
          renovated, 
        data = train, 
        method = "lm")

# Summary
summary(linear.train)

# Fitted values
linear.train_fitted <- predict(linear.train, train)

# Predicted values
linear.train_forecasts <- predict(linear.train, test)

# RMSE, R^2, MAE of test set
postResample(pred = linear.train_forecasts, obs = test$price)

# RMSE, R^2, MAE of train set
postResample(pred = linear.train_fitted, obs = train$price)


