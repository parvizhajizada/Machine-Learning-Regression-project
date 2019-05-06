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
library(gridExtra)
library(devtools)
library(maps)
library(mapdata)
library(ggmap)


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

# Drop highly correlated columns
M <- cor(house[,c("price", "sqft_living", "sqft_lot", 
                  "sqft_basement", "age", 
                  "sqft_above", "sqft_living15", "sqft_lot15")])
corrplot.mixed(M) # Drop sqft_above, sqft_living15, sqft_lot15 
# because of high correlation

# Data Visualization
p1 <- ggplot(train, aes(x=sqft_living, y=price)) + 
  geom_point()+
  geom_smooth()

p2 <- ggplot(train, aes(x=sqft_lot, y=price)) + 
  geom_point()+
  geom_smooth()

p3 <- ggplot(train, aes(x=sqft_basement, y=price)) + 
  geom_point()+
  geom_smooth()

grid.arrange(p1, p2, p3, ncol = 3)

p4 <- ggplot(data=train, aes(x=bedrooms, y=price)) +
  geom_bar(stat="identity")

p5 <- ggplot(data=train, aes(x=bathrooms, y=price)) +
  geom_bar(stat="identity")

p6 <- ggplot(data=train, aes(x=floors, y=price)) +
  geom_bar(stat="identity")

p7 <- ggplot(data=train, aes(x=waterfront, y=price)) +
  geom_bar(stat="identity")

p8 <- ggplot(data=train, aes(x=view, y=price)) +
  geom_bar(stat="identity")

p9 <- ggplot(data=train, aes(x=condition, y=price)) +
  geom_bar(stat="identity")

p10 <- ggplot(data=train, aes(x=grade, y=price)) +
  geom_bar(stat="identity")

p11 <- ggplot(data=train, aes(x=month, y=price)) +
  geom_bar(stat="identity")

p12 <- ggplot(data=train, aes(x=renovated, y=price)) +
  geom_bar(stat="identity")

grid.arrange(p4, p5, p6 ,p7 ,p8,
             p9 ,p10 ,p11, p12,
             nrow = 3, ncol=3)

# Map
states <- map_data("state")
ca_df <- subset(states, region == "washington")
counties <- map_data("county")
ca_county <- subset(counties, region == "washington")
king_county <- subset(ca_county, subregion == "king")
ca_base <- ggplot(data = king_county, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(color = "black", fill = "gray")

b <- ggplot(house, aes(x = long, y = lat))+
  geom_point(aes(color = price)) +
  scale_color_gradientn(colours = rainbow(5)) +
  theme(legend.position = "right")

ca_base
b

# More visualization
ggplot(train, aes(x=sqft_living, y=price, color = bathrooms)) + 
  geom_point()

avg_price_ren <- house %>% group_by(renovated) %>% summarize(average_price = mean(price))
ggplot(avg_price_ren, aes(x = renovated, y = average_price, fill = renovated)) + 
  geom_bar(stat = "identity") +
  ggtitle("Mean price VS renovated")

avg_price_view <- house %>% group_by(view) %>% summarize(average_price = mean(price))
ggplot(avg_price_view, aes(x = view, y = average_price, fill = view)) + 
  geom_bar(stat = "identity") +
  ggtitle("Mean price VS view")

# Modelling
# Data Partitioning
set.seed(1)
which_train <- createDataPartition(house$price, 
                                   p = 0.8, 
                                   list = FALSE) 

# Split data into training and test test 
train <- house[which_train,]
test <- house[-which_train,]


# Linear Regression
set.seed(1)

formula <- price ~ bedrooms + bathrooms + sqft_living +
  sqft_lot + floors + waterfront + view + condition + 
  grade + sqft_basement + zipcode +
  month + renovated

linear.train <- 
  train(formula, 
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

#         RMSE     Rsquared          MAE 
# 1.488733e+05 8.296537e-01 8.981330e+04


# RMSE, R^2, MAE of train set
postResample(pred = linear.train_fitted, obs = train$price)

#         RMSE     Rsquared          MAE 
# 1.496397e+05 8.355731e-01 8.779856e+04


# SVM
# Setting train control
ctrl_cv2 <- trainControl(method = "cv",
                         number = 2)

# Parameters of svmLinear
modelLookup("svmLinear")

# Grid search
parametersC <- data.frame(C = c(0.01, 0.1, 0.2, 0.5, 1, 5))

# Train data
set.seed(1)
svm_Linear <- train(formula, 
                    data = train, 
                    method = "svmLinear",
                    tuneGrid = parametersC,
                    trControl = ctrl_cv2)

# Summary
summary(svm_Linear)

# Fitted values
svm_linear.train_fitted <- predict(svm_Linear, train)

# Predicted values
svm_linear.train_forecasts <- predict(svm_Linear, test)

# RMSE, R^2, MAE of test set
postResample(pred = svm_linear.train_forecasts, obs = test$price)

#         RMSE     Rsquared          MAE 
# 1.513252e+05 8.279697e-01 8.291942e+04 

# RMSE, R^2, MAE of train set
postResample(pred = svm_linear.train_fitted, obs = train$price)

#         RMSE     Rsquared          MAE 
# 1.579167e+05 8.290153e-01 8.222068e+04


# Parameters of svmRadial
modelLookup("svmRadial")

# Grid Search
parametersC_sigma <- 
  expand.grid(C = c(0.01, 0.05, 0.1, 0.5, 1, 5),
              sigma = c(0.05, 0.1, 0.2, 0.5, 1))

# Train data
set.seed(1)
svm_radial <- train(formula, 
                    data = train, 
                    method = "svmRadial",
                    tuneGrid = parametersC_sigma,
                    trControl = ctrl_cv2)

# Summary
summary(svm_radial)

# Fitted values
svm_radial.train_fitted <- predict(svm_radial, train)

# Predicted values
svm_radial.train_forecasts <- predict(svm_radial, test)

# RMSE, R^2, MAE of test set
postResample(pred = svm_radial.train_forecasts, obs = test$price)

#         RMSE     Rsquared          MAE 
# 2.634224e+05 4.849898e-01 1.342765e+05

# RMSE, R^2, MAE of train set
postResample(pred = svm_radial.train_fitted, obs = train$price)

#         RMSE     Rsquared          MAE 
# 8.308042e+04 9.569912e-01 3.602115e+04 

# Parameters of Treebag
modelLookup("treebag")

# Train data
set.seed(1)
treebag.train <- train(formula, 
                       data = train, 
                       method = "treebag")

# Summary
summary(treebag.train)

# Fitted values
treebag.train_fitted <- predict(treebag.train, train)

# Predicted values
treebag.train_forecasts <- predict(treebag.train, test)

# RMSE, R^2, MAE of test set
postResample(pred = treebag.train_forecasts, obs = test$price)

#         RMSE     Rsquared          MAE 
# 2.246180e+05 6.091055e-01 1.568402e+05


# RMSE, R^2, MAE of train set
postResample(pred = treebag.train_fitted, obs = train$price)

#         RMSE     Rsquared          MAE 
# 2.263118e+05 6.291737e-01 1.559959e+05 


# Bayesglm
# Parameters of svmRadial
modelLookup("bayesglm")

# Train data
set.seed(1)
bayesglm.train <- train(formula, 
                        data = train, 
                        method = "bayesglm")

# Summary
summary(bayesglm.train)

# Fitted values
bayesglm.train_fitted <- predict(bayesglm.train, train)

# Predicted values
bayesglm.train_forecasts <- predict(bayesglm.train, test)

# RMSE, R^2, MAE of test set
postResample(pred = bayesglm.train_forecasts, obs = test$price)

#         RMSE     Rsquared          MAE 
# 1.488737e+05 8.296501e-01 8.981299e+04 


# RMSE, R^2, MAE of train set
postResample(pred = bayesglm.train_fitted, obs = train$price)

#         RMSE     Rsquared          MAE 
# 1.496397e+05 8.355730e-01 8.779858e+04 


# CART
# Parameters of svmRadial
modelLookup("rpart1SE")

# Train data
set.seed(1)
rpart1SE.train <- train(formula, 
                        data = train, 
                        method = "rpart1SE")

# Summary
summary(rpart1SE.train)

# Fitted values
rpart1SE.train_fitted <- predict(rpart1SE.train, train)

# Predicted values
rpart1SE.train_forecasts <- predict(rpart1SE.train, test)

# RMSE, R^2, MAE of test set
postResample(pred = rpart1SE.train_forecasts, obs = test$price)

#         RMSE     Rsquared          MAE 
# 2.430796e+05 5.481711e-01 1.650317e+05


# RMSE, R^2, MAE of train set
postResample(pred = rpart1SE.train_fitted, obs = train$price)

#         RMSE     Rsquared          MAE 
# 2.434084e+05 5.649388e-01 1.644436e+05
