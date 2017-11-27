library(dplyr)
library(MASS)
library(olsrr)
library(car)
library(caret)
library(caTools)
source("~/Desktop/SMU_MSDS_Homework/Homework/6371/Project/helper_functions.R")

df.train2 <- read.csv("~/Desktop/SMU_MSDS_Homework/Homework/6371/Project/train.csv")
df.test <- read.csv("~/Desktop/SMU_MSDS_Homework/Homework/6371/Project/testCleaned.csv")
## clean data
df.train2 <- cleanData(df.train2)

fit.full2 <- lm(df.train2$SalePrice ~ ., data = df.train2, na.action = na.exclude)
summary(fit.full2)

# look at the scatter plots to assess for normality ####################
#df.train2.numeric <- select_if(df.train2, is.numeric)

#df.plots <- melt(df.train2.numeric, "SalePrice")

#ggplot(df.plots, aes(value, df.plots$SalePrice)) + 
 # geom_point() + 
  #facet_wrap(~variable, scales = "free")

# hist(df.train2.numeric$SalePrice)
######################################################################

# transform values
df.train2 <- transformData(df.train2)

# get train and test
set.seed(101) 
train.size <- 0.8
train.index <- sample.int(length(df.train2$SalePrice), round(length(df.train2$SalePrice) * train.size))

train = df.train2[train.index, ]
test  = df.train2[-train.index, ]

# df.train2 <- train

# remove inf
df.train2[mapply(is.infinite, df.train2)] <- NA

#--------------------------------------------------------#

# choose model 
df.train2.steps <- df.train2
fit.steps <- lm(SalePrice ~ ., data = df.train2.steps, na.action = na.exclude)

# olsrr::ols_stepaic_forward(fit.steps, details = T)
fit.forward <- lm(formula = SalePrice ~ OverallQual + GrLivArea + Neighborhood + TotalBsmtSF + OverallCond 
                  + YearBuilt + LotArea + BsmtUnfSF + KitchenQual + SaleCondition + GarageCars + Exterior1st 
                  + Condition1 + BsmtExposure + Fireplaces + MSZoning + BsmtQual + BldgType + Foundation 
                  + BsmtFullBath + CentralAir + BsmtFinType1 + FullBath + HalfBath + LotConfig + HouseStyle 
                  + X2ndFlrSF + MasVnrType + PavedDrive + YearRemodAdd + Electrical + ExterCond, 
                  data = df.train2.steps, na.action = na.exclude)
summary(fit.forward)

# olsrr::ols_stepaic_backward(fit.steps, details = T)
fit.backward <- lm(SalePrice ~ Id + MSZoning + LotArea + LotConfig + Neighborhood + Condition1 
                   + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd 
                   + Exterior1st + MasVnrType + ExterCond + Foundation + BsmtQual + BsmtExposure 
                   + BsmtFinType1 + BsmtUnfSF + TotalBsmtSF + CentralAir + Electrical + X2ndFlrSF 
                   + GrLivArea + BsmtFullBath + FullBath + HalfBath + KitchenQual + Fireplaces 
                   + GarageCars + PavedDrive + YrSold + SaleCondition, data = df.train2.steps, na.action = na.exclude)
summary(fit.backward)

#olsrr::ols_stepaic_both(fit.steps, details = T)
fit.both <- lm(formula = SalePrice ~ OverallQual + GrLivArea + Neighborhood + TotalBsmtSF + OverallCond 
               + YearBuilt + LotArea + BsmtUnfSF + KitchenQual + SaleCondition + GarageCars + Exterior1st 
               + Condition1 + BsmtExposure + Fireplaces + MSZoning + BsmtQual + BldgType + Foundation 
               + BsmtFullBath + CentralAir + BsmtFinType1 + FullBath + HalfBath + LotConfig + HouseStyle 
               + X2ndFlrSF + MasVnrType + PavedDrive + YearRemodAdd + Electrical + ExterCond, 
               data = df.train2.steps, na.action = na.exclude)
summary(fit.both)

# k fold cross validation
df.train2.kfold <- df.train2
set.seed(17)
model <- train(
  SalePrice ~ LotArea + OverallQual  
  + EncodeBsmtQual + EncodeBsmtExposure + GrLivArea + TotalBsmtSF
  + YearBuilt + MSZoning 
  + OverallCond + EncodedFoundation + CentralAir + KitchenQual + Fireplaces 
  + GarageCars + EncodeSaleType + EncodedSaleCondition + cent1 + EncodeNeighborhood 
  + EncodeCondition1 + EncodeCondition1L + EncodeBldgType + BsmtFullBath, df.train2.manual,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  ), na.action = na.omit
)
model


# manual fit
df.train2.manual <- df.train2

df.train2.manual <- encodeData(df.train2.manual)

fit.manual <- lm(formula = SalePrice ~ LotArea + OverallQual  
                  + EncodeBsmtQual + EncodeBsmtExposure + GrLivArea + TotalBsmtSF
                 + YearBuilt + MSZoning 
                 + OverallCond + EncodedFoundation + CentralAir + KitchenQual + Fireplaces 
                 + GarageCars + EncodeSaleType + EncodedSaleCondition + cent1 + EncodeNeighborhood 
                 + EncodeCondition1 + EncodeCondition1L + EncodeBldgType + BsmtFullBath, 
                 data = df.train2.manual, na.action = na.exclude)

summary(fit.manual)
vif(fit.manual)
plot(fit.manual)
hist(fit.manual$residuals)
plot(cooks.distance(fit.manual, data = df.train2.manual))

# alpha precitions for pre-validation
test <- encodeData(test)
test$PredPrice <- predict(fit.manual, newdata = subset(test, select = c(BsmtFullBath, EncodeBldgType, EncodeCondition1L, EncodeCondition1,EncodeNeighborhood, cent1, LotArea,OverallQual,EncodeBsmtQual,EncodeBsmtExposure,GrLivArea,TotalBsmtSF,YearBuilt,MSZoning,OverallCond,EncodedFoundation,CentralAir,KitchenQual,Fireplaces,GarageCars,EncodeSaleType,EncodedSaleCondition)))
sqrt(mean((test$PredPrice - test$SalePrice) ^2, na.rm=TRUE))

# clean, transform, encode test data
t <- read.csv("~/Desktop/SMU_MSDS_Homework/Homework/6371/Project/train.csv")
t <- t[-c(1460), ]
df.test$SalePrice <- log(t$SalePrice)
df.test <- cleanData(df.test, isTrain = F)
df.test <- transformData(df.test, isTest = F)
df.test <- encodeData(df.test)

# generate predictions
df.test$PredPrice <- predict(fit.manual, newdata = subset(df.test, select = c(BsmtFullBath,EncodeBldgType, EncodeCondition1L, EncodeCondition1, EncodeNeighborhood, cent1, LotArea,OverallQual,EncodeBsmtQual,EncodeBsmtExposure,GrLivArea,TotalBsmtSF,BsmtUnfSF,BathToRoom,YearBuilt,MSZoning,OverallCond,EncodedFoundation,CentralAir,KitchenQual,Fireplaces,GarageCars,EncodeSaleType,EncodedSaleCondition)))
df.test$SalePrice <- exp(df.test$PredPrice)

# create kaggle data frame
kaggleColumns <- c("Id", "SalePrice")
df.kaggle <- df.test[kaggleColumns]
df.kaggle[mapply(is.na, df.kaggle)] <- exp(median(df.train2.manual$SalePrice, na.rm=TRUE))
write.csv(x = df.kaggle, file = "~/Desktop/meacreatio_housing.csv", row.names = F)

# RMSE = 0.09326669
# Kaggle = 0.15712

# for testing
df.trainTest <- read.csv("q1_data1.csv")
df.trainTest <- transformData(df.trainTest)
df.trainTest[mapply(is.na, df.trainTest)] <- 0
modelTest <- train(
  SalePrice ~ ., data = df.trainTest,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  ), na.action = na.omit
)




