library(dplyr)
library(MASS)
library(olsrr)
library(car)
library(caret)
library(caTools)
library(mice)
source("helper_functions.R")

df.train2 <- read.csv("train.csv")
df.test <- read.csv("true_test.csv")
## clean data for scatter plots
df.train2.scatters <- cleanData(df.train2)

fit.full2 <- lm(df.train2.scatters$SalePrice ~ ., data = df.train2, na.action = na.exclude)
summary(fit.full2)

# look at the scatter plots to assess for normality ####################
#df.train2.numeric <- select_if(df.train2, is.numeric)

#df.plots <- melt(df.train2.numeric, "SalePrice")

#ggplot(df.plots, aes(value, df.plots$SalePrice)) + 
 # geom_point() + 
  #facet_wrap(~variable, scales = "free")

# hist(df.train2.numeric$SalePrice)
######################################################################
# for generating imputed training data
writeImputeData <- F
if (writeImputeData == T) {
  # clean data
  df.train2 <- cleanData(df.train2)
  # transform values
  df.train2 <- transformData(df.train2)
  # encode variables
  df.train2 <- encodeData(df.train2)
  df.train2 <- mice(df.train2[, names(df.train2)], method="rf")
  df.train2 <- complete(df.train2)
  write.csv(x = df.train2, file = "true_train.csv", row.names = F)
}

df.train2 <- read.csv("true_train.csv")

# get internal train and test
set.seed(101) 
train.size <- 0.8
train.index <- sample.int(length(df.train2$SalePrice), round(length(df.train2$SalePrice) * train.size))

train = df.train2[train.index, ]
test  = df.train2[-train.index, ]

#--------------------------------------------------------#

# choose model 
df.train2.steps <- df.train2
fit.steps <- lm(SalePrice ~ ., data = df.train2.steps, na.action = na.exclude)

# olsrr::ols_stepaic_forward(fit.steps, details = T)
formula.forward <- as.formula(SalePrice ~ OverallQual + GrLivArea + Neighborhood + TotalBsmtSF 
                              + OverallCond + YearBuilt + LotArea + BsmtFullBath + KitchenQual 
                              + GarageCars + SaleCondition + BldgType + EncodeBsmtExposure 
                              + BsmtFinSF1 + ExteriorFirst + Condition1 + cent1 + Foundation 
                              + cent3 + Fireplaces + MSZoning + BsmtUnfSF + CentralAir 
                              + MasVnrType + BsmtQual + ExterCond + BathToRoom + GarageYrBlt 
                              + EncodeHeatingQC + PavedDrive + X1stFlrSF + BedroomAbvGr 
                              + EncodeLotConfig + YearRemodAdd, env = new.env())
fit.forward <- lm(formula = formula.forward, data = df.train2.steps, na.action = na.exclude)
summary(fit.forward)

formula.backward <- as.formula(SalePrice ~ MSSubClass + MSZoning + LotArea + Neighborhood 
                               + HouseStyle + OverallQual + OverallCond + YearBuilt 
                               + YearRemodAdd + MasVnrType + ExterCond + Foundation 
                               + BsmtQual + BsmtFinType1 + BsmtFinSF1 + TotalBsmtSF 
                               + CentralAir + X2ndFlrSF + GrLivArea + BsmtFullBath 
                               + BedroomAbvGr + KitchenQual + Fireplaces + GarageYrBlt 
                               + GarageCars + PavedDrive + ExteriorFirst + BsmtUnfSf 
                               + EncodeBsmtExposure + BathToRoom + EncodeBldgType 
                               + EncodeSaleType + EncodedSaleCondition + cent1 
                               + EncodeCondition1 + EncodeCondition1L + EncodeLotConfig 
                               + EncodeHeatingQC + cent3, env = new.env())
# olsrr::ols_stepaic_backward(fit.steps, details = T)
fit.backward <- lm(formula.backward,data = df.train2.steps, na.action = na.exclude)
summary(fit.backward)

formula.both <- as.formula(SalePrice ~ OverallQual + GrLivArea + Neighborhood + TotalBsmtSF 
                           + OverallCond + YearBuilt + LotArea + BsmtFullBath + KitchenQual 
                           + GarageCars + SaleCondition + BldgType + EncodeBsmtExposure 
                           + BsmtFinSF1 + ExteriorFirst + Condition1 + cent1 + Foundation 
                           + cent3 + Fireplaces + MSZoning + BsmtUnfSF + CentralAir 
                           + MasVnrType + BsmtQual + ExterCond + BathToRoom + GarageYrBlt 
                           + EncodeHeatingQC + PavedDrive + X1stFlrSF + BedroomAbvGr 
                           + EncodeLotConfig + YearRemodAdd, env = new.env())
# olsrr::ols_stepaic_both(fit.steps, details = T)
fit.both <- lm(formula = formula.both, data = df.train2.steps, na.action = na.exclude)
summary(fit.both)

# k fold cross validation
df.train2.kfold <- df.train2

kfold.forward <- kfold(lmFormula = formula.forward, df = df.train2.steps)
kfold.backward <- kfold(lmFormula = formula.backward, df = df.train2.steps)
kfold.both <- kfold(lmFormula = formula.both, df = df.train2.steps)
rss(kfold.forward)
rss(kfold.backward)
rss(kfold.both)

# manual fit
df.train2.manual <- df.train2
formula.manual <- as.formula(SalePrice ~ OverallQual + GrLivArea + TotalBsmtSF 
                             + OverallCond + YearBuilt + LotArea + BsmtFullBath + KitchenQual 
                             + GarageCars + EncodeBsmtExposure
                             + EncodeExteriorFirst + EncodeCondition1 + EncodeCondition1L + cent1  
                             + cent3 + Fireplaces + MSZoning + BsmtUnfSF + CentralAir 
                             + MasVnrType + BsmtQual + BathToRoom + EncodeCondition1 
                             + EncodeCondition1L + EncodeBldgType
                             + EncodeNeighborhood + EncodeNeighborhoodL + EncodedSaleCondition, env = new.env())
fit.manual <- lm(formula = formula.manual, data = df.train2.manual, na.action = na.exclude)
kfold.manual <- kfold(lmFormula = formula.manual, df = df.train2.manual)
rss(kfold.manual)
summary(fit.manual)

vif(fit.manual)
plot(fit.manual)
hist(fit.manual$residuals)
plot(cooks.distance(fit.manual, data = df.train2.manual))

# alpha precitions for pre-validation
test$PredPrice <- predict.lm(fit.manual, newdata = test)
sqrt(mean((test$PredPrice - test$SalePrice) ^2, na.rm=TRUE))

# clean, transform, encode test data
t <- read.csv("train.csv")
t <- t[-c(1460), ]
df.test$SalePrice <- log(t$SalePrice)

# for creating an imputed test set with test.csv
if (writeImputeData == T) {
  df.test <- cleanData(df.test, isTrain = F)
  df.test <- transformData(df.test, isTest = F)
  df.test <- encodeData(df.test)
  df.test <- mice(df.test[, names(df.test)], method="rf")
  df.test <- complete(df.test)
  write.csv(x = df.test, file = "true_test.csv", row.names = F)
}

# generate predictions for manual
df.test$PredPrice <- predict.lm(fit.manual, df.test)
df.test$SalePrice <- exp(df.test$PredPrice)

# generate predictions for forward
df.test$PredPrice <- predict.lm(object = fit.forward, newdata = df.test)
df.test$SalePrice <- exp(df.test$PredPrice)

# generate predictions for backward
df.test$PredPrice <- predict.lm(object = fit.backward, newdata = df.test)
df.test$SalePrice <- exp(df.test$PredPrice)

# generate predictions for both
df.test$PredPrice <- predict.lm(object = fit.both, newdata = df.test)
df.test$SalePrice <- exp(df.test$PredPrice)

# create kaggle data frame
kaggleColumns <- c("Id", "SalePrice")
df.kaggle <- df.test[kaggleColumns]
write.csv(x = df.kaggle, file = "predictions.csv", row.names = F)

# RMSE = 0.08950696 fit.manual
# Kaggle = 0.12127 - fit.both





