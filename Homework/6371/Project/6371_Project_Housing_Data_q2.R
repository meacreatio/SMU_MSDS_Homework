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
# encode variables
df.train2 <- encodeData(df.train2)
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
formula.forward <- as.formula(SalePrice ~ OverallQual + GrLivArea + Neighborhood + TotalBsmtSF 
                           + OverallCond + YearBuilt + LotArea + BsmtUnfSF + KitchenQual + SaleCondition 
                           + GarageCars + Condition1 + BsmtExposure + Fireplaces + MSZoning 
                           + BsmtQual + EncodeBldgType + cent1 + BsmtFullBath + CentralAir 
                           + BsmtFinType1 + MSSubClass + LotConfig + ExterCond + HeatingQC + FullBath 
                           + HalfBath + PavedDrive + HouseStyle + X2ndFlrSF, env = new.env())
fit.forward <- lm(formula = formula.forward, data = df.train2.steps, na.action = na.exclude)
summary(fit.forward)

formula.backward <- as.formula(SalePrice ~ MSSubClass + MSZoning + LotArea + LotConfig + Neighborhood 
                               + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd 
                               + MasVnrType + ExterCond + BsmtQual + BsmtExposure + BsmtFinType1 
                               + BsmtUnfSF + TotalBsmtSF + CentralAir + X2ndFlrSF + GrLivArea + BsmtFullBath 
                               + FullBath + HalfBath + KitchenQual + Fireplaces + GarageCars + PavedDrive 
                               + SaleCondition + EncodeBldgType + cent1 + EncodeCondition1 + EncodeCondition1L, env = new.env())
# olsrr::ols_stepaic_backward(fit.steps, details = T)
fit.backward <- lm(formula.backward,data = df.train2.steps, na.action = na.exclude)
summary(fit.backward)

formula.both <- as.formula(SalePrice ~ OverallQual + GrLivArea + Neighborhood + TotalBsmtSF 
                           + OverallCond + YearBuilt + LotArea + BsmtUnfSF + KitchenQual + SaleCondition 
                           + GarageCars + Condition1 + BsmtExposure + Fireplaces + MSZoning 
                           + BsmtQual + EncodeBldgType + cent1 + BsmtFullBath + CentralAir 
                           + BsmtFinType1 + MSSubClass + LotConfig + ExterCond + HeatingQC + FullBath 
                           + HalfBath + PavedDrive + HouseStyle, env = new.env())
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
formula.manual <- as.formula(SalePrice ~ LotArea + OverallQual  
                             + EncodeBsmtQual + EncodeBsmtExposure + GrLivArea + TotalBsmtSF
                             + YearBuilt + MSZoning + EncodeNeighborhoodL
                             + OverallCond + EncodedFoundation + CentralAir + KitchenQual + Fireplaces 
                             + GarageCars + EncodeSaleType + EncodedSaleCondition + cent1 + EncodeNeighborhood 
                             + EncodeCondition1 + EncodeCondition1L + EncodeBldgType + BsmtFullBath, env = new.env())
fit.manual <- lm(formula = formula.manual, data = df.train2.manual, na.action = na.exclude)
kfold.manual <- kfold(lmFormula = formula.manual, df = df.train2.manual)
rss(kfold.manual)

summary(fit.manual)
vif(fit.manual)
plot(fit.manual)
hist(fit.manual$residuals)
plot(cooks.distance(fit.manual, data = df.train2.manual))

# alpha precitions for pre-validation
test$PredPrice <- predict(fit.manual, newdata = test)
sqrt(mean((test$PredPrice - test$SalePrice) ^2, na.rm=TRUE))

# clean, transform, encode test data
t <- read.csv("~/Desktop/SMU_MSDS_Homework/Homework/6371/Project/train.csv")
t <- t[-c(1460), ]
df.test$SalePrice <- log(t$SalePrice)
df.test <- cleanData(df.test, isTrain = F)
df.test <- transformData(df.test, isTest = F)
df.test <- encodeData(df.test)

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
df.kaggle[mapply(is.na, df.kaggle)] <- exp(median(df.train2.manual$SalePrice, na.rm=TRUE))
write.csv(x = df.kaggle, file = "~/Desktop/meacreatio_housing.csv", row.names = F)

# RMSE = 0.09110417
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




