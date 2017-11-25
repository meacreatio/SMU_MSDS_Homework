library(dplyr)
library(MASS)
library(olsrr)
library(car)
library(caret)
library(caTools)

cleanData <- function(df, isTrain = T) {
  if(isTrain == T) {
    df <- df[df$LotShape != 'IR3', ] 
    # remove extreme outliers
    df <- df[!(df$Id %in% c(1299, 524, 31, 463, 633, 89, 589, 496)), ]
  }

  # remove variables with near zero variance
  df <- df[-nearZeroVar(df)]
  
  # find high rate of missing values and remove those variables
  colSums(is.na(df))
  df$Alley <- NULL
  df$PoolQC <- NULL
  df$Fence <- NULL
  df$FireplaceQu <- NULL
  dfOpenPorchSF <- NULL
  df$WoodDeckSF <- NULL
  df
}

transformData <- function(df, isTest = F) {
  df$LotFrontage <- log(df$LotFrontage)
  df$LotArea <- log(df$LotArea)
  df$MasVnrArea <- log(df$MasVnrArea)
  df$BsmtFinSF1 <- log(df$BsmtFinSF1)
  df$BsmtUnfSf <- log(df$BsmtUnfSF)
  df$TotalBsmtSF <- log(df$TotalBsmtSF)
  df$X1stFlrSF <- log(df$X1stFlrSF)
  
  v <- sapply(X = df$X2ndFlrSF, function(x) {
    value = 0
    if (x != 0) {
      value = log(x)
    }
    value
  })
  df$X2ndFlrSF <- v
  
  df$GrLivArea <- log(df$GrLivArea)
  df$GarageArea <- log(df$GarageArea)
  if(isTest == F) {
    df$SalePrice <- log(df$SalePrice)
  }
  
  df[mapply(is.infinite, df)] <- NA
  
  df
}

encodeData <- function(df) {
  df$EncodeLotShape <- as.numeric(df$LotShape)
  df$EncodeNeighborhood <- ifelse(df$Neighborhood == "MeadowV", 1, 0)
  df$EncodeHouseStyle <- ifelse(df$HouseStyle == "1Story", 1, 0)
  df$EncodeBsmtExposure <- ifelse(df$BsmtExposure == "Gd", 1, 0)
  df$EncodeKitchenQual <- ifelse(df$KitchenQual == "TA", 1, 0)
  df$EncodeBsmtQual <- ifelse(df$BsmtQual == "Gd" | df$BsmtQual == "TA", 1, 0)
  df$BathToRoom <- (df$FullBath + df$HalfBath + df$BsmtHalfBath 
                    + df$BsmtFullBath) / df$BedroomAbvGr
  df$EncodeBldgType <- ifelse(df$BldgType == "Duplex", 1, 0)
  df$EncodeHouseStyle <- ifelse(df$HouseStyle == "1Story" | df$HouseStyle == "SFoyer", 1, 0)
  df$EncodedFoundation <- ifelse(df$Foundation == "PConc" | df$Foundation == "Stone", 1, 0)
  df$EncodeSaleType <- ifelse(df$SaleType == 'ConLD' | df$SaleType == 'New', 1, 0)
  df$EncodedSaleCondition <- ifelse(df$SaleCondition == 'Normal' | df$SaleType == 'Alloca', 1, 0)
  df$cent1 <- (df$SalePrice - mean(df$SalePrice, na.rm=TRUE)) * (df$EncodedSaleCondition - mean(df$EncodedSaleCondition))
  df[mapply(is.infinite, df)] <- NA
  df
}

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
fit.steps <- lm(df.train2.steps$SalePrice ~ ., data = df.train2.steps, na.action = na.exclude)

# backward
# olsrr::ols_stepaic_backward(fit.steps, details = T)
fit.backward <- lm(df.train2.steps$SalePrice ~ MSSubClass + MSZoning + LotArea + LotConfig + Neighborhood + Condition1 + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + Exterior1st + ExterCond + Foundation + BsmtQual + BsmtExposure + BsmtFinType1 + BsmtUnfSF + TotalBsmtSF + HeatingQC + CentralAir + X2ndFlrSF + GrLivArea + BsmtFullBath + FullBath + HalfBath + KitchenQual + TotRmsAbvGrd + Fireplaces + GarageCars + GarageArea + PavedDrive + OpenPorchSF + YrSold + SaleType + SaleCondition , data = df.train2.steps, na.action = na.exclude)
summary(fit.backward)

# olsrr::ols_stepaic_forward(fit.steps, details = T)
fit.forward <- lm(formula = df.train2.steps$SalePrice ~ OverallQual + GrLivArea + Neighborhood + TotalBsmtSF 
                  + OverallCond + GarageArea + BsmtUnfSF + YearBuilt + LotArea + MSZoning + KitchenQual 
                  + SaleCondition + SaleType + BsmtExposure + BldgType + Condition1 + BsmtQual + Fireplaces 
                  + Exterior1st + Foundation + CentralAir + GarageCars + MasVnrType + YearRemodAdd + BsmtFullBath 
                  + HalfBath + YrSold + LotConfig + ExterCond + PavedDrive + TotRmsAbvGrd, 
                  data = df.train2.steps, na.action = na.exclude)
summary(fit.forward)

# olsrr::ols_stepaic_both(fit.steps, details = T)
fit.both <- lm(formula = df.train2.steps$SalePrice ~ OverallQual + GrLivArea + Neighborhood + BsmtFullBath + OverallCond + GarageCars 
               + TotalBsmtSF + YearBuilt + LotArea + MSZoning + BsmtUnfSF + SaleCondition + KitchenQual + Condition1 + Exterior1st 
               + CentralAir + BsmtExposure + BldgType + Foundation + Fireplaces + BsmtQual + LotShape + YearRemodAdd + LotConfig + FullBath 
               + HalfBath + HouseStyle + X1stFlrSF + SaleType + BsmtFinType1 + YrSold + PavedDrive + HeatingQC, 
               data = df.train2.steps, na.action = na.exclude)
summary(fit.both)

# manual fit
df.train2.manual <- df.train2

df.train2.manual <- encodeData(df.train2.manual)

fit.manual <- lm(formula = SalePrice ~ LotArea + OverallQual  
                  + EncodeBsmtQual + EncodeBsmtExposure + GrLivArea + TotalBsmtSF
                 + BsmtUnfSF + BathToRoom + YearBuilt + MSZoning 
                 + OverallCond + EncodedFoundation + CentralAir + KitchenQual + Fireplaces 
                 + GarageCars + EncodeSaleType + EncodedSaleCondition + cent1, 
                 data = df.train2.manual, na.action = na.exclude)

summary(fit.manual)
vif(fit.manual)
plot(fit.manual)
hist(fit.manual$residuals)
plot(cooks.distance(fit.manual, data = df.train2.manual))

# alpha precitions for pre-validation
test <- encodeData(test)
test$PredPrice <- predict(fit.manual, newdata = subset(test, select = c(cent1, LotArea,OverallQual,EncodeBsmtQual,EncodeBsmtExposure,GrLivArea,TotalBsmtSF,BsmtUnfSF,BathToRoom,YearBuilt,MSZoning,OverallCond,EncodedFoundation,CentralAir,KitchenQual,Fireplaces,GarageCars,EncodeSaleType,EncodedSaleCondition)))
sqrt(mean((exp(test$PredPrice) - exp(test$SalePrice)) ^2, na.rm=TRUE))

# clean, transform, encode test data
t <- read.csv("~/Desktop/SMU_MSDS_Homework/Homework/6371/Project/train.csv")
t <- t[-c(1460), ]
df.test$SalePrice <- log(t$SalePrice)
df.test <- cleanData(df.test, isTrain = F)
df.test <- transformData(df.test, isTest = F)
df.test <- encodeData(df.test)

# generate predictions
df.test$PredPrice <- predict(fit.manual, newdata = subset(df.test, select = c(cent1, LotArea,OverallQual,EncodeBsmtQual,EncodeBsmtExposure,GrLivArea,TotalBsmtSF,BsmtUnfSF,BathToRoom,YearBuilt,MSZoning,OverallCond,EncodedFoundation,CentralAir,KitchenQual,Fireplaces,GarageCars,EncodeSaleType,EncodedSaleCondition)))
df.test$SalePrice <- exp(df.test$PredPrice)

# create kaggle data frame
kaggleColumns <- c("Id", "SalePrice")
df.kaggle <- df.test[kaggleColumns]
df.kaggle[mapply(is.na, df.kaggle)] <- exp(mean(df.train2.manual$SalePrice, na.rm=TRUE))
write.csv(x = df.kaggle, file = "~/Desktop/meacreatio_housing.csv", row.names = F)

# 0.16279









