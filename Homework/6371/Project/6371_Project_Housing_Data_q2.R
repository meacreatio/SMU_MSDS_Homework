library(dplyr)
library(MASS)
library(olsrr)
library(car)
library(caret)
library(caTools)

df.train2 <- read.csv("~/Desktop/SMU_MSDS_Homework/Homework/6371/Project/train.csv")

df.train2 <- df.train2[df.train2$LotShape != 'IR3', ] 
# remove extreme outliers
df.train2 <- df.train2[!(df.train2$Id %in% c(1299, 524)), ]

# remove variables with near zero variance
df.train2 <- df.train2[-nearZeroVar(df.train2)]

# get train and test
set.seed(101) 
sample = sample.split(df.train2, SplitRatio = .8)

train = subset(df.train2, sample == TRUE)
test  = subset(df.train2, sample == FALSE)

df.train2 <- train

# find high rate of missing values and remove those variables
colSums(is.na(df.train2))
df.train2$Alley <- NULL
df.train2$PoolQC <- NULL
df.train2$Fence <- NULL
df.train2$FireplaceQu <- NULL

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
df.train2$LotFrontage <- log(df.train2$LotFrontage)
df.train2$LotArea <- log(df.train2$LotArea)
df.train2$MasVnrArea <- log(df.train2$MasVnrArea)
df.train2$BsmtFinSF1 <- log(df.train2$BsmtFinSF1)
df.train2$BsmtUnfSf <- log(df.train2$BsmtUnfSF)
df.train2$TotalBsmtSF <- log(df.train2$TotalBsmtSF)
df.train2$X1stFlrSF <- log(df.train2$X1stFlrSF)

v <- sapply(X = df.train2$X2ndFlrSF, function(x) {
  value = 0
  if (x != 0) {
    value = log(x)
  }
  value
})
df.train2$X2ndFlrSF <- v

df.train2$GrLivArea <- log(df.train2$GrLivArea)
df.train2$GarageYrBlt <- log(df.train2$GarageYrBlt)
df.train2$GarageArea <- log(df.train2$GarageArea)
df.train2$WoodDeckSF < log(df.train2$WoodDeckSF)
df.train2$OpenPorchSF <- log(df.train2$OpenPorchSF)
df.train2$SalePrice <- log(df.train2$SalePrice)

# remove inf
df.train2[mapply(is.infinite, df.train2)] <- NA

# choose model 
df.train2.steps <-na.omit(df.train2)
fit.steps <- lm(df.train2.steps$SalePrice ~ ., data = df.train2.steps, na.action = na.exclude)

# backward
olsrr::ols_stepaic_backward(fit.steps, details = T)
fit.backward <- lm(df.train2.steps$SalePrice ~ MSSubClass + LotArea + LotShape + LotConfig + Condition1 + BldgType + OverallQual + OverallCond + YearBuilt + Exterior2nd + MasVnrType + BsmtQual + BsmtExposure + BsmtUnfSF + TotalBsmtSF + X1stFlrSF + X2ndFlrSF + GrLivArea + BsmtHalfBath + FullBath + BedroomAbvGr + Fireplaces + GarageYrBlt + GarageCars + PavedDrive + WoodDeckSF + OpenPorchSF + SaleCondition, 
                   data = df.train2.steps, na.action = na.exclude)
summary(fit.backward)

olsrr::ols_stepaic_forward(fit.steps, details = T)
fit.forward <- lm(formula = df.train2.steps$SalePrice ~ OverallQual + GrLivArea + TotalBsmtSF + BsmtUnfSF + GarageCars + SaleCondition + BedroomAbvGr + LotArea + YearRemodAdd + BsmtExposure + MSSubClass + BsmtQual + YearBuilt + MSZoning + OverallCond + BsmtHalfBath + ExterCond + BldgType + GarageYrBlt + BsmtFullBath + LotConfig + FullBath + Exterior2nd + Condition1 + X2ndFlrSF + Electrical + GarageFinish + Fireplaces, 
                  data = df.train2.steps, na.action = na.exclude)
summary(fit.forward)

olsrr::ols_stepaic_both(fit.steps, details = T)
fit.both <- lm(formula = df.train2.steps$SalePrice ~ OverallQual + GrLivArea + TotalBsmtSF + BsmtUnfSF + SaleCondition + BedroomAbvGr + LotArea + BsmtExposure + MSSubClass + BsmtQual + YearBuilt + MSZoning + OverallCond + BsmtFinType1 + Fireplaces, data = df.train2.steps, na.action = na.exclude)
summary(fit.both)

# manual fit
df.train2.manual <- na.omit(df.train2)

df.train2.manual$EncodeLotShape <- as.numeric(df.train2.manual$LotShape)
df.train2.manual$EncodeNeighborhood <- ifelse(df.train2.manual$Neighborhood == "NoRidge" | df.train2.manual$Neighborhood == "NridgHt" 
                                        | df.train2.manual$Neighborhood == "StoneBr", 1, 0)
df.train2.manual$EncodeHouseStyle <- ifelse(df.train2.manual$HouseStyle == "1Story", 1, 0)
df.train2.manual$EncodeBsmtExposure <- ifelse(df.train2.manual$BsmtExposure == "Gd", 1, 0)
df.train2.manual$EncodeKitchenQual <- ifelse(df.train2.manual$KitchenQual == "TA", 1, 0)
df.train2.manual$EncodeBsmtQual <- ifelse(df.train2.manual$BsmtQual == "Gd" | df.train2.manual$BsmtQual == "Fa", 1, 0)
df.train2.manual$BathToRoom <- (df.train2.manual$FullBath + df.train2.manual$HalfBath + df.train2.manual$BsmtHalfBath + df.train2.manual$BsmtFullBath) / df.train2.manual$BedroomAbvGr

# add interaction
# df.train2.manual$int1 <- df.train2.manual$EncodeNeighborhood * df.train2.manual$OverallQual

fit.manual <- lm(formula = df.train2.manual$SalePrice ~ LotArea + OverallQual  
                  + df.train2.manual$EncodeBsmtQual + df.train2.manual$EncodeBsmtExposure + GrLivArea + TotalBsmtSF
                 + BsmtUnfSF + BathToRoom + YearBuilt, 
                 data = df.train2.manual, na.action = na.exclude)
summary(fit.manual)

