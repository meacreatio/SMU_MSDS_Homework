library("dplyr")

df.train2 <- read.csv("~/Desktop/SMU_MSDS_Homework/Homework/6371/Project/train.csv")
  
# remove extreme outliers
# df.train2 <- df.train2[!(df.train2$Id %in% c(1299,524)), ]

# remove variables with near zero variance
library(caret)
df.train2 <- df.train2[-nearZeroVar(df.train2)]

# find high rate of missing values and remove those variables
colSums(is.na(df.train2))
df.train2$Alley <- NULL
df.train2$PoolQC <- NULL
df.train2$Fence <- NULL
df.train2$FireplaceQu <- NULL

fit.full2 <- lm(df.train2$SalePrice ~ ., data = df.train2, na.action = na.exclude)
summary(fit.full2)

# look at the scatter plots to asset for normality ####################
df.train2.numeric <- select_if(df.train2, is.numeric)

df.plots <- melt(df.train2.numeric, "SalePrice")

ggplot(df.plots, aes(value, df.plots$SalePrice)) + 
  geom_point() + 
  facet_wrap(~variable, scales = "free")

hist(df.train2.numeric$SalePrice)
######################################################################

# transform values
df.train2$LotFrontage <- log(df.train2$LotFrontage)
df.train2$LotArea <- log(df.train2$LotArea)
df.train2$YearBuilt <- log(df.train2$YearBuilt)
df.train2$YearRemodAdd <- log(df.train2$YearRemodAdd)
df.train2$MasVnrArea <- log(df.train2$MasVnrArea)
df.train2$BsmtFinSF1 <- log(df.train2$BsmtFinSF1)
df.train2$BsmtUnfSf <- log(df.train2$BsmtUnfSF)
df.train2$TotalBsmtSF <- log(df.train2$TotalBsmtSF)
df.train2$X1stFlrSF <- log(df.train2$X1stFlrSF)
df.train2$X2stFlrSF <- log(df.train2$X2stFlrSF)
df.train2$GrLivArea <- log(df.train2$GrLivArea)
df.train2$GarageYrBlt <- log(df.train2$GarageYrBlt)
df.train2$GarageArea <- log(df.train2$GarageArea)
df.train2$WoodDeckSF < log(df.train2$WoodDeckSF)
df.train2$OpenPorchSF <- log(df.train2$OpenPorchSF)
df.train2$SalePrice <- log(df.train2$SalePrice)

# remove inf
df.train2[mapply(is.infinite, df.train2)] <- NA

# TODO create training set

# choose model 
df.train2.steps <-na.omit(df.train2)
fit.steps <- lm(df.train2.steps$SalePrice ~ ., data = df.train2.steps, na.action = na.exclude)
step(fit.steps, direction="backward")
fit.backward <- lm(formula = df.train2.steps$SalePrice ~ LotArea + Neighborhood + 
                     Condition1 + BldgType + OverallQual + OverallCond + Exterior1st + 
                     BsmtQual + BsmtUnfSF + TotalBsmtSF + HeatingQC + CentralAir + 
                     X1stFlrSF + GrLivArea + FullBath + KitchenQual + Fireplaces + 
                     GarageType + GarageYrBlt + GarageArea + MoSold + SaleCondition, 
                   data = df.train2.steps, na.action = na.exclude)
summary(fit.backward)



