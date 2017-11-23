library("dplyr")

df.train2 <- read.csv("~/Desktop/SMU_MSDS_Homework/Homework/6371/Project/train.csv")
  
# remove extreme outliers
df.train2 <- df.train2[!(df.train2$Id %in% c(1299,524)), ]

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

# look at the scatter plots to asset for normality
df.train2.numeric <- select_if(df.train2, is.numeric)

pairs(df.train2.numeric)
                  
df.plots <- melt(df.train2.numeric, "SalePrice")

ggplot(df.plots, aes(value, df.plots$SalePrice)) + 
  geom_point() + 
  facet_wrap(~variable, scales = "free")

df.train2.steps <-na.omit(df.train2)
fit.steps <- lm(df.train2.steps$SalePrice ~ ., data = df.train2.steps, na.action = na.exclude)
step(fit.steps, direction="backward")
fit.backward <- lm(formula = df.train2.steps$SalePrice ~ MSZoning + LotFrontage + 
                        LotArea + LotShape + LotConfig + Neighborhood + BldgType + 
                        HouseStyle + OverallQual + OverallCond + YearBuilt + RoofStyle + 
                        MasVnrType + MasVnrArea + ExterQual + BsmtQual + BsmtExposure + 
                        BsmtFinSF1 + TotalBsmtSF + CentralAir + X2ndFlrSF + GrLivArea + 
                        BedroomAbvGr + KitchenQual + GarageYrBlt + GarageCars + OpenPorchSF + 
                        SaleCondition, data = df.train2.steps, na.action = na.exclude)
summary(fit.backward)
vif(fit.backward)


