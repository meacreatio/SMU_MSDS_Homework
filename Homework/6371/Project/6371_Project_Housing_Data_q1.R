library(car)
library(Amelia)
source("~/Desktop/SMU_MSDS_Homework/Homework/6371/Project/helper_functions.R")

df.train <- read.csv("~/Desktop/SMU_MSDS_Homework/Homework/6371/Project/train.csv")
df.train <- cleanData(df.train)
ameliatedData6 <- amelia(df.train,m=1, p2s=1, ords = c("MSZoning", "LotShape", "LotConfig", "Neighborhood", "Condition1", 
                                                                "BldgType", "HouseStyle", "RoofStyle", "Exterior1st", "Exterior2nd", 
                                                                "MasVnrType", "ExterQual", "ExterCond", "Foundation", "BsmtQual", 
                                                                "BsmtExposure", "BsmtFinType1", "HeatingQC", "CentralAir", "Electrical", 
                                                                "KitchenQual", "GarageType", "GarageFinish", "PavedDrive", "SaleType", 
                                                                "SaleCondition"))

write.amelia(obj=ameliatedData6, file.stem="q1_data")
df.train <- read.csv("q1_data1.csv")

# handle only NAmes, Edwards, BrkSide
df.filtered <- df.train[df.train$Neighborhood == "NAmes" | df.train$Neighborhood == "Edwards" | df.train$Neighborhood == "BrkSide", ]



fit.full <- lm(df.filtered$SalePrice ~ df.filtered$GrLivArea + df.filtered$Neighborhood, data = df.filtered)
summary(fit.full)

# check plots -> need log transform
scatterplotMatrix(~df.filtered$SalePrice + df.filtered$GrLivArea, data=df.filtered)

# convert sales to logSales
df.filtered$logSalePrice <- log(df.filtered$SalePrice)
fit.full <- lm(df.filtered$logSalePrice ~ df.filtered$GrLivArea + df.filtered$Neighborhood, data = df.filtered)
summary(fit.full)

# account for sqrft in 100's
df.filtered$GrLivArea <- df.filtered$GrLivArea / 100

#convert to logGrLivArea
df.filtered$LogGrLiveArea <- log(df.filtered$GrLivArea)
fit.full <- lm(df.filtered$logSalePrice ~ df.filtered$LogGrLiveArea + df.filtered$Neighborhood, data = df.filtered)
summary(fit.full)

# aggregate based on neighborhoods 
df.filtered$EncodeN <- ifelse(df.filtered$Neighborhood == "NAmes", 1, 0)
df.filtered$EncodeE <- ifelse(df.filtered$Neighborhood == "Edwards", 1, 0)

df.filtered$int1 <- df.filtered$EncodeN * df.filtered$GrLivArea
df.filtered$int2 <- df.filtered$EncodeE * df.filtered$GrLivArea

#temp interaction
fit.full <- lm(df.filtered$logSalePrice ~ df.filtered$LogGrLiveArea + df.filtered$EncodeN + df.filtered$EncodeE + df.filtered$int1 + df.filtered$int2, data = df.filtered)
summary(fit.full)
vif(fit.full)

# scatterplotMatrix(~df.filtered$logSalePrice + df.filtered$LogGrLiveArea + df.filtered$Neighborhood, data=df.filtered)

# add centering since VIF from interaction is very high
df.filtered$cent1 <- (df.filtered$LogGrLiveArea - mean(df.filtered$LogGrLiveArea)) * (df.filtered$EncodeN - mean(df.filtered$EncodeN))
df.filtered$cent2 <- (df.filtered$LogGrLiveArea - mean(df.filtered$LogGrLiveArea)) * (df.filtered$EncodeE - mean(df.filtered$EncodeE))
fit.full <- lm(df.filtered$logSalePrice ~ df.filtered$LogGrLiveArea + df.filtered$EncodeN + df.filtered$EncodeE + df.filtered$cent1 + df.filtered$cent2, data = df.filtered)
summary(fit.full)
vif(fit.full)

# remove EncodeE and cent2 due to their high p-values
fit.reduced <- lm(df.filtered$logSalePrice ~ df.filtered$LogGrLiveArea + df.filtered$EncodeN + df.filtered$cent1, data = df.filtered)
summary(fit.reduced)

# compare the two models -> including neighborhood contibutes significantly to the model
anova(fit.full, fit.reduced)

# view the data
plot(fit.reduced)
olsrr::ols_rsd_hist(fit.reduced)
plot(cooks.distance(fit.reduced, data=df.filtered))

# k fold
df.train1.kfold <- df.filtered
set.seed(17)
model1 <- train(
  logSalePrice ~ LogGrLiveArea + Neighborhood, data = df.train1.kfold,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  ), na.action = na.omit
)
model1
summary(model1$finalModel)
# Residual standard error: 0.1936 on 376 degrees of freedom
# Multiple R-squared:  0.5034,	Adjusted R-squared:  0.4995 
# F-statistic: 127.1 on 3 and 376 DF,  p-value: < 2.2e-16
rss(model1$finalModel)
