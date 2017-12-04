source("helper_functions.R")
libraries <- c('car', 'Amelia', 'caret', 'ggfortify')
loadLibraries(libs = libraries)

df.train <- read.csv("train.csv")
df.train <- cleanData(df.train)

#Create plots prior to data transformation for inspection
df.trainG <- df.train[c("Id", "Neighborhood", "GrLivArea","SalePrice")]
df.trainG <- subset(df.trainG, Neighborhood == 'NAmes' | Neighborhood == 'Edwards' | Neighborhood == 'BrkSide')
df.trainoutliers <- df.trainG[(df.trainG$Id %in% c(1299, 643, 725, 524, 1424)), ]
df.trainoutliers$PPSF <- (df.trainoutliers$SalePrice / df.trainoutliers$GrLivArea)

#Calculate Price Per Square Foot for data set and by Neighborhood
AvgPriceperSqFt <- mean(df.trainG$SalePrice) / mean(df.trainG$GrLivArea)
df.PPSF <- df.trainG %>% group_by(Neighborhood) %>% summarise(SalePriceMean=mean(SalePrice), GrLivAreaMean=mean(GrLivArea))
df.PPSF$PPSF <- df.PPSF$SalePriceMean / df.PPSF$GrLivAreaMean

#Log transformation of SalesPrice and GrLivArea to allow for comparison
df.trainG$logSalePrice <- log(df.trainG$SalePrice)
df.trainG$logGrLivArea <- log(df.trainG$GrLivArea)
df.trainG$EncodeN <- ifelse(df.trainG$Neighborhood == "NAmes", 1, 0)
df.trainG$EncodeE <- ifelse(df.trainG$Neighborhood == "Edwards", 1, 0)
df.trainG$cent1 <- (df.trainG$logGrLivArea - mean(df.trainG$logGrLivArea)) * (df.trainG$EncodeN - mean(df.trainG$EncodeN))
df.trainG$cent2 <- (df.trainG$logGrLivArea - mean(df.trainG$logGrLivArea)) * (df.trainG$EncodeE - mean(df.trainG$EncodeE))

#linear linear with diagnostic plots
fit.trainG <- lm(SalePrice ~ GrLivArea + Neighborhood, data = df.trainG)
summary(fit.trainG)
autoplot(fit.trainG, which = 1:6)
ols_rsd_hist(fit.trainG)

#log log
fit.trainG <- lm(logSalePrice ~ logGrLivArea + Neighborhood, data = df.trainG)
summary(fit.trainG)
autoplot(fit.trainG, which = 1:6)
ols_rsd_hist(fit.trainG)

ameliatedData6 <- amelia(df.train,m=1, p2s=1, ords = c("MSZoning", "LotShape", "LotConfig", "Neighborhood", "Condition1", 
                                                       "BldgType", "HouseStyle", "RoofStyle", "ExteriorFirst", "Exterior2nd", 
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
confint(fit.reduced)

# compare the two models -> including neighborhood contibutes significantly to the model
anova(fit.full, fit.reduced)

# view the data
plot(fit.reduced)
olsrr::ols_rsd_hist(fit.reduced)
plot(cooks.distance(fit.reduced, data=df.filtered))

# k fold
df.train1.kfold <- df.filtered
set.seed(17)
formula.q1 <- as.formula(logSalePrice ~ LogGrLiveArea + Neighborhood, env = new.env())
kfold.q1 <- kfold(lmFormula = formula.q1, df = df.train1.kfold)
rss(kfold.q1)
