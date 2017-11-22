df.train <- read.csv("~/Desktop/SMU_MSDS_Homework/Homework/6371/Project/train.csv")

# handle only NAmes, Edwards, BrkSide
df.filtered <- df.train[df.train$Neighborhood == "NAmes" | df.train$Neighborhood == "Edwards" | df.train$Neighborhood == "BrkSide", ]

fit.full <- lm(df.filtered$SalePrice ~ df.filtered$GrLivArea + df.filtered$Neighborhood, data = df.filtered)
summary(fit.full)

# convert sales to logSales
df.filtered$logSalePrice <- log(df.filtered$SalePrice)
fit.full <- lm(df.filtered$logSalePrice ~ df.filtered$GrLivArea + df.filtered$Neighborhood, data = df.filtered)
summary(fit.full)

#convert to logGrLivArea
df.filtered$LogGrLiveArea <- log(df.filtered$GrLivArea)
fit.full <- lm(df.filtered$logSalePrice ~ df.filtered$LogGrLiveArea + df.filtered$Neighborhood, data = df.filtered)
summary(fit.full)

# add interaction
df.filtered$Neighborhood <- as.numeric(df.filtered$Neighborhood)
df.filtered$int1 <- NULL
fit.full <- lm(df.filtered$logSalePrice ~ df.filtered$LogGrLiveArea + df.filtered$Neighborhood + df.filtered$Neighborhood : df.filtered$logSalePrice, data = df.filtered)
summary(fit.full)

# add centering
df.filtered$cent1 <- (df.filtered$logSalePrice - mean(df.filtered$logSalePrice)) * (df.filtered$Neighborhood - mean(df.filtered$Neighborhood))
fit.full <- lm(df.filtered$logSalePrice ~ df.filtered$LogGrLiveArea + df.filtered$Neighborhood + df.filtered$cent1, data = df.filtered)
summary(fit.full)
vif(fit.full)


# create full and reduced models
fit.full <- lm(df.filtered$logSalePrice ~ df.filtered$logGrLivArea + df.filtered$Neighborhood, data = df.filtered)
fit.reduced <- lm(df.filtered$logSalePrice ~ df.filtered$GrLivArea, data = df.filtered)

# check if the inclusion of Neighborhood matters -> it does
anova(fit.full, df.filtered)

