df.train <- read.csv("~/Desktop/SMU_MSDS_Homework/Homework/6371/Project/train.csv")

# handle only NAmes, Edwards, BrkSide
df.filtered <- df.train[df.train$Neighborhood == "NAmes" | df.train$Neighborhood == "Edwards" | df.train$Neighborhood == "BrkSide", ]

# remove extreme outliers
df.filtered <- df.filtered[!(df.filtered$Id %in% c(1299,524)), ]

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

# use the reduced model
# view the data
olsrr::ols_rpc_plot(fit.full)


plot(fit.reduced)

