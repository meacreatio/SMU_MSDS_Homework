df.train <- read.csv("~/Desktop/SMU_MSDS_Homework/Homework/6371/Project/train.csv")

# handle only NAmes, Edwards, BrkSide
df.filtered <- df.train[df.train$Neighborhood == "NAmes" | df.train$Neighborhood == "Edwards" | df.train$Neighborhood == "BrkSide", ]

# df.filtered <- df.filtered[!(df.filtered$Id %in% c(1299,524)), ]

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

# aggregate based on neighborhoods that have a positive impact on house prices relative to the others
df.filtered$Neighborhood <- ifelse(df.filtered$Neighborhood == 'NAmes', 1, 0)

# view the data
olsrr::ols_rpc_plot(fit.full)
# scatterplotMatrix(~df.filtered$logSalePrice + df.filtered$LogGrLiveArea + df.filtered$Neighborhood, data=df.filtered)

# add interaction
df.filtered$Neighborhood <- as.numeric(df.filtered$Neighborhood)
df.filtered$int1 <- NULL
fit.full <- lm(df.filtered$logSalePrice ~ df.filtered$LogGrLiveArea + df.filtered$Neighborhood + df.filtered$Neighborhood : df.filtered$logSalePrice, data = df.filtered)
summary(fit.full)

# add centering since VIF from interaction is very high
df.filtered$cent1 <- (df.filtered$logSalePrice - mean(df.filtered$logSalePrice)) * (df.filtered$Neighborhood - mean(df.filtered$Neighborhood))
fit.full <- lm(df.filtered$logSalePrice ~ df.filtered$LogGrLiveArea + df.filtered$Neighborhood + df.filtered$cent1, data = df.filtered)
summary(fit.full)
vif(fit.full)

# compare the two models -> including neighborhood contibutes significantly to the model
fit.reduced <- lm(df.filtered$logSalePrice ~ df.filtered$LogGrLiveArea, data = df.filtered)
anova(fit.full, fit.reduced)

plot(fit.full)

