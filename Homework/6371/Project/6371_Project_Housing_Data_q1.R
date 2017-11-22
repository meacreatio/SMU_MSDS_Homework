df.train <- read.csv("~/Desktop/SMU_MSDS_Homework/Homework/6371/Project/train.csv")

# remove variables with near zero variance
df.train <- df.train[-nearZeroVar(df.train)]

# find high rate of missing values and remove those variables
colSums(is.na(df.train))
df.train$Alley <- NULL
df.train$PoolQC <- NULL
df.train$Fence <- NULL
df.train$FireplaceQu <- NULL

# handle only NAmes, Edwards, BrkSide
df.reduced <- df.train[df.train$Neighborhood == "NAmes" | df.train$Neighborhood == "Edwards" | df.train$Neighborhood == "BrkSide", ]

# convert sales to logSales
df.reduced$logSalePrice <- log(df.reduced$SalePrice)

fit.train <- lm(df.reduced$logSalePrice ~ ., data = df.reduced)
summary(fit.train)
