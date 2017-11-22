# remove variables with near zero variance
df.train <- df.train[-nearZeroVar(df.train)]

# find high rate of missing values and remove those variables
colSums(is.na(df.train))
df.train$Alley <- NULL
df.train$PoolQC <- NULL
df.train$Fence <- NULL
df.train$FireplaceQu <- NULL