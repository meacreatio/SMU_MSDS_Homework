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

