rss <- function(model) {
  sum(residuals(model)^2)
}

cleanData <- function(df, isTrain = T) {
  if(isTrain == T) {
    df <- df[df$LotShape != 'IR3', ] 
    # remove extreme outliers
    df <- df[!(df$Id %in% c(1299, 524, 31, 463, 633, 89, 589, 496, 682, 813, 969)), ]
  }
  
  # remove variables with near zero variance
  df <- df[-nearZeroVar(df)]
  
  # find high rate of missing values and remove those variables
  colSums(is.na(df))
  df$Alley <- NULL
  df$PoolQC <- NULL
  df$Fence <- NULL
  df$FireplaceQu <- NULL
  df$OpenPorchSF <- NULL
  df$WoodDeckSF <- NULL
  df$Street <- NULL
  df$LandContour <- NULL
  df$LandSlope <- NULL
  df$Condition2 <- NULL
  df$RoofMatl <- NULL
  df$BsmtCond <- NULL
  df$BsmtFinType2 <- NULL
  df$Heating <- NULL
  df$Functional <- NULL
  df$GarageQual <- NULL
  df$GarageCond <- NULL
  df$MiscFeature  <- NULL
  df$Utilities <- NULL
  df
}