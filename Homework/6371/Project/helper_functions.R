rss <- function(model) {
  sum(residuals(model)^2, na.rm = T)
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

transformData <- function(df, isTest = F) {
  df$LotFrontage <- log(df$LotFrontage)
  df$LotArea <- log(df$LotArea)
  df$MasVnrArea <- log(df$MasVnrArea)
  df$BsmtFinSF1 <- log(df$BsmtFinSF1)
  df$BsmtUnfSf <- log(df$BsmtUnfSF)
  df$TotalBsmtSF <- log(df$TotalBsmtSF)
  df$X1stFlrSF <- log(df$X1stFlrSF)
  
  v <- sapply(X = df$X2ndFlrSF, function(x) {
    value = 0
    if (x != 0) {
      value = log(x)
    }
    value
  })
  df$X2ndFlrSF <- v
  
  df$GrLivArea <- log(df$GrLivArea)
  df$GarageArea <- log(df$GarageArea)
  if(isTest == F) {
    df$SalePrice <- log(df$SalePrice)
  }
  
  df[mapply(is.infinite, df)] <- NA
  
  df
}

encodeData <- function(df) {
  df$EncodeLotShape <- as.numeric(df$LotShape)
  df$EncodeNeighborhood <- ifelse(df$Neighborhood == "MeadowV", 1, 0)
  df$EncodeHouseStyle <- ifelse(df$HouseStyle == "1Story", 1, 0)
  df$EncodeBsmtExposure <- ifelse(df$BsmtExposure == "Gd", 1, 0)
  df$EncodeKitchenQual <- ifelse(df$KitchenQual == "TA", 1, 0)
  df$EncodeBsmtQual <- ifelse(df$BsmtQual == "Gd" | df$BsmtQual == "TA", 1, 0)
  df$BathToRoom <- (df$FullBath + df$HalfBath + df$BsmtHalfBath 
                    + df$BsmtFullBath) / df$BedroomAbvGr
  df$EncodeBldgType <- ifelse(df$BldgType == "Duplex", 1, 0)
  df$EncodeHouseStyle <- ifelse(df$HouseStyle == "1Story" | df$HouseStyle == "SFoyer", 1, 0)
  df$EncodedFoundation <- ifelse(df$Foundation == "PConc" | df$Foundation == "Stone", 1, 0)
  df$EncodeSaleType <- ifelse(df$SaleType == 'ConLD' | df$SaleType == 'New', 1, 0)
  df$EncodedSaleCondition <- ifelse(df$SaleCondition == 'Normal' | df$SaleCondition == 'Partial' 
                                    | df$SaleCondition == 'AdjLand', 1, 0)
  df$cent1 <- (df$SalePrice - mean(df$SalePrice, na.rm=TRUE)) * (df$EncodedSaleCondition - mean(df$EncodedSaleCondition))
  df$EncodeNeighborhood <- ifelse(df$Neighborhood == 'StoneBr' | df$Neighborhood == 'Crawfor' 
                                  | df$Neighborhood == 'NoRidge', 1, 0)
  df$EncodeNeighborhoodL <- ifelse(df$Neighborhood == 'MeadowV' | df$Neighborhood == 'Mitchel' 
                                   | df$Neighborhood == 'Edwards' | df$Neighborhood == 'NWAmes', 1, 0)
  df$EncodeCondition1 <- ifelse(df$Condition1 == 'Norm' | df$Neighborhood == 'PosN', 1, 0)
  df$EncodeCondition1L <- ifelse(df$Condition1 == 'RRAe', 1, 0)
  df$EncodeLotConfig <- ifelse(df$LotConfig == 'FR2', 1, 0)
  df[mapply(is.infinite, df)] <- NA
  df
}

kfold <- function(lmFormula, df) {
  model <- train(
    lmFormula, data = df,
    method = "lm",
    trControl = trainControl(
      method = "cv", number = 10,
      verboseIter = TRUE
    ), na.action = na.omit
  )
  model$finalModel
}