loadLibraries <- function(libs) {
  missingLibs <- libs[!libs %in% installed.packages()]
  for(l in missingLibs) {
    install.packages(l, dependences = TRUE)
  }
  sapply(libs, require, character = TRUE)
}

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
  df$ExteriorFirst <- df$Exterior1st
  df$Exterior1st <- NULL
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
  df$EncodedFoundation <- ifelse(df$Foundation == "PConc" | df$Foundation == "Slab", 1, 0)
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
  df$EncodeHeatingQC <- ifelse(df$HeatingQC == 'TA', 1, 0)
  df$cent2 <- (df$SalePrice - mean(df$SalePrice, na.rm=TRUE)) * (df$OverallQual - mean(df$OverallQual))
  df$cent3 <- (df$SalePrice - mean(df$SalePrice, na.rm=TRUE)) * (df$GrLivArea - mean(df$GrLivArea))
  df$EncodeExteriorFirst <- ifelse(df$ExteriorFirst == 'BrkFace', 1, 0)
  
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

# credit to sperrorest package
remove_missing_levels <- function(fit, test_data) {
  
  # https://stackoverflow.com/a/39495480/4185785
  
  # drop empty factor levels in test data
  test_data %>%
    droplevels() %>%
    as.data.frame() -> test_data
  
  # 'fit' object structure of 'lm' and 'glmmPQL' is different so we need to
  # account for it
  if (any(class(fit) == "glmmPQL")) {
    # Obtain factor predictors in the model and their levels
    factors <- (gsub("[-^0-9]|as.factor|\\(|\\)", "",
                     names(unlist(fit$contrasts))))
    # do nothing if no factors are present
    if (length(factors) == 0) {
      return(test_data)
    }
    
    map(fit$contrasts, function(x) names(unmatrix(x))) %>%
      unlist() -> factor_levels
    factor_levels %>% str_split(":", simplify = TRUE) %>%
      extract(, 1) -> factor_levels
    
    model_factors <- as.data.frame(cbind(factors, factor_levels))
  } else {
    # Obtain factor predictors in the model and their levels
    factors <- (gsub("[-^0-9]|as.factor|\\(|\\)", "",
                     names(unlist(fit$xlevels))))
    # do nothing if no factors are present
    if (length(factors) == 0) {
      return(test_data)
    }
    
    factor_levels <- unname(unlist(fit$xlevels))
    model_factors <- as.data.frame(cbind(factors, factor_levels))
  }
  
  # Select column names in test data that are factor predictors in
  # trained model
  
  predictors <- names(test_data[names(test_data) %in% factors])
  
  # For each factor predictor in your data, if the level is not in the model,
  # set the value to NA
  
  for (i in 1:length(predictors)) {
    found <- test_data[, predictors[i]] %in% model_factors[
      model_factors$factors == predictors[i], ]$factor_levels
    if (any(!found)) {
      # track which variable
      var <- predictors[i]
      # set to NA
      test_data[!found, predictors[i]] <- NA
      # drop empty factor levels in test data
      test_data %>%
        droplevels() -> test_data
      # issue warning to console
      message(sprintf(paste0("Setting missing levels in '%s', only present",
                             " in test data but missing in train data,",
                             " to 'NA'."),
                      var))
    }
  }
  return(test_data)
}