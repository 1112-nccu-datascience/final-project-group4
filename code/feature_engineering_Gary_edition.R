library(dplyr)
library(stringr)

freq_encoding <- function(x){
  frequency_table  <- table(x)
  applied_data_table <- frequency_table[x]
  return(as.numeric(applied_data_table))
}

get_mode <- function(x){
  freq_table <- table(x)
  return(names(freq_table)[which.max(freq_table)])
}

data_process <- function(dataset, mode){
  # # 找出有缺失值的欄位
  # na.cols <- which(colSums(is.na(dataset)) > 0)  # 找出有缺失值的欄位索引
  # sort(colSums(sapply(dataset[na.cols], is.na)), decreasing = TRUE)  # 根據缺失值的數量進行排序
  # paste('There are', length(na.cols), 'columns with missing values')  # 輸出有缺失值的欄位數量
  
  # 將缺失值填補為 0
  dataset$LotFrontage[is.na(dataset$LotFrontage)] <- 0
  dataset$MasVnrArea[is.na(dataset$MasVnrArea)] <- 0
  dataset$BsmtFinSF1[is.na(dataset$BsmtFinSF1)] <- 0
  dataset$BsmtFinSF2[is.na(dataset$BsmtFinSF2)] <- 0
  dataset$BsmtUnfSF[is.na(dataset$BsmtUnfSF)] <- 0
  dataset$TotalBsmtSF[is.na(dataset$TotalBsmtSF)] <- 0
  dataset$BsmtFullBath[is.na(dataset$BsmtFullBath)] <- 0
  dataset$BsmtHalfBath[is.na(dataset$BsmtHalfBath)] <- 0
  dataset$GarageCars[is.na(dataset$GarageCars)] <- 0
  dataset$GarageArea[is.na(dataset$GarageArea)] <- 0
  # 
  # # 假設車庫建造的年份與房屋本身建造的年份相同
  dataset$GarageYrBlt[is.na(dataset$GarageYrBlt)] <- 0  
  dataset$GarageYrBlt[dataset$GarageYrBlt==2207] <- 2007  # 將 "GarageYrBlt" 欄位中錯誤的值 2207 修正為 2007(remodel year)
  # 將缺失值填補為該特徵中出現頻率最高的值 (仍為缺失值)
  dataset$KitchenQual[is.na(dataset$KitchenQual)] <- names(sort(-table(dataset$KitchenQual)))[1]
  dataset$MSZoning[is.na(dataset$MSZoning)] <- names(sort(-table(dataset$MSZoning)))[1]  
  dataset$SaleType[is.na(dataset$SaleType)] <- names(sort(-table(dataset$SaleType)))[1]  
  dataset$Exterior1st[is.na(dataset$Exterior1st)] <- names(sort(-table(dataset$Exterior1st)))[1]  
  dataset$Exterior2nd[is.na(dataset$Exterior2nd)] <- names(sort(-table(dataset$Exterior2nd)))[1] 
  dataset$Functional[is.na(dataset$Functional)] <- names(sort(-table(dataset$Functional)))[1]
  dataset$Utilities[is.na(dataset$Utilities)] <- names(sort(-table(dataset$Utilities)))[1]
  
  # 對於空值，我們將將 'NA' 值改為一個新的值 - 'No'
  dataset$Alley[is.na(dataset$Alley)] = "No"
  
  # Bsmt : 地下室相關特徵的 NA 代表「沒有地下室」
  dataset$BsmtQual[is.na(dataset$BsmtQual)] = "No"
  
  dataset$BsmtCond[is.na(dataset$BsmtCond)] = "No"
  
  dataset$BsmtExposure[is.na(dataset$BsmtExposure)] = "No"  
  
  dataset$BsmtFinType1[is.na(dataset$BsmtFinType1)] = "No"
  
  dataset$BsmtFinType2[is.na(dataset$BsmtFinType2)] = "No"
  
  # Fence : NA 代表「沒有圍牆」
  dataset$Fence[is.na(dataset$Fence)] = "No"
  
  # FireplaceQu : NA 代表「沒有壁爐」
  dataset$FireplaceQu[is.na(dataset$FireplaceQu)] = "No"
  
  # Garage : 車庫類型相關特徵的 NA 代表「沒有車庫」
  dataset$GarageType[is.na(dataset$GarageType)] = "No"
  dataset$GarageFinish[is.na(dataset$GarageFinish)] = "No"  
  dataset$GarageQual[is.na(dataset$GarageQual)] = "No"
  dataset$GarageCond[is.na(dataset$GarageCond)] = "No"
  
  # MasVnrType : NA 代表沒有石裝飾品
  dataset$MasVnrType[is.na(dataset$MasVnrType)] = "No"
  
  # MiscFeature : NA 代表「沒有其他雜項特徵」
  dataset$MiscFeature[is.na(dataset$MiscFeature)] = "No"
  
  # PoolQC : 根據資料描述，NA 代表「沒有游泳池」
  dataset$PoolQC[is.na(dataset$PoolQC)] = "No"
  
  # Electrical : NA 代表「未知」
  dataset$Electrical[is.na(dataset$Electrical)] = "UNK"
  
  # 再次檢查是否有空值
  na.cols <- which(colSums(is.na(dataset)) > 0)
  paste('現在有', length(na.cols), '個含有缺失值的欄位')
  
  # # 用圖表檢視和處理資料中的異常值
  # plot(training_data$SalePrice, training_data$GrLivArea)
  # plot(training_data$SalePrice, training_data$LotArea)
  # plot(training_data$SalePrice, training_data$X1stFlrSF)
  # plot(training_data$SalePrice, training_data$X2ndFlrSF)
  # plot(training_data$SalePrice, training_data$LowQualFinSF)
  # plot(training_data$SalePrice, training_data$TotalBsmtSF)
  # plot(training_data$SalePrice, training_data$MiscVal)
  # 通過檢視這些圖表，我們可以觀察到以下情況：
  # X2ndFlrSF 沒有顯著的異常值。MiscVal 和 LowQualFinSF 沒有異常值存在。
  # 我們將其餘的異常值轉換為各變數的平均值。
  dataset$GrLivArea[dataset$GrLivArea>4000] <- mean(dataset$GrLivArea)%>%as.numeric
  dataset$LotArea[dataset$LotArea>35000] <- mean(dataset$LotArea)%>%as.numeric
  dataset$X1stFlrSF[dataset$X1stFlrSF>3000] <- mean(dataset$X1stFlrSF)%>%as.numeric
  dataset$TotalBsmtSF[dataset$TotalBsmtSF>2900] <- mean(dataset$TotalBsmtSF)%>%as.numeric
  
  #用數字代替文字來分裡面的等級
  dataset$ExterQual<- recode(dataset$ExterQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
  dataset$ExterCond<- recode(dataset$ExterCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
  dataset$BsmtQual<- recode(dataset$BsmtQual,"No"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
  dataset$BsmtCond<- recode(dataset$BsmtCond,"No"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
  dataset$BsmtExposure<- recode(dataset$BsmtExposure,"No"=0,"No"=1,"Mn"=2,"Av"=3,"Gd"=5)
  dataset$BsmtFinType1<- recode(dataset$BsmtFinType1,"No"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
  dataset$BsmtFinType2<- recode(dataset$BsmtFinType2,"No"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
  dataset$HeatingQC<- recode(dataset$HeatingQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
  dataset$KitchenQual<- recode(dataset$KitchenQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
  dataset$Functional<- recode(dataset$Functional,"None"=0,"Sev"=1,"Maj2"=2,"Maj1"=3,"Mod"=4,"Min2"=5,"Min1"=6,"Typ"=7)
  dataset$FireplaceQu<- recode(dataset$FireplaceQu,"No"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
  dataset$GarageFinish<- recode(dataset$GarageFinish,"No"=0,"Unf"=1,"RFn"=2,"Fin"=3)
  dataset$GarageQual<- recode(dataset$GarageQual,"No"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
  dataset$GarageCond<- recode(dataset$GarageCond,"No"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
  dataset$PoolQC<- recode(dataset$PoolQC,"No"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
  dataset$Fence<- recode(dataset$Fence,"No"=0,"MnWw"=1,"GdWo"=2,"MnPrv"=3,"GdPrv"=4)
  
  # 辨別裡面數據裡面相關係數(材質之類的)是好或是壞 <3是壞 >=3是好 根據前面設定的等級來判別
  dataset['IsExterQualBad'] <- ifelse(dataset$ExterQual< 3, 1, 0)
  dataset['IsExterCondlBad'] <- ifelse(dataset$ExterCond< 3, 1, 0)
  dataset['IsBsmtQualBad'] <- ifelse(dataset$BsmtQual< 3, 1, 0)
  dataset['IsBsmtCondBad'] <- ifelse(dataset$BsmtCond< 3, 1, 0)
  dataset['IsBsmtExposureBad'] <- ifelse(dataset$BsmtExposure< 3, 1, 0)
  dataset['IsHeatingQCBad'] <- ifelse(dataset$HeatingQC< 3, 1, 0)
  dataset['IsKitchenQualBad'] <- ifelse(dataset$KitchenQual< 3, 1, 0)
  dataset['IsFireplaceQuBad'] <- ifelse(dataset$FireplaceQu< 3, 1, 0)
  dataset['IsGarageQualBad'] <- ifelse(dataset$GarageQual< 3, 1, 0)
  dataset['IsGarageCondBad'] <- ifelse(dataset$GarageCond< 3, 1, 0)
  dataset['IsPoolQCBad'] <- ifelse(dataset$PoolQC< 3, 1, 0)
  
  dataset['IsExterQualGood'] <- ifelse(dataset$ExterQual >= 3, 1, 0)
  dataset['IsExterCondlGood'] <- ifelse(dataset$ExterCond >= 3, 1, 0)
  dataset['IsBsmtQualGood'] <- ifelse(dataset$BsmtQual >= 3, 1, 0)
  dataset['IsBsmtCondGood'] <- ifelse(dataset$BsmtCond >= 3, 1, 0)
  dataset['IsBsmtExposureGood'] <- ifelse(dataset$BsmtExposure >= 3, 1, 0)
  dataset['IsHeatingQCGood'] <- ifelse(dataset$HeatingQC >= 3, 1, 0)
  dataset['IsKitchenQualGood'] <- ifelse(dataset$KitchenQual >= 3, 1, 0)
  dataset['IsFireplaceQuGood'] <- ifelse(dataset$FireplaceQu >= 3, 1, 0)
  dataset['IsGarageQualGood'] <- ifelse(dataset$GarageQual >= 3, 1, 0)
  dataset['IsGarageCondGood'] <- ifelse(dataset$GarageCond >= 3, 1, 0)
  dataset['IsPoolQCGood'] <- ifelse(dataset$PoolQC >= 3, 1, 0)
  
  #增加一些新的特徵
  # 是否翻修過：如果YearBuilt不等於重新裝修的年份
  dataset['HasBeenRemodeled'] <- ifelse(dataset$YearRemodAdd == dataset$YearBuilt, 0, 1)
  
  # 是否在進行了最近的翻修
  dataset['HasBeenRecentlyRemodeled'] <- ifelse(dataset$YearRemodAdd == dataset$YrSold, 0, 1) 
  
  # 是否在建造年份出售
  dataset['IsNewHouse'] <- ifelse(dataset$YearBuilt == dataset$YrSold, 1, 0) 
  
  # 幾歲
  dataset['Age'] <- as.numeric(2010 - dataset$YearBuilt)
  
  # 距上次出售時間
  dataset['TimeSinceLastSelling'] <- as.numeric(2010 - dataset$YrSold)
  
  # 距翻修並出售的時間
  dataset['TimeSinceRemodeledAndSold'] <- as.numeric(dataset$YrSold - dataset$YearRemodAdd)
  
  areas <- c('LotFrontage', 'LotArea', 'MasVnrArea', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF',
             'TotalBsmtSF', 'X1stFlrSF', 'X2ndFlrSF', 'GrLivArea', 'GarageArea', 'WoodDeckSF', 
             'OpenPorchSF', 'EnclosedPorch', 'X3SsnPorch', 'ScreenPorch', 'LowQualFinSF', 'PoolArea')
  
  # 房屋的總面積，結合與面積相關的特徵
  dataset['TotalSF'] <- as.numeric(rowSums(dataset[,areas]))
  
  # 房屋的總內部面積，結合總一樓和總二樓的面積
  dataset['TotalInsideSF'] <- as.numeric(dataset$X1stFlrSF + dataset$X2ndFlrSF)
  
  # 五月、六月和七月的銷售量較多，可能表示有一定的季節性。我們創建一個新的變數，表示房屋是否在這些月份中售出
  dataset['IsHotMonth'] <- recode(dataset$MoSold,"1"=0,"2"=0,"3"=0,"4"=0,"5"=1, "6"=1, "7"=1, "8"=0, "9"=0, "10"=0, "11"=0, "12"=0)
  
  
  area_features <- c('X2ndFlrSF', 'MasVnrArea', 'WoodDeckSF', 'OpenPorchSF', 'EnclosedPorch', 'X3SsnPorch', 'ScreenPorch', 'WoodDeckSF')
  for (area_feature in area_features){
    dataset[str_c('Has',area_feature)] <- ifelse(dataset[,area_feature] != 0, 1, 0)
  }
  
  expNeighborhood <- c('Crawfor', 'ClearCr', 'Veenker', 'Somerst', 'Timber', 'StoneBr', 'NridgeHt', 'NoRidge')
  dataset['IsNeighborhoodExpensive'] <- (dataset$Neighborhood %in% expNeighborhood) *1
  dataset$NeighborhoodScored <- recode(dataset$Neighborhood, 'MeadowV' = 0, 'IDOTRR' = 0, 'BrDale' = 0, 'OldTown' = 1, 'Edwards' = 1, 'BrkSide' = 1, 'Sawyer' = 1, 'Blueste' = 2, 'SWISU' = 2, 'NAmes' = 2, 'NPkVill' = 2, 'Mitchel' = 2,'SawyerW' = 3, 'Gilbert' = 3, 'NWAmes' = 3, 'Blmngtn' = 3, 'CollgCr' = 3, 'ClearCr' = 3, 'Crawfor' = 3, 'Veenker' = 4, 'Somerst' = 4, 'Timber' = 4, 'StoneBr' = 6, 'NoRidge' = 6, 'NridgHt' = 6)
  
  # 讓評分表達更複雜
  dataset["OverallQual-s2"] <- sapply(dataset$OverallQual, function(x){x**2})
  dataset["OverallQual-s3"] <- sapply(dataset$OverallQual, function(x){x**3})
  dataset["OverallQual-Sq"] <- sqrt(dataset["OverallQual"])
  dataset["TotalSF-2"] <- sapply(dataset$TotalSF, function(x){x**2})
  dataset["TotalSF-3"] = sapply(dataset$TotalSF, function(x){x**3})
  dataset["TotalSF-Sq"] = sqrt(dataset["TotalSF"])
  dataset["GrLivArea-2"] = sapply(dataset$GrLivArea, function(x){x**2})
  dataset["GrLivArea-3"] = sapply(dataset$GrLivArea, function(x){x**3})
  dataset["GrLivArea-Sq"] = sqrt(dataset["GrLivArea"])
  dataset["ExterQual-2"] = sapply(dataset$ExterQual, function(x){x**2})
  dataset["ExterQual-3"] = sapply(dataset$ExterQual, function(x){x**3})
  dataset["ExterQual-Sq"] = sqrt(dataset["ExterQual"])
  dataset["GarageCars-2"] = sapply(dataset$GarageCars, function(x){x**2})
  dataset["GarageCars-3"] = sapply(dataset$GarageCars, function(x){x**3})
  dataset["GarageCars-Sq"] = sqrt(dataset["GarageCars"])
  dataset["KitchenQual-2"] = sapply(dataset$KitchenQual, function(x){x**2})
  dataset["KitchenQual-3"] = sapply(dataset$KitchenQual, function(x){x**3})
  dataset["KitchenQual-Sq"] = sqrt(dataset["KitchenQual"])
  
  summary(dataset)
  if (mode == "prepro_train"){
    write.csv(dataset, file = "./data/prepro_train.csv", row.names=F)
  } else {
    write.csv(dataset, file = "./data/prepro_test.csv", row.names=F)
  }
  
  # write.csv(raw_data, file = "./data/self_prepro_train_freq_encode.csv", row.names=F)
}

train <- read.csv(file = file.path("./data/train_origin.csv"))
test <- read.csv(file = file.path("./data/test_origin.csv"))

data_process(train ,"prepro_train")
data_process(test ,"prepro_test")




