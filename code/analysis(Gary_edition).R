library(plyr)
library(Matrix)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(glmnet)
library(caret)
library(stringr)
library(e1071)
library(xgboost)
library(DiagrammeR)
ALLOW_MEMORY_GROWTH=1

minimax_scalar <- function(x){
    if (min(x) == max(x)) {
      return(rep(1, length(x)))
    } else {
      (x - min(x)) / (max(x) - min(x))
    }
}

RMSE <- function(pred, ground){
  diff <- pred - ground
  sqrt(mean(diff^2))
}

# normal
train <- read.csv(file = "./data/prepro_train.csv")
test <- read.csv(file = "./data/prepro_test.csv")

# # freq_encoding
# train <- read.csv(file = "./data/self_prepro_train_freq_encode.csv")
# test <- read.csv(file = "./data/self_prepro_test_freq_encode.csv")

# train data
rownames(train) <- train$Id
train <- train[,-1]
train_X <- train[,-match("SalePrice",colnames(train))]
# MSSubClass、MoSold 和 YrSold 是數值特徵，代表房屋的建築類型、銷售月份和銷售年份。
# 通過將它們轉換為類別特徵，可以更好地將它們用於模型訓練和預測。
train_X$MSSubClass <- as.character(factor(train_X$MSSubClass))
train_y <- log(train$SalePrice)

# # scalar
# cols_to_apply <- sapply(train_X, is.integer) | sapply(train_X, is.numeric)
# result <- apply(train_X[, cols_to_apply], 2, minimax_scalar)
# train_X[, cols_to_apply] <- result
# train_X

# test data
test_X <- test[,-1]
rownames(test_X) <- test$Id
# MSSubClass、MoSold 和 YrSold 是數值特徵，代表房屋的建築類型、銷售月份和銷售年份。
# 通過將它們轉換為類別特徵，可以更好地將它們用於模型訓練和預測。
test_X$MSSubClass <- as.character(factor(test_X$MSSubClass))

# # scalar
# cols_to_apply <- sapply(test_X, is.integer) | sapply(test_X, is.numeric)
# result <- apply(test_X[, cols_to_apply], 2, minimax_scalar)
# test_X[, cols_to_apply] <- result
# test_X

# # Lasso回歸
# set.seed(46)
# lasso <- cv.glmnet(x=data.matrix(train_X), y=train_y, nfolds=10, alpha=1, type.measure = "mse")
# lasso #0.01601
# lasso_pred <- exp(predict(lasso, newx=data.matrix(test_X), s=lasso$lambda.min))
# #RMSE
# 
# # Ridge回歸
# set.seed(46)
# ridge <- cv.glmnet(x=data.matrix(train_X), y=train_y, nfolds=10, alpha=0, type.measure = "mse")
# ridge # 0.01726
# ridge_pred <- exp(predict(ridge, newx=data.matrix(test_X), s=ridge$lambda.min))
# 
# Elastic_net回歸
set.seed(46)
elastic <- cv.glmnet(x=data.matrix(train_X), y=train_y, nfolds=10, alpha=0.7, type.measure = "mse")
elastic
elastic_pred <- exp(predict(elastic, newx=data.matrix(test_X), s=elastic$lambda.min))
# 
# 
# # double variables null model
# null_model_df = data.frame(matrix(nrow = 0, ncol = 4))
# tset_alpha <- seq(0, 1, by = 0.1)
# col_name <- colnames(train_X)
# no_col <- ncol(train_X)
# for (i in 1:(no_col-1)){
#   for (j in (i+1):no_col){
#     # for (a in tset_alpha){
#       print(paste(i,j,a, sep=" "))
#       null_model <- cv.glmnet(x=data.matrix(train_X[, c(i,j)]), y=train_y, nfolds=10, alpha=1, type.measure = "mse")
#       index <- null_model$index[1]
#       print(null_model$cvm[index])
#       null_model_df[nrow(null_model_df)+1,] <- c(i, j, 1, null_model$cvm[index])
#     # }
#   }
# }
#
# train_X[, c(17,125)]
# null_model_df
# colnames(null_model_df) <- c("attribute1","attribute2","test alpha","mse")
# write.csv(null_model_df, file="null_model.csv", row.names = FALSE)
# 
# # # Elastic net alpha testing
# elastic_test_mse <- c()
# tset_alpha <- seq(0.1, 0.9, by = 0.1)
# for (a in tset_alpha) {
#   set.seed(46)
#   elastic_test_a_model <- cv.glmnet(x=data.matrix(train_X), y=train_y, nfolds=10, alpha=a, type.measure = "mse")
#   index <- elastic_test_a_model$index[1]
#   elastic_test_mse <- append(elastic_test_mse, elastic_test_a_model$cvm[index])
# }
# elastic_test_mse
# 
# alpha <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
# # 
# plot(alpha, elastic_test_mse, type = "o", col = "blue")

# # coef
# lasso_coef <- coef(elastic, s="lambda.min")*10
# lasso_coef
# coef <- as.matrix(lasso_coef)
# coef <- data.frame(coef)
# 
# coef_df <- data.frame(coef$s1[-1])
# rownames(coef_df) <- row.names(coef)[-1]
# colnames(coef_df) <- "coef"
# # coef_df
# coef_df_sorted <- coef_df %>% arrange(desc(abs(coef)))
# coef_df_sorted$rownames <- rownames(coef_df_sorted)
# coef_df_sorted$rownames <- factor(coef_df_sorted$rownames,levels=coef_df_sorted$rownames)
# coef_df_sorted <- head(coef_df_sorted, 20)
# coef_df_sorted
# 
# ggplot(coef_df_sorted, aes(coef, rownames)) +
#   geom_bar(stat = "identity") +
#   labs(title = "varible importance (coef*10)", x = "coef", y = "Attibute") +
#   theme(axis.text = element_text(size = 10)) +
#   xlim(-1.5, 1.5)
 
# # lasso pred submit
# lasso_submission <- data.frame(Id = rownames(lasso_pred), SalePrice=(lasso_pred))
# colnames(lasso_submission) <-c("Id", "SalePrice")
# write.csv(lasso_submission, file = "lasso_submission.csv", row.names=FALSE)
# 
# # ridge pred submit
# ridge_submission <- data.frame(Id = rownames(ridge_pred), SalePrice=(ridge_pred))
# colnames(ridge_submission) <-c("Id", "SalePrice")
# write.csv(ridge_submission, file = "ridge_submission.csv", row.names=FALSE)
# 
# elastic pred submit
elastic_submission <- data.frame(Id = rownames(elastic_pred), SalePrice=(elastic_pred))
colnames(elastic_submission) <-c("Id", "SalePrice")
write.csv(elastic_submission, file = "elastic0.7_submission.csv", row.names=FALSE)

##########################################
# # 設定 XGBoost 參數
# params1 <- list(
#   objective = "reg:squarederror",
#   eta = 0.2,
#   max_depth = 20
# )
# 
# 將訓練資料轉換成 DMatrix 格式
dtrain <- xgb.DMatrix(data = data.matrix(train_X), label = train_y)
# 
# xgb_cv <- xgb.cv(params=params1, data=dtrain, nrounds=20, nfold=10, metrics="rmse",
#                  verbose=TRUE, early_stopping_rounds=50, prediction=TRUE, save_models=TRUE)
# 
# print(xgb_cv)
# xgb_cv$
##########################################

# 設定 XGBoost 參數
params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = 0.2,
    max_depth = 20,
    colsample_bytree=0.4,
    min_child_weight=5,
    reg_lambda=0.45
  )

watchlist <- list(train = dtrain)

# Train the XGBoost model
set.seed(46)
xgb <- xgb.train(params = params, data = dtrain,nrounds = 2000,
                   watchlist=watchlist, early_stopping_rounds=50)
tree <- xgb.dump(xgb, with_stats = TRUE)
print(tree[17:24])
colnames(dtrain)[120]
xgb.importance(model=xgb)[1:20,]

# ##########################################
# 
# 將測試資料轉換成 DMatrix 格式
dtest <- xgb.DMatrix(data = data.matrix(test_X))

# 進行預測
xgboost_pred <- exp(predict(xgb, newdata = dtest))

# 將 XGBoost 模型的預測結果生成最終提交的 CSV 文件

xgboost_submission <- data.frame(Id = test$Id, SalePrice = xgboost_pred)
write.csv(xgboost_submission, file = "xgboost_submission.csv", row.names = FALSE)

#Ensamble
predictions <- 0.9 * elastic_pred + 0.1 * xgboost_pred
Ensamble <- data.frame(Id = test$Id, SalePrice = predictions)
colnames(Ensamble) <-c("Id", "SalePrice")
write.csv(Ensamble, file = "Ensamble91.csv", row.names = FALSE)
