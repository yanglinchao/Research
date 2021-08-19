setwd("C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault")

library(randomForest)

# 载入建模数据
# 载入建模数据
cutmethod <- "word2vec"
data_setmodel <- read.csv(paste(c("cut_", cutmethod, "_ph.csv"), collapse = ""))
data_table_system <- read.csv("table_system.csv")
data_table_handle <- read.csv("table_handle.csv")
data_setmodel$y1 <- factor(data_table_system$sysnum)
data_setmodel$y2 <- factor(data_table_handle$handle)

# 生成训练集和测试集
set.seed(123)
trainSample <- sample(x = c(1:nrow(data_setmodel)), size = trunc(nrow(data_setmodel)*2/3), replace = FALSE)
data_train <- data_setmodel[trainSample, ]
data_test <- data_setmodel[-trainSample, ]

# 建立故障定位模型
RFsystem <- randomForest(y1~., data = subset(data_train, select = -c(y2)), ntree = 400, importance = TRUE)

# 对测试集的故障进行定位
predsite <- predict(RFsystem, subset(data_test, select = -c(y1, y2)))

# 建立处理方式模型
# 生成WoE数据的函数
def_WoE <- function(x){
  tag_unique <- sort(unique(x))
  woe <- matrix(data = c(-0.3357, 0.7367, -0.6576,
                         0.1703, -0.0775, -0.0752,
                         0.4510, 0.3036, -1.0284,
                         -0.2844, -0.5266, 0.7296),
                nrow = 4, ncol = 3, byrow = TRUE)
  tag_matrix <- matrix(NA, nrow = length(x), ncol = ncol(woe))
  for(i in 1:length(x)){
    for(j in 1:length(tag_unique)){
      if(x[i]==tag_unique[j]){
        tag_matrix[i, ] <- woe[j, ]
      }
    }
  }
  tag_data <- data.frame(tag_matrix)
  names(tag_data) <- c("T1", "T2", "T3")
  return(tag_data)
}
# 先对训练集的真实故障系统打成WoE数据
data_train_tag <- def_WoE(data_train$y1)
# 构建建模数据
data_train_tag$y <- data_train$y2
# 建立模型
RFhandle <- randomForest(y~., data = data_train_tag, ntree = 100, importance = TRUE)

# 根据上面的定位结果生成WoE测试数据
data_test_tag <- def_WoE(predsite)
# 对测试集的处理方式进行预测
pred <- predict(RFhandle, data_test_tag)

# 多分类指标计算
df_multiIndex <- function(true, pred){
  
  label <- sort(unique(true))
  data <- data.frame(true = true, pred = pred)
  
  precision <- NA
  recall <- NA
  f1 <- NA
  #  tp: True Positive,把正样本预测为正
  #  fp: False Positive,把负样本预测为正
  #  tn: True Negative,把负样本预测为负
  #  fn: False Negative,把正样本预测为负
  # accuracy = (tp+tn)/(tp+fp+tn+fn)
  # precision = tp/(tp+fp) 在预测为正的所有数据中，真实的占多少
  # recall = tp/(tp+fn) 在所有的正样本中，成功预测的有多少
  # f1 = (2*precision*recall)/(precision+recall)
  for(i in 1:length(label)){
    tp <- nrow(data[which(data$true==label[i] & data$pred==label[i]), ])
    fp <- nrow(data[which(data$true!=label[i] & data$pred==label[i]), ])
    tn <- nrow(data[which(data$true!=label[i] & data$pred!=label[i]), ])
    fn <- nrow(data[which(data$true==label[i] & data$pred!=label[i]), ])
    precision[i] <- tp/(tp+fp)
    recall[i] <- tp/(tp+fn)
    f1[i] <- (2*(tp/(tp+fp))*(tp/(tp+fn)))/((tp/(tp+fp))+(tp/(tp+fn)))
  }
  accuracy <- nrow(data[which(data$true==data$pred), ])/nrow(data)
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  f1[is.na(f1)] <- 0
  
  result <- data.frame(accuracy = accuracy, precision = mean(precision), recall = mean(recall), f1 = mean(f1))
  return(result)
}
# 计算评价指标
index <- df_multiIndex(data_test$y2, pred)
index
