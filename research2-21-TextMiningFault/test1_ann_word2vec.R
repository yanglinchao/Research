setwd("C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault")

library(nnet)


# 载入建模数据
name <- "ph"
word2vec_vector_size <- 300
word2vec_window <- 10
data_setmodel <- read.csv(paste("cut_word2vec_", name, "_", word2vec_vector_size, "_", word2vec_window, ".csv", sep = ""))
data_table <- read.csv("table_system.csv")
data_setmodel$y <- factor(data_table$sysnum)

# 开始循环建模
index_result <- data.frame(accuracy=NA, precision=NA, recall=NA, f1=NA)
for(cirulation in 1:100){
  
  # 初始时间
  t0 <- Sys.time()
  
  # 设置训练集与测试集
  set.seed(cirulation)
  trainSample <- sample(x = c(1:nrow(data_setmodel)), size = trunc(nrow(data_setmodel)*(2/3)), replace = FALSE)
  data_train <- data_setmodel[trainSample, ]
  data_test <- data_setmodel[-trainSample, ]
  
  # 建立神经网络模型
  
  # 关键参数设置
  size = 20 # 隐神经元数量
  maxit = 100 # 最大迭代次数
  MaxNWts = 1000000 # 允许的最大权值个数
  
  # 建模
  ann <- nnet(y~., data = data_train, size = size, maxit = maxit, MaxNWts = MaxNWts)
  
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
  
  # 进行预测
  pred <- predict(ann, subset(data_test, select = -c(y)), type = "class")
  index <- df_multiIndex(true = data_test$y, pred = pred)
  index_result <- rbind(index_result, index)
  
  # 循环结束时间
  t1 <- Sys.time()
  
  # 输出循环戳
  print(paste(c("第", cirulation, "次循环,", "本次循环用时：", t1-t0), collapse=""))
  print(index)
}
index_result[is.na(index_result)] <- 0
index_result <- index_result[-1, ]
apply(index_result, MARGIN = 2, mean)

# 输出最终结果
outputdata <- data.frame(num = apply(index_result, MARGIN = 2, mean),
                         index = c("Accuracy", "Precision", "Recall", "F1-Score"),
                         cut = rep("word2vec", 4),
                         vectorsize = rep(word2vec_vector_size, 4),
                         window = rep(word2vec_window, 4),
                         algorithm = rep("ANN", 4),
                         size = rep(size, 4))
write.csv(outputdata, paste(c("result_test1_ANN_", size, "_word2vec_", "_", word2vec_vector_size, "_", word2vec_window, ".csv"), collapse = ""), row.names = FALSE)