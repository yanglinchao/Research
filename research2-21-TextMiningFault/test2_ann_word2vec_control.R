setwd("C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault")

library(e1071)


# 载入建模数据
name <- "ph"
word2vec_vector_size <- 300
word2vec_window <- 10
data_setmodel <- read.csv(paste("cut_word2vec_", name, "_", word2vec_vector_size, "_", word2vec_window, ".csv", sep = ""))
data_table <- read.csv("table_system.csv")
data_setmodel$y <- factor(data_table$sysnum)

# 开始循环建模
index_result <- data.frame(accuracy=NA, precision=NA, recall=NA, f1=NA)
for(cirulation in 1:10){
  
  # 初始时间
  t0 <- Sys.time()
  
  # 生成训练集和测试集
  set.seed(cirulation)
  trainSample <- sample(x = c(1:nrow(data_setmodel)), size = trunc(nrow(data_setmodel)*(2/3)), replace = FALSE)
  data_train <- data_setmodel[trainSample, ]
  data_test <- data_setmodel[-trainSample, ]
  
  # 关键参数设置
  gamma = 150
  cost = 30
  type = "C-classification" # "C-classification"和"nu-classification"适用于y为factor
  kernel = "radial" # "linear“; ”polynomial“; ”radial"; "sigmoid"
  
  # 建立处理方式模型
  ANNhandle <- nnet(y2~., data = subset(data_train, select = -c(y1)), size = size, maxit = maxit, MaxNWts = MaxNWts)
  
  # 使用处理方式模型对测试集的处理方式进行预测
  pred <- predict(ANNhandle, subset(data_test, select = -c(y1, y2)), type = "class")
  
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
  
  # 预测指标计算
  index <- df_multiIndex(data_test$y2, pred)
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
