setwd("C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault/paper")

library(e1071)

# 载入建模数据
name <- "ph"
word2vec_vector_size <- 300
word2vec_window <- 10
data_setmodel <- read.csv(paste("cut_word2vec_", name, "_", word2vec_vector_size, "_", word2vec_window, ".csv", sep = ""))
data_table_system <- read.csv("table_system.csv")
data_table_handle <- read.csv("table_handle.csv")
data_setmodel$y1 <- factor(data_table_system$sysnum)
data_setmodel$y2 <- factor(data_table_handle$handle)


# 开始循环建模
index_result <- data.frame(accuracy = NA, precision = NA, recall = NA, f1 = NA)
for(cirulation in 64){
  
  # 初始时间
  t0 <- Sys.time()
  
  # 生成训练集和测试集
  set.seed(cirulation)
  trainSample <- sample(x = c(1:nrow(data_setmodel)), size = trunc(nrow(data_setmodel)*(2/3)), replace = FALSE)
  data_train <- data_setmodel[trainSample, ]
  data_test <- data_setmodel[-trainSample, ]
  
  
  # 关键参数设置
  gamma = 20
  cost = 10
  type = "C-classification" # "C-classification"和"nu-classification"适用于y为factor
  kernel = "radial" # "linear“; ”polynomial“; ”radial"; "sigmoid"
  
  # 建立故障定位模型
  SVMsystem <- svm(y1~., data = subset(data_train, select = -c(y2)), type = type, kernel = kernel, cost = cost, gamma = gamma, scale = FALSE)
  
  # 给训练集打上标签，用真实数据把系统打成标签，把处理方式当做变量，训练模型
  
  # 打标签的function
  def_Tag <- function(data, tag){
    
    tag_unique <- sort(unique(tag))
    tag_vec <- rep(NA, nrow(data))
    for(i in 1:nrow(data)){
      for(j in 1:length(tag_unique)){
        if(tag[i]==tag_unique[j]){
          tag_vec[i] <- j
        }
      }
    }
    tag_data <- data.frame(tag_vec)
    names(tag_data) <- c("T1")
    result <- cbind(tag_data, data)
    return(result)
  }
  # 打标签
  data_train <- def_Tag(data_train, data_train$y1)
  
  # 建立处理方式模型，此时的训练集包含打入的故障定位标签
  SVMhandle <- svm(y2~., data = subset(data_train, select = -c(y1)), type = type, kernel = kernel, cost = cost, gamma = gamma, scale = FALSE)
  
  # 对测试集的故障部位进行预测
  predsite <- predict(SVMsystem, subset(data_test, select = -c(y1, y2)), type = "class")
  
  # 根据预测的故障部位给测试集打上标签
  data_test <- def_Tag(data_test, predsite)
  
  # 对测试集的处理方式进行预测
  pred <- predict(SVMhandle, subset(data_test, select = -c(y1, y2)), type = "class")
  
  # 计算分类效果指标
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
  # 计算
  index <- df_multiIndex(data_test$y2, pred)
  # 保存
  index_result <- rbind(index_result, index)
  
  # 循环结束时间
  t1 <- Sys.time()
  
  # 输出循环戳
  print(paste(c("第", cirulation, "次循环,", "本次循环用时：", t1-t0), collapse=""))
  print(index)
}

index_result[is.na(index_result)] <- 0
index_result <- index_result[-1, ]


# 输出最终结果
# outputdata <- data.frame(num = apply(index_result, MARGIN = 2, mean),
#                          index = c("Accuracy", "Precision", "Recall", "F1-Score"),
#                          cut = rep("word2vec", 4),
#                          vectorsize = rep(word2vec_vector_size, 4),
#                          window = rep(word2vec_window, 4),
#                          algorithm = rep("SVM", 4),
#                          gammacost = rep(paste(c(gamma, cost), collapse = "_"), 4))
outputdata <- index_result
write.csv(outputdata, paste(c("result_test2_single_SVM_", gamma, "_", cost, "_word2vec_", word2vec_vector_size, "_", word2vec_window, ".csv"), collapse = ""), row.names = FALSE)



