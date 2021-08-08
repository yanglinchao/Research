setwd("C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault")

library(nnet)


# 载入建模数据
data_setmodel <- read.csv("tfidfwordm.csv")
data_setmodel$y <- factor(data_setmodel$y)

# 设置训练集与测试集
set.seed(1)
trainSample <- sample(x = c(1:nrow(data_setmodel)), size = trunc(nrow(data_setmodel)*(2/3)), replace = FALSE)
data_train <- data_setmodel[trainSample, ]
data_test <- data_setmodel[-trainSample, ]

# 建立神经网络模型

# 关键参数设置
size = 30 # 隐层神经元数量
maxit = 100 # 最大迭代数
MaxNWts = 1e+06 # 最大可允许的weights个数
# 建模
ann <- nnet(y~., data = data_train, size = size, maxit=maxit, MaxNWts = MaxNWts)

# 进行预测
pred <- predict(ann, data_test[, -1], type = "class")
table(data_test$y, pred)