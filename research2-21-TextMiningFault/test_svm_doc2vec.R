setwd("C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault")

library(e1071)


# 载入建模数据
data_setmodel <- read.csv("doc2vec.csv")
data_setmodel$y <- factor(data_setmodel$y)

# 设置训练集与测试集
set.seed(1)
trainSample <- sample(x = c(1:nrow(data_setmodel)), size = trunc(nrow(data_setmodel)*(2/3)), replace = FALSE)
data_train <- data_setmodel[trainSample, ]
data_test <- data_setmodel[-trainSample, ]

# 建立神经网络模型

# 关键参数设置
gamma = 10
cost = 1
type = "C-classification" # "C-classification"和"nu-classification"适用于y为factor
kernel = "radial" # "linear“; ”polynomial“; ”radial"; "sigmoid"

# 建模
svm <- svm(y~., data = data_train, type = type, kernel = kernel, cost = cost, gamma = gamma, scale = FALSE)

# 进行预测
pred <- predict(svm, data_test[, -1])
table(data_test$y, pred)
