setwd("C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault")

library(randomForest)


# 载入建模数据
data_setmodel <- read.csv("tfidfwords.csv")
data_setmodel$y <- factor(data_setmodel$y)

# 设置训练集与测试集
set.seed(1)
trainSample <- sample(x = c(1:nrow(data_setmodel)), size = trunc(nrow(data_setmodel)*(2/3)), replace = FALSE)
data_train <- data_setmodel[trainSample, ]
data_test <- data_setmodel[-trainSample, ]

# 建立神经网络模型

# 关键参数设置
ntree = 500

# 建模
random <- randomForest(y~., data = data_train, ntree = ntree, importance = TRUE)

# 进行预测
pred <- predict(random, data_test[, -1])
table(data_test$y, pred)
