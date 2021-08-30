setwd("C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault")

library(dplyr)
library(ggplot2)
windowsFonts(RMN = windowsFont("Times New Roman"))

data_result_site <- read.csv("result_test1_site.csv")

data_result_site <- as_tibble(data_result_site)
data_result_site$index <- factor(data_result_site$index, levels = c("Accuracy", "Precision", "Recall", "F1-Score"), ordered = TRUE)
names(data_result_site)

# 提取SVM数据
data_svm <- data_result_site %>% filter(algorithm=="SVM") %>% filter(paramter == "gamma=20, C=10")


# 做svm和word2vec的图
data_svm_word2vec <- data_svm %>% filter(cut_type == "Word2Vec")
data_svm_word2vec_min <- min(data_svm_word2vec$result)
data_svm_word2vec_max <- max(data_svm_word2vec$result)
label = round(seq(data_svm_word2vec_min, data_svm_word2vec_max, (data_svm_word2vec_max-data_svm_word2vec_min)/5), 3)
line_svm_word2vec <- ggplot() +
  geom_point(data = data_svm_word2vec, aes(x = vectorsize, y = result, shape = index), size = 4) +
  geom_line(data = data_svm_word2vec %>% filter(index == "Accuracy"), aes(x = vectorsize, y = result), size = 1.3) +
  geom_line(data = data_svm_word2vec %>% filter(index == "Precision"), aes(x = vectorsize, y = result), size = 1.3) +
  geom_line(data = data_svm_word2vec %>% filter(index == "Recall"), aes(x = vectorsize, y = result), size = 1.3) +
  geom_line(data = data_svm_word2vec %>% filter(index == "F1-Score"), aes(x = vectorsize, y = result), size = 1.3) +
  scale_x_continuous(breaks = seq(100, 1000, 100)) +
  scale_y_continuous(limits = c(data_svm_word2vec_min, data_svm_word2vec_max),
                     breaks = label,
                     labels = label) +
  scale_shape_discrete(name = "模型评价指标：") +
  labs(x = "文本向量长度", y = "指标值") +
  theme(axis.title.x = element_text(family = "RMN", size = 30),
        axis.text.x = element_text(family = "RMN", size = 30),
        axis.title.y = element_text(family = "RMN", size = 30),
        axis.text.y = element_text(family = "RMN", size = 30),
        strip.text = element_text(family = "RMN", size = 30),
        legend.title = element_text(family = "RMN", size = 30),
        legend.text = element_text(family = "RMN", size = 30),
        legend.position = "bottom")
line_svm_word2vec
# ggsave(line_svm_word2vec, filename = "C:/Users/ylc/Desktop/故障定位word2vec.tiff", dpi = 600, height = 10, width = 16)

# 做svm和TFIDF的图
data_svm_tfidf <- data_svm %>% filter(cut_type == "TF-IDF")
data_svm_tfidf_min <- min(data_svm_tfidf$result)
data_svm_tfidf_max <- max(data_svm_tfidf$result)
label = round(seq(data_svm_tfidf_min, data_svm_tfidf_max, (data_svm_tfidf_max-data_svm_tfidf_min)/5), 3)
line_svm_tfidf <- ggplot() +
  geom_point(data = data_svm_tfidf, aes(x = vectorsize, y = result, shape = index), size = 4) +
  geom_line(data = data_svm_tfidf %>% filter(index == "Accuracy"), aes(x = vectorsize, y = result), size = 1.3) +
  geom_line(data = data_svm_tfidf %>% filter(index == "Precision"), aes(x = vectorsize, y = result), size = 1.3) +
  geom_line(data = data_svm_tfidf %>% filter(index == "Recall"), aes(x = vectorsize, y = result), size = 1.3) +
  geom_line(data = data_svm_tfidf %>% filter(index == "F1-Score"), aes(x = vectorsize, y = result), size = 1.3) +
  scale_x_continuous(breaks = c(seq(100, 1000, 100), 1066)) +
  scale_y_continuous(limits = c(data_svm_tfidf_min, data_svm_tfidf_max),
                     breaks = label,
                     labels = label) +
  scale_shape_discrete(name = "模型评价指标：") +
  labs(x = "文本向量长度", y = "指标值") +
  theme(axis.title.x = element_text(family = "RMN", size = 30),
        axis.text.x = element_text(family = "RMN", size = 30),
        axis.title.y = element_text(family = "RMN", size = 30),
        axis.text.y = element_text(family = "RMN", size = 30),
        strip.text = element_text(family = "RMN", size = 30),
        legend.title = element_text(family = "RMN", size = 30),
        legend.text = element_text(family = "RMN", size = 30),
        legend.position = "bottom")
line_svm_tfidf
# ggsave(line_svm_tfidf, filename = "C:/Users/ylc/Desktop/故障定位tfidf.tiff", dpi = 600, height = 10, width = 16)

# 做svm和LDA的图
data_svm_lda <- data_svm %>% filter(cut_type == "LDA")
data_svm_lda_min <- min(data_svm_lda$result)
data_svm_lda_max <- max(data_svm_lda$result)
label = round(seq(data_svm_lda_min, data_svm_lda_max, (data_svm_lda_max-data_svm_lda_min)/5), 3)
line_svm_lda <- ggplot() +
  geom_point(data = data_svm_lda, aes(x = vectorsize, y = result, shape = index), size = 4) +
  geom_line(data = data_svm_lda %>% filter(index == "Accuracy"), aes(x = vectorsize, y = result), size = 1.3) +
  geom_line(data = data_svm_lda %>% filter(index == "Precision"), aes(x = vectorsize, y = result), size = 1.3) +
  geom_line(data = data_svm_lda %>% filter(index == "Recall"), aes(x = vectorsize, y = result), size = 1.3) +
  geom_line(data = data_svm_lda %>% filter(index == "F1-Score"), aes(x = vectorsize, y = result), size = 1.3) +
  scale_x_continuous(breaks = seq(100, 1000, 100)) +
  scale_y_continuous(limits = c(data_svm_lda_min, data_svm_lda_max),
                     breaks = label,
                     labels = label) +
  scale_shape_discrete(name = "模型评价指标：") +
  labs(x = "文本向量长度", y = "指标值") +
  theme(axis.title.x = element_text(family = "RMN", size = 30),
        axis.text.x = element_text(family = "RMN", size = 30),
        axis.title.y = element_text(family = "RMN", size = 30),
        axis.text.y = element_text(family = "RMN", size = 30),
        strip.text = element_text(family = "RMN", size = 30),
        legend.title = element_text(family = "RMN", size = 30),
        legend.text = element_text(family = "RMN", size = 30),
        legend.position = "bottom")
line_svm_lda
# ggsave(line_svm_lda, filename = "C:/Users/ylc/Desktop/故障定位lda.tiff", dpi = 600, height = 10, width = 16)

# 做svm和Doc2Vec的图
data_svm_doc2vec <- data_svm %>% filter(cut_type == "Doc2Vec")
data_svm_doc2vec_min <- min(data_svm_doc2vec$result)
data_svm_doc2vec_max <- max(data_svm_doc2vec$result)
label = round(seq(data_svm_doc2vec_min, data_svm_doc2vec_max, (data_svm_doc2vec_max-data_svm_doc2vec_min)/5), 3)
line_svm_doc2vec <- ggplot() +
  geom_point(data = data_svm_doc2vec, aes(x = vectorsize, y = result, shape = index), size = 4) +
  geom_line(data = data_svm_doc2vec %>% filter(index == "Accuracy"), aes(x = vectorsize, y = result), size = 1.3) +
  geom_line(data = data_svm_doc2vec %>% filter(index == "Precision"), aes(x = vectorsize, y = result), size = 1.3) +
  geom_line(data = data_svm_doc2vec %>% filter(index == "Recall"), aes(x = vectorsize, y = result), size = 1.3) +
  geom_line(data = data_svm_doc2vec %>% filter(index == "F1-Score"), aes(x = vectorsize, y = result), size = 1.3) +
  scale_x_continuous(breaks = seq(100, 1000, 100)) +
  scale_y_continuous(limits = c(data_svm_doc2vec_min, data_svm_doc2vec_max),
                     breaks = label,
                     labels = label) +
  scale_shape_discrete(name = "模型评价指标：") +
  labs(x = "文本向量长度", y = "指标值") +
  theme(axis.title.x = element_text(family = "RMN", size = 30),
        axis.text.x = element_text(family = "RMN", size = 30),
        axis.title.y = element_text(family = "RMN", size = 30),
        axis.text.y = element_text(family = "RMN", size = 30),
        strip.text = element_text(family = "RMN", size = 30),
        legend.title = element_text(family = "RMN", size = 30),
        legend.text = element_text(family = "RMN", size = 30),
        legend.position = "bottom")
line_svm_doc2vec
# ggsave(line_svm_doc2vec, filename = "C:/Users/ylc/Desktop/故障定位doc2vec.tiff", dpi = 600, height = 10, width = 16)








# # 提取SVM数据
# data_svm <- data_result_site %>% filter(algorithm=="SVM")
# data_svm$paramter <- factor(data_svm$paramter, levels = c("gamma=50, C=10", "gamma=100, C=20", "gamma=150, C=30"), ordered = TRUE)
# 
# 
# 
# 
# # 做svm和word2vec的图
# data_svm_word2vec <- data_svm %>% filter(cut_type == "Word2Vec")
# data_svm_word2vec_min <- min(data_svm_word2vec$result)
# data_svm_word2vec_max <- max(data_svm_word2vec$result)
# label = round(seq(data_svm_word2vec_min, data_svm_word2vec_max, (data_svm_word2vec_max-data_svm_word2vec_min)/5), 3)
# line_svm_word2vec <- ggplot() +
#   geom_point(data = data_svm_word2vec, aes(x = vectorsize, y = result, shape = index), size = 2) +
#   geom_line(data = data_svm_word2vec %>% filter(index == "Accuracy"), aes(x = vectorsize, y = result)) +
#   geom_line(data = data_svm_word2vec %>% filter(index == "Precision"), aes(x = vectorsize, y = result)) +
#   geom_line(data = data_svm_word2vec %>% filter(index == "Recall"), aes(x = vectorsize, y = result)) +
#   geom_line(data = data_svm_word2vec %>% filter(index == "F1-Score"), aes(x = vectorsize, y = result)) +
#   scale_x_continuous(breaks = seq(100, 1000, 100)) +
#   scale_y_continuous(limits = c(data_svm_word2vec_min, data_svm_word2vec_max),
#                      breaks = label,
#                      labels = label) +
#   scale_shape_discrete(name = "模型评价指标：") +
#   labs(x = "文本向量长度") +
#   facet_grid(paramter~.) +
#   theme(axis.title.x = element_text(family = "RMN", size = 25),
#         axis.text.x = element_text(family = "RMN", size = 25),
#         axis.title.y = element_blank(),
#         axis.text.y = element_text(family = "RMN", size = 25),
#         strip.text = element_text(family = "RMN", size = 25),
#         legend.title = element_text(family = "RMN", size = 25),
#         legend.text = element_text(family = "RMN", size = 25),
#         legend.position = "bottom")
# line_svm_word2vec
# # ggsave(line_svm_word2vec, filename = "C:/Users/ylc/Desktop/word2vec故障定位.tiff", dpi = 600, height = 13, width = 13)
# 
# 
# # 做svm和TFIDF的图
# data_svm_tfidf <- data_svm %>% filter(cut_type == "TF-IDF")
# data_svm_tfidf_min <- min(data_svm_tfidf$result)
# data_svm_tfidf_max <- max(data_svm_tfidf$result)
# label = round(seq(data_svm_tfidf_min, data_svm_tfidf_max, (data_svm_tfidf_max-data_svm_tfidf_min)/5), 3)
# line_svm_tfidf <- ggplot() +
#   geom_point(data = data_svm_tfidf, aes(x = vectorsize, y = result, shape = index), size = 2) +
#   geom_line(data = data_svm_tfidf %>% filter(index == "Accuracy"), aes(x = vectorsize, y = result)) +
#   geom_line(data = data_svm_tfidf %>% filter(index == "Precision"), aes(x = vectorsize, y = result)) +
#   geom_line(data = data_svm_tfidf %>% filter(index == "Recall"), aes(x = vectorsize, y = result)) +
#   geom_line(data = data_svm_tfidf %>% filter(index == "F1-Score"), aes(x = vectorsize, y = result)) +
#   scale_x_continuous(breaks = c(seq(100, 1000, 100), 1066)) +
#   scale_y_continuous(limits = c(data_svm_tfidf_min, data_svm_tfidf_max),
#                      breaks = label,
#                      labels = label) +
#   scale_shape_discrete(name = "模型评价指标：") +
#   labs(x = "文本向量长度") +
#   facet_grid(paramter~.) +
#   theme(axis.title.x = element_text(family = "RMN", size = 25),
#         axis.text.x = element_text(family = "RMN", size = 25),
#         axis.title.y = element_blank(),
#         axis.text.y = element_text(family = "RMN", size = 25),
#         strip.text = element_text(family = "RMN", size = 25),
#         legend.title = element_text(family = "RMN", size = 25),
#         legend.text = element_text(family = "RMN", size = 25),
#         legend.position = "bottom")
# line_svm_tfidf
# # ggsave(line_svm_tfidf, filename = "C:/Users/ylc/Desktop/tfidf故障定位.tiff", dpi = 600, height = 13, width = 13)
# 
# # 做svm和LDA的图
# data_svm_lda <- data_svm %>% filter(cut_type == "LDA")
# data_svm_lda_min <- min(data_svm_lda$result)
# data_svm_lda_max <- max(data_svm_lda$result)
# label = round(seq(data_svm_lda_min, data_svm_lda_max, (data_svm_lda_max-data_svm_lda_min)/5), 3)
# line_svm_lda <- ggplot() +
#   geom_point(data = data_svm_lda, aes(x = vectorsize, y = result, shape = index), size = 2) +
#   geom_line(data = data_svm_lda %>% filter(index == "Accuracy"), aes(x = vectorsize, y = result)) +
#   geom_line(data = data_svm_lda %>% filter(index == "Precision"), aes(x = vectorsize, y = result)) +
#   geom_line(data = data_svm_lda %>% filter(index == "Recall"), aes(x = vectorsize, y = result)) +
#   geom_line(data = data_svm_lda %>% filter(index == "F1-Score"), aes(x = vectorsize, y = result)) +
#   scale_x_continuous(breaks = seq(100, 1000, 100)) +
#   scale_y_continuous(limits = c(data_svm_lda_min, data_svm_lda_max),
#                      breaks = label,
#                      labels = label) +
#   scale_shape_discrete(name = "模型评价指标：") +
#   labs(x = "文本向量长度") +
#   facet_grid(paramter~.) +
#   theme(axis.title.x = element_text(family = "RMN", size = 25),
#         axis.text.x = element_text(family = "RMN", size = 25),
#         axis.title.y = element_blank(),
#         axis.text.y = element_text(family = "RMN", size = 25),
#         strip.text = element_text(family = "RMN", size = 25),
#         legend.title = element_text(family = "RMN", size = 25),
#         legend.text = element_text(family = "RMN", size = 25),
#         legend.position = "bottom")
# line_svm_lda
# # ggsave(line_svm_lda, filename = "C:/Users/ylc/Desktop/lda故障定位.tiff", dpi = 600, height = 13, width = 13)
# 
# # 做svm和Doc2Vec的图
# data_svm_doc2vec <- data_svm %>% filter(cut_type == "Doc2Vec")
# data_svm_doc2vec_min <- min(data_svm_doc2vec$result)
# data_svm_doc2vec_max <- max(data_svm_doc2vec$result)
# label = round(seq(data_svm_doc2vec_min, data_svm_doc2vec_max, (data_svm_doc2vec_max-data_svm_doc2vec_min)/5), 3)
# line_svm_doc2vec <- ggplot() +
#   geom_point(data = data_svm_doc2vec, aes(x = vectorsize, y = result, shape = index), size = 2) +
#   geom_line(data = data_svm_doc2vec %>% filter(index == "Accuracy"), aes(x = vectorsize, y = result)) +
#   geom_line(data = data_svm_doc2vec %>% filter(index == "Precision"), aes(x = vectorsize, y = result)) +
#   geom_line(data = data_svm_doc2vec %>% filter(index == "Recall"), aes(x = vectorsize, y = result)) +
#   geom_line(data = data_svm_doc2vec %>% filter(index == "F1-Score"), aes(x = vectorsize, y = result)) +
#   scale_x_continuous(breaks = seq(100, 1000, 100)) +
#   scale_y_continuous(limits = c(data_svm_doc2vec_min, data_svm_doc2vec_max),
#                      breaks = label,
#                      labels = label) +
#   scale_shape_discrete(name = "模型评价指标：") +
#   labs(x = "文本向量长度") +
#   facet_grid(paramter~.) +
#   theme(axis.title.x = element_text(family = "RMN", size = 25),
#         axis.text.x = element_text(family = "RMN", size = 25),
#         axis.title.y = element_blank(),
#         axis.text.y = element_text(family = "RMN", size = 25),
#         strip.text = element_text(family = "RMN", size = 25),
#         legend.title = element_text(family = "RMN", size = 25),
#         legend.text = element_text(family = "RMN", size = 25),
#         legend.position = "bottom")
# line_svm_doc2vec
# # ggsave(line_svm_doc2vec, filename = "C:/Users/ylc/Desktop/doc2vec故障定位.tiff", dpi = 600, height = 13, width = 13)
# 
# 
# 
# # 进行tfidf和Word2Vec的对比
# data_svm_tfidf_word2vec <- data_svm %>% filter(cut_type == "Word2Vec" | cut_type == "TF-IDF") 
# data_svm_tfidf_word2vec_min <- min(data_svm_tfidf_word2vec$result)
# data_svm_tfidf_word2vec_max <- max(data_svm_tfidf_word2vec$result)
# label = round(seq(data_svm_tfidf_word2vec_min, data_svm_tfidf_word2vec_max, (data_svm_tfidf_word2vec_max-data_svm_tfidf_word2vec_min)/5), 3)
# line_svm_tfidf_word2vec <- ggplot() +
#   geom_point(data = data_svm_tfidf_word2vec, aes(x = vectorsize, y = result, shape = index), size = 2) +
#   geom_line(data = data_svm_tfidf_word2vec %>% filter(index == "Accuracy"), aes(x = vectorsize, y = result)) +
#   geom_line(data = data_svm_tfidf_word2vec %>% filter(index == "Precision"), aes(x = vectorsize, y = result)) +
#   geom_line(data = data_svm_tfidf_word2vec %>% filter(index == "Recall"), aes(x = vectorsize, y = result)) +
#   geom_line(data = data_svm_tfidf_word2vec %>% filter(index == "F1-Score"), aes(x = vectorsize, y = result)) +
#   scale_x_continuous(breaks = seq(100, 1000, 100)) +
#   scale_y_continuous(limits = c(data_svm_tfidf_word2vec_min, data_svm_tfidf_word2vec_max),
#                      breaks = label,
#                      labels = label) +
#   scale_shape_discrete(name = "模型评价指标：") +
#   labs(x = "文本向量长度") +
#   facet_grid(paramter~cut_type) +
#   theme(axis.title.x = element_text(family = "RMN", size = 25),
#         axis.text.x = element_text(family = "RMN", size = 25),
#         axis.title.y = element_blank(),
#         axis.text.y = element_text(family = "RMN", size = 25),
#         strip.text = element_text(family = "RMN", size = 25),
#         legend.title = element_text(family = "RMN", size = 25),
#         legend.text = element_text(family = "RMN", size = 25),
#         legend.position = "bottom")
# line_svm_tfidf_word2vec
