setwd("C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault")

library(dplyr)
library(ggplot2)
windowsFonts(RMN = windowsFont("Times New Roman"))


data_result_site <- read.csv("result_test1_site.csv")
data_result_site <- data_result_site[-which(data_result_site$vectorsize>1000), ]
data_result_site <- as_tibble(data_result_site)
data_result_site$index <- factor(data_result_site$index, levels = c("Accuracy", "Precision", "Recall", "F1-Score"), ordered = TRUE)
names(data_result_site)

# 提取ANN数据
data_ann <- data_result_site %>% filter(algorithm=="ANN")
data_ann$paramter[which(data_ann$paramter==10)] <- "隐神经元数量：10"
data_ann$paramter[which(data_ann$paramter==20)] <- "隐神经元数量：20"
data_ann$paramter[which(data_ann$paramter==30)] <- "隐神经元数量：30"
data_ann$paramter <- factor(data_ann$paramter, levels = c("隐神经元数量：10", "隐神经元数量：20", "隐神经元数量：30"), ordered = TRUE)

# 做ANN和word2vec的图
data_ann_word2vec <- data_ann %>% filter(cut_type == "Word2Vec")
data_ann_word2vec_min <- min(data_ann_word2vec$result)
data_ann_word2vec_max <- max(data_ann_word2vec$result)
label = round(seq(data_ann_word2vec_min, data_ann_word2vec_max, (data_ann_word2vec_max-data_ann_word2vec_min)/5), 3)
line_ann_word2vec <- ggplot() +
  geom_point(data = data_ann_word2vec, aes(x = vectorsize, y = result, shape = index), size = 2) +
  geom_line(data = data_ann_word2vec %>% filter(index == "Accuracy"), aes(x = vectorsize, y = result)) +
  geom_line(data = data_ann_word2vec %>% filter(index == "Precision"), aes(x = vectorsize, y = result)) +
  geom_line(data = data_ann_word2vec %>% filter(index == "Recall"), aes(x = vectorsize, y = result)) +
  geom_line(data = data_ann_word2vec %>% filter(index == "F1-Score"), aes(x = vectorsize, y = result)) +
  scale_x_continuous(breaks = seq(100, 1000, 100)) +
  scale_y_continuous(limits = c(data_ann_word2vec_min, data_ann_word2vec_max),
                     breaks = label,
                     labels = label) +
  scale_shape_discrete(name = "模型评价指标：") +
  labs(x = "文本向量长度") +
  facet_grid(paramter~.) +
  theme(axis.title.x = element_text(family = "RMN", size = 25),
        axis.text.x = element_text(family = "RMN", size = 25),
        axis.title.y = element_blank(),
        axis.text.y = element_text(family = "RMN", size = 25),
        strip.text = element_text(family = "RMN", size = 25),
        legend.title = element_text(family = "RMN", size = 25),
        legend.text = element_text(family = "RMN", size = 25),
        legend.position = "bottom")
line_ann_word2vec
# ggsave(line_ann_word2vec, filename = "C:/Users/ylc/Desktop/word2vec故障定位.tiff", dpi = 600, height = 13, width = 13)

# 做ANN和TFIDF的图
data_ann_tfidf <- data_ann %>% filter(cut_type == "TF-IDF")
data_ann_tfidf_min <- min(data_ann_tfidf$result)
data_ann_tfidf_max <- max(data_ann_tfidf$result)
label = round(seq(data_ann_tfidf_min, data_ann_tfidf_max, (data_ann_tfidf_max-data_ann_tfidf_min)/5), 3)
line_ann_tfidf <- ggplot() +
  geom_point(data = data_ann_tfidf, aes(x = vectorsize, y = result, shape = index), size = 2) +
  geom_line(data = data_ann_tfidf %>% filter(index == "Accuracy"), aes(x = vectorsize, y = result)) +
  geom_line(data = data_ann_tfidf %>% filter(index == "Precision"), aes(x = vectorsize, y = result)) +
  geom_line(data = data_ann_tfidf %>% filter(index == "Recall"), aes(x = vectorsize, y = result)) +
  geom_line(data = data_ann_tfidf %>% filter(index == "F1-Score"), aes(x = vectorsize, y = result)) +
  scale_x_continuous(breaks = seq(100, 1000, 100)) +
  scale_y_continuous(limits = c(data_ann_tfidf_min, data_ann_tfidf_max),
                     breaks = label,
                     labels = label) +
  scale_shape_discrete(name = "模型评价指标：") +
  labs(x = "文本向量长度") +
  facet_grid(paramter~.) +
  theme(axis.title.x = element_text(family = "RMN", size = 25),
        axis.text.x = element_text(family = "RMN", size = 25),
        axis.title.y = element_blank(),
        axis.text.y = element_text(family = "RMN", size = 25),
        strip.text = element_text(family = "RMN", size = 25),
        legend.title = element_text(family = "RMN", size = 25),
        legend.text = element_text(family = "RMN", size = 25),
        legend.position = "bottom")
line_ann_tfidf
# ggsave(line_ann_tfidf, filename = "C:/Users/ylc/Desktop/tfidf故障定位.tiff", dpi = 600, height = 13, width = 13)

# 做ANN和TFIDF的图
data_ann_lda <- data_ann %>% filter(cut_type == "LDA")
data_ann_lda_min <- min(data_ann_lda$result)
data_ann_lda_max <- max(data_ann_lda$result)
label = round(seq(data_ann_lda_min, data_ann_lda_max, (data_ann_lda_max-data_ann_lda_min)/5), 3)
line_ann_lda <- ggplot() +
  geom_point(data = data_ann_lda, aes(x = vectorsize, y = result, shape = index), size = 2) +
  geom_line(data = data_ann_lda %>% filter(index == "Accuracy"), aes(x = vectorsize, y = result)) +
  geom_line(data = data_ann_lda %>% filter(index == "Precision"), aes(x = vectorsize, y = result)) +
  geom_line(data = data_ann_lda %>% filter(index == "Recall"), aes(x = vectorsize, y = result)) +
  geom_line(data = data_ann_lda %>% filter(index == "F1-Score"), aes(x = vectorsize, y = result)) +
  scale_x_continuous(breaks = seq(100, 1000, 100)) +
  scale_y_continuous(limits = c(data_ann_lda_min, data_ann_lda_max),
                     breaks = label,
                     labels = label) +
  scale_shape_discrete(name = "模型评价指标：") +
  labs(x = "文本向量长度") +
  facet_grid(paramter~.) +
  theme(axis.title.x = element_text(family = "RMN", size = 25),
        axis.text.x = element_text(family = "RMN", size = 25),
        axis.title.y = element_blank(),
        axis.text.y = element_text(family = "RMN", size = 25),
        strip.text = element_text(family = "RMN", size = 25),
        legend.title = element_text(family = "RMN", size = 25),
        legend.text = element_text(family = "RMN", size = 25),
        legend.position = "bottom")
line_ann_lda
# ggsave(line_ann_lda, filename = "C:/Users/ylc/Desktop/lda故障定位.tiff", dpi = 600, height = 13, width = 13)

# 做ANN和Doc2Vec的图
data_ann_doc2vec <- data_ann %>% filter(cut_type == "Doc2Vec")
data_ann_doc2vec_min <- min(data_ann_doc2vec$result)
data_ann_doc2vec_max <- max(data_ann_doc2vec$result)
label = round(seq(data_ann_doc2vec_min, data_ann_doc2vec_max, (data_ann_doc2vec_max-data_ann_doc2vec_min)/5), 3)
line_ann_doc2vec <- ggplot() +
  geom_point(data = data_ann_doc2vec, aes(x = vectorsize, y = result, shape = index), size = 2) +
  geom_line(data = data_ann_doc2vec %>% filter(index == "Accuracy"), aes(x = vectorsize, y = result)) +
  geom_line(data = data_ann_doc2vec %>% filter(index == "Precision"), aes(x = vectorsize, y = result)) +
  geom_line(data = data_ann_doc2vec %>% filter(index == "Recall"), aes(x = vectorsize, y = result)) +
  geom_line(data = data_ann_doc2vec %>% filter(index == "F1-Score"), aes(x = vectorsize, y = result)) +
  scale_x_continuous(breaks = seq(100, 1000, 100)) +
  scale_y_continuous(limits = c(data_ann_doc2vec_min, data_ann_doc2vec_max),
                     breaks = label,
                     labels = label) +
  scale_shape_discrete(name = "模型评价指标：") +
  labs(x = "文本向量长度") +
  facet_grid(paramter~.) +
  theme(axis.title.x = element_text(family = "RMN", size = 25),
        axis.text.x = element_text(family = "RMN", size = 25),
        axis.title.y = element_blank(),
        axis.text.y = element_text(family = "RMN", size = 25),
        strip.text = element_text(family = "RMN", size = 25),
        legend.title = element_text(family = "RMN", size = 25),
        legend.text = element_text(family = "RMN", size = 25),
        legend.position = "bottom")
line_ann_doc2vec
# ggsave(line_ann_doc2vec, filename = "C:/Users/ylc/Desktop/doc2vec故障定位.tiff", dpi = 600, height = 13, width = 13)


# 进行tfidf和Word2Vec的对比
data_ann_tfidf_word2vec <- data_ann %>% filter(cut_type == c("TF-IDF", "Word2Vec")) %>% filter(vectorsize > 600)
data_ann_tfidf_word2vec_min <- min(data_ann_tfidf_word2vec$result)
data_ann_tfidf_word2vec_max <- max(data_ann_tfidf_word2vec$result)
label = round(seq(data_ann_tfidf_word2vec_min, data_ann_tfidf_word2vec_max, (data_ann_tfidf_word2vec_max-data_ann_tfidf_word2vec_min)/5), 3)
line_ann_tfidf_word2vec <- ggplot() +
  geom_point(data = data_ann_tfidf_word2vec, aes(x = vectorsize, y = result, shape = index, color = cut_type), size = 2) +
  geom_line(data = data_ann_tfidf_word2vec %>% filter(index == "Accuracy"), aes(x = vectorsize, y = result, color = cut_type)) +
  geom_line(data = data_ann_tfidf_word2vec %>% filter(index == "Precision"), aes(x = vectorsize, y = result, color = cut_type)) +
  geom_line(data = data_ann_tfidf_word2vec %>% filter(index == "Recall"), aes(x = vectorsize, y = result, color = cut_type)) +
  geom_line(data = data_ann_tfidf_word2vec %>% filter(index == "F1-Score"), aes(x = vectorsize, y = result, color = cut_type)) +
  scale_x_continuous(breaks = seq(100, 1000, 100)) +
  scale_y_continuous(limits = c(data_ann_tfidf_word2vec_min, data_ann_tfidf_word2vec_max),
                     breaks = label,
                     labels = label) +
  scale_shape_discrete(name = "模型评价指标：") +
  labs(x = "文本向量长度") +
  facet_grid(paramter~.) +
  theme(axis.title.x = element_text(family = "RMN", size = 25),
        axis.text.x = element_text(family = "RMN", size = 25),
        axis.title.y = element_blank(),
        axis.text.y = element_text(family = "RMN", size = 25),
        strip.text = element_text(family = "RMN", size = 25),
        legend.title = element_text(family = "RMN", size = 25),
        legend.text = element_text(family = "RMN", size = 25),
        legend.position = "bottom")
line_ann_tfidf_word2vec