setwd("C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault")

library(dplyr)
library(ggplot2)
windowsFonts(RMN = windowsFont("Times New Roman"))

data_result_handle_svm <- read.csv("result_test2_handle_svm.csv")

data_result_handle_svm <- as_tibble(data_result_handle_svm)
data_result_handle_svm$index <- factor(data_result_handle_svm$index, levels = c("Accuracy", "Precision", "Recall", "F1-Score"), ordered = TRUE)
data_result_handle_svm$type <- factor(data_result_handle_svm$type,
                                      levels = c("control1", "control2", "goal"),
                                      labels = c("模型1", "模型2", "模型3"), ordered = TRUE)
data_result_handle_svm$vectorsize <- factor(data_result_handle_svm$vectorsize,
                                            levels = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000),
                                            labels = c(paste("文本向量长度：", seq(100, 1000, 100), sep = "")),
                                            ordered = TRUE)
names(data_result_handle_svm)

# 做三个模型的对比图
bar_3modelvs <- ggplot(data = data_result_handle_svm, 
                       aes(x = index, y = result, fill = type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(.~vectorsize, ncol = 2) +
  scale_fill_discrete(name = "模型：") +
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF")) +
  labs(y = "指标值") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(family = "RMN", size = 28),
        axis.title.y = element_text(family = "RMN", size = 28),
        axis.text.y = element_text(family = "RMN", size = 28),
        strip.text = element_text(family = "RMN", size = 28),
        legend.title = element_text(family = "RMN", size = 28),
        legend.text = element_text(family = "RMN", size = 28),
        legend.position = "bottom")
bar_3modelvs
# ggsave(bar_3modelvs, filename = "C:/Users/ylc/Desktop/故障处理方式三个模型对比.tiff", dpi = 600, height = 22, width = 16)



# # 做svm和word2vec的图
# data_goal <- data_result_handle_svm %>% filter(type == "goal")
# data_goal_min <- min(data_goal$result)
# data_goal_max <- max(data_goal$result)
# data_goal_label <- round(seq(data_goal_min, data_goal_max, (data_goal_max-data_goal_min)/5), 3)
# line_goal <- ggplot()+
#   geom_point(data = data_goal, aes(x = vectorsize, y = result, shape = index), size = 2) +
#   geom_line(data = data_goal %>% filter(index == "Accuracy"), aes(x = vectorsize, y = result)) +
#   geom_line(data = data_goal %>% filter(index == "Precision"), aes(x = vectorsize, y = result)) +
#   geom_line(data = data_goal %>% filter(index == "Recall"), aes(x = vectorsize, y = result)) +
#   geom_line(data = data_goal %>% filter(index == "F1-Score"), aes(x = vectorsize, y = result)) +
#   scale_x_continuous(breaks = seq(100, 1000, 100)) +
#   scale_y_continuous(limits = c(data_goal_min, data_goal_max),
#                      breaks = data_goal_label,
#                      labels = data_goal_label) +
#   scale_shape_discrete(name = "模型评价指标：") +
#   labs(x = "文本向量长度", y = "指标值") +
#   theme(axis.title.x = element_text(family = "RMN", size = 25),
#         axis.text.x = element_text(family = "RMN", size = 25),
#         axis.title.y = element_text(family = "RMN", size = 25),
#         axis.text.y = element_text(family = "RMN", size = 25),
#         strip.text = element_text(family = "RMN", size = 25),
#         legend.title = element_text(family = "RMN", size = 25),
#         legend.text = element_text(family = "RMN", size = 25),
#         legend.position = "bottom")
# line_goal
# # ggsave(line_goal, filename = "C:/Users/ylc/Desktop/故障处理方式goal.tiff", dpi = 600, height = 10, width = 16)
# 
# # 做goal组和control组的对比图(向量长度200)
# data_goal_control <- data_result_handle_svm %>% filter(vectorsize == 200)
# bar_goal_control <- ggplot(data = data_goal_control, aes(x = index, y = result, fill = type)) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.4) +
#   scale_fill_discrete(name = "模型:", breaks = c("goal", "control"), labels = c("融合定位信息", "未融合定位信息")) +
#   labs(y = "指标值", x = "模型评价指标") +
#   theme(axis.title.x = element_blank(),
#         axis.text.x = element_text(family = "RMN", size = 28),
#         axis.title.y = element_text(family = "RMN", size = 28),
#         axis.text.y = element_text(family = "RMN", size = 28),
#         strip.text = element_text(family = "RMN", size = 28),
#         legend.title = element_text(family = "RMN", size = 28),
#         legend.text = element_text(family = "RMN", size = 28),
#         legend.position = "bottom")
# bar_goal_control
# # ggsave(bar_goal_control, filename = "C:/Users/ylc/Desktop/故障处理方式对比.tiff", dpi = 600, height = 10, width = 16)
