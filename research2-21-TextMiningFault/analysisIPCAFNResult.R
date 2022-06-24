# The classification results of fault state and normal state are analyzed

load("C:/Users/ylc/jgy/research/16-CIPCABPNN/test/dataResultPC5.RData")
library(ggplot2)
library(dplyr)
windowsFonts(RMN = windowsFont("Times New Roman"))

################################
# Step1
# IPCA
# It shows that the effect of the integrated learner is better than that of the base learner
# it's better as the number of neurons increases
dataIPCAINdexFN <- tbl_df(dataIPCA5IndexFN)
dataIPCAIndexbyGroupFN <- dataIPCAINdexFN %>%
  group_by(Neurons, Type, Index) %>%
  summarise(mean=mean(Value))
dataIPCAIndexbyGroupFN$Type <- factor(dataIPCAIndexbyGroupFN$Type, levels = c("Lower", "Upper", "Ensemble"), ordered = TRUE)
# write.csv(dataIPCAIndexbyGroupFN, "CIPCA分组指标统计.csv", row.names = FALSE)

# gpIPCAIndexbyGroupFN <- ggplot(data = dataIPCAIndexbyGroupFN, aes(x = Neurons, y = mean, fill = Type)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_fill_discrete(name = "Classifier type:", breaks = c("Lower", "Upper", "Ensemble"), labels = c("Lower bond", "Upper bond", "Ensemble")) +
#   labs(x = "Number of hidden neurons") +
#   scale_x_continuous(breaks = c(10, 15, 20, 25, 30, 35, 40)) +
#   facet_grid(.~Index) +
#   theme(axis.title.x = element_text(family = "RMN", size = 15),
#         axis.text.x = element_text(family = "RMN", size = 15),
#         axis.title.y = element_blank(),
#         axis.text.y = element_text(family = "RMN", size = 15),
#         strip.text = element_text(family = "RMN", size = 15),
#         legend.title = element_text(family = "RMN", size = 15),
#         legend.text = element_text(family = "RMN", size = 15),
#         legend.position = "bottom")
# gpIPCAIndexbyGroupFN
# # ggsave(gpIPCAIndexbyGroupFN, filename = "plotIPCA5IndexbyGroupFN.tiff", dpi = 200)


gpIPCAaccbyGroupFN <- ggplot(data = dataIPCAIndexbyGroupFN %>% filter(Index=="Accuracy"), aes(x = Neurons, y = mean, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 4) +
  scale_fill_discrete(name = "模型:", breaks = c("Lower", "Upper", "Ensemble"), labels = c("下界基学习器", "上界基学习器", "集成学习模型")) +
  labs(x = "隐神经元数量", y = "Accuracy") +
  scale_x_continuous(breaks = c(10, 15, 20, 25, 30, 35, 40, 45)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1),
                     labels = c(0, "25%", "50%", "75%", "100%"),
                     limits = c(0, 1)) +
  labs(y = "准确率") +
  theme(axis.title.x = element_text(family = "RMN", size = 28),
        axis.text.x = element_text(family = "RMN", size = 28),
        axis.title.y = element_text(family = "RMN", size = 28),
        axis.text.y = element_text(family = "RMN", size = 28),
        strip.text = element_text(family = "RMN", size = 28),
        legend.title = element_text(family = "RMN", size = 28),
        legend.text = element_text(family = "RMN", size = 28),
        legend.position = "bottom")
gpIPCAaccbyGroupFN
# ggsave(gpIPCAaccbyGroupFN, filename = "C:/Users/ylc/Desktop/CIPCA故障预测ACC.tiff", dpi = 600, height = 8, width = 13)

gpIPCAprebyGroupFN <- ggplot(data = dataIPCAIndexbyGroupFN %>% filter(Index=="Precision"), aes(x = Neurons, y = mean, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 4) +
  scale_fill_discrete(name = "模型:", breaks = c("Lower", "Upper", "Ensemble"), labels = c("下界基学习器", "上界基学习器", "集成学习模型")) +
  labs(x = "隐神经元数量", y = "Precision") +
  scale_x_continuous(breaks = c(10, 15, 20, 25, 30, 35, 40, 45)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1),
                     labels = c(0, "25%", "50%", "75%", "100%"),
                     limits = c(0, 1)) +
  labs(y = "精确率") + 
  theme(axis.title.x = element_text(family = "RMN", size = 28),
        axis.text.x = element_text(family = "RMN", size = 28),
        axis.title.y = element_text(family = "RMN", size = 28),
        axis.text.y = element_text(family = "RMN", size = 28),
        strip.text = element_text(family = "RMN", size = 28),
        legend.title = element_text(family = "RMN", size = 28),
        legend.text = element_text(family = "RMN", size = 28),
        legend.position = "bottom")
gpIPCAprebyGroupFN
# ggsave(gpIPCAprebyGroupFN, filename = "C:/Users/ylc/Desktop/CIPCA故障预测PRE.tiff", dpi = 600, height = 8, width = 13)


gpIPCArecbyGroupFN <- ggplot(data = dataIPCAIndexbyGroupFN %>% filter(Index=="Recall"), aes(x = Neurons, y = mean, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 4) +
  scale_fill_discrete(name = "模型:", breaks = c("Lower", "Upper", "Ensemble"), labels = c("下界基学习器", "上界基学习器", "集成学习模型")) +
  labs(x = "隐神经元数量", y = "Recall") +
  scale_x_continuous(breaks = c(10, 15, 20, 25, 30, 35, 40, 45)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1),
                     labels = c(0, "25%", "50%", "75%", "100%"),
                     limits = c(0, 1)) +
  labs(y = "召回率") +
  theme(axis.title.x = element_text(family = "RMN", size = 28),
        axis.text.x = element_text(family = "RMN", size = 28),
        axis.title.y = element_text(family = "RMN", size = 28),
        axis.text.y = element_text(family = "RMN", size = 28),
        strip.text = element_text(family = "RMN", size = 28),
        legend.title = element_text(family = "RMN", size = 28),
        legend.text = element_text(family = "RMN", size = 28),
        legend.position = "bottom")
gpIPCArecbyGroupFN
# ggsave(gpIPCArecbyGroupFN, filename = "C:/Users/ylc/Desktop/CIPCA故障预测REC.tiff", dpi = 600, height = 8, width = 13)



# # PCA
# # It shows that the effect of the integrated learner is better than that of the base learner
# # it's better as the number of neurons increases
dataPCAINdexFN <- tbl_df(dataPCA5IndexFN)
dataPCAIndexbyGroupFN <- dataPCAINdexFN %>%
  group_by(Neurons, Type, Index) %>%
  summarise(mean=mean(Value))
# gpPCAIndexbyGroupFN <- ggplot(data = dataPCAIndexbyGroupFN, aes(x = Neurons, y = mean, fill = Index)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Number of hidden neurons") +
#   scale_x_continuous(breaks = c(10, 15, 20, 25, 30, 35, 40)) +
#   facet_grid(.~Index) +
#   theme(axis.title.x = element_text(family = "RMN", size = 15),
#         axis.text.x = element_text(family = "RMN", size = 15),
#         axis.title.y = element_blank(),
#         axis.text.y = element_text(family = "RMN", size = 15),
#         strip.text = element_text(family = "RMN", size = 15),
#         legend.position = "none")
# gpPCAIndexbyGroupFN
# # ggsave(gpPCAIndexbyGroupFN, filename = "plotPCA5IndexbyGroupFN.tiff", dpi = 200)
# # write.csv(dataIPCAIndexbyGroupFN, "dataIPCAIndexCbyGroupFN.csv", row.names = FALSE)

# Compare the Index of PCA and IPCA 
dataIndexCompare <- rbind(dataIPCAIndexbyGroupFN %>% 
                            filter(Type=="Ensemble"),
                          dataPCAIndexbyGroupFN)
dataIndexCompare$Type[which(dataIndexCompare$Type=="Ensemble")] <- "IPCA"
dataIndexCompare$Type <- factor(dataIndexCompare$Type, levels = c("PCA", "IPCA"), ordered = TRUE)
# write.csv(dataIPCAIndexbyGroupFN, "CIPCA和PCA预测指标分组统计.csv", row.names = FALSE)

# gpCompareIndexFN <- ggplot(data = dataIndexCompare, aes(x = Neurons, y = mean, fill = Type)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_fill_discrete(name = "Method:", breaks = c("IPCA", "PCA"), labels = c("CIPCA-BPNN", "PCA-BPNN")) +
#   labs(x = "Number of hidden neurons") +
#   scale_x_continuous(breaks = c(10, 15, 20, 25, 30, 35, 40)) +
#   facet_grid(.~Index) +
#   theme(axis.title.x = element_text(family = "RMN", size = 15),
#         axis.text.x = element_text(family = "RMN", size = 15),
#         axis.title.y = element_blank(),
#         axis.text.y = element_text(family = "RMN", size = 15),
#         strip.text = element_text(family = "RMN", size = 15),
#         legend.title = element_text(family = "RMN", size = 15),
#         legend.text = element_text(family = "RMN", size = 15),
#         legend.position = "bottom")
# gpCompareIndexFN
# # ggsave(gpCompareIndexFN, filename = "plotPC5CompareIndexFN.tiff", dpi = 200)

gpCompareaccFN <- ggplot(data = dataIndexCompare%>%filter(Index=="Accuracy"), aes(x = Neurons, y = mean, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "模型:", breaks = c("PCA", "IPCA"), labels = c("PCA-BPNN", "CIPCA-BPNN")) +
  labs(x = "隐神经元数量", y = "准确率") +
  scale_x_continuous(breaks = c(10, 15, 20, 25, 30, 35, 40, 45)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1),
                     labels = c(0, "25%", "50%", "75%", "100%"),
                     limits = c(0, 1)) +
  theme(axis.title.x = element_text(family = "RMN", size = 28),
        axis.text.x = element_text(family = "RMN", size = 28),
        axis.title.y = element_text(family = "RMN", size = 28),
        axis.text.y = element_text(family = "RMN", size = 28),
        strip.text = element_text(family = "RMN", size = 28),
        legend.title = element_text(family = "RMN", size = 28),
        legend.text = element_text(family = "RMN", size = 28),
        legend.position = "bottom")
gpCompareaccFN
ggsave(gpCompareaccFN, filename = "C:/Users/ylc/Desktop/CIPCA和PCA故障预测ACC.tiff", dpi = 600, width = 13, height = 8)

gpComparepreFN <- ggplot(data = dataIndexCompare%>%filter(Index=="Precision"), aes(x = Neurons, y = mean, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "模型:", breaks = c("PCA", "IPCA"), labels = c("PCA-BPNN", "CIPCA-BPNN")) +
  labs(x = "隐神经元数量", y = "精确率") +
  scale_x_continuous(breaks = c(10, 15, 20, 25, 30, 35, 40, 45)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1),
                     labels = c(0, "25%", "50%", "75%", "100%"),
                     limits = c(0, 1)) +
  theme(axis.title.x = element_text(family = "RMN", size = 28),
        axis.text.x = element_text(family = "RMN", size = 28),
        axis.title.y = element_text(family = "RMN", size = 28),
        axis.text.y = element_text(family = "RMN", size = 28),
        strip.text = element_text(family = "RMN", size = 28),
        legend.title = element_text(family = "RMN", size = 28),
        legend.text = element_text(family = "RMN", size = 28),
        legend.position = "bottom")
gpComparepreFN
ggsave(gpComparepreFN, filename = "C:/Users/ylc/Desktop/CIPCA和PCA故障预测PRE.tiff", dpi = 600, width = 13, height = 8)

gpComparerecFN <- ggplot(data = dataIndexCompare%>%filter(Index=="Recall"), aes(x = Neurons, y = mean, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "模型:", breaks = c("PCA", "IPCA"), labels = c("PCA-BPNN", "CIPCA-BPNN")) +
  labs(x = "隐神经元数量", y = "召回率") +
  scale_x_continuous(breaks = c(10, 15, 20, 25, 30, 35, 40, 45)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1),
                     labels = c(0, "25%", "50%", "75%", "100%"),
                     limits = c(0, 1)) +
  theme(axis.title.x = element_text(family = "RMN", size = 28),
        axis.text.x = element_text(family = "RMN", size = 28),
        axis.title.y = element_text(family = "RMN", size = 28),
        axis.text.y = element_text(family = "RMN", size = 28),
        strip.text = element_text(family = "RMN", size = 28),
        legend.title = element_text(family = "RMN", size = 28),
        legend.text = element_text(family = "RMN", size = 28),
        legend.position = "bottom")
gpComparerecFN
ggsave(gpComparerecFN, filename = "C:/Users/ylc/Desktop/CIPCA和PCA故障预测REC.tiff", dpi = 600, width = 13, height = 8)
