# plot ROC
# Compare IPCA and PCA
load("C:/Users/ylc/jgy/research/16-CIPCABPNN/test/dataResultPC5.RData")
library(ggplot2)
library(dplyr)
windowsFonts(RMN = windowsFont("Times New Roman"))

dataIPCAROC <- dataIPCA5ROC
dataPCAROC <- dataPCA5ROC

dataIPCAROC$Method <- rep("IPCA", nrow(dataIPCAROC))
dataPCAROC$Method <- rep("PCA", nrow(dataPCAROC))

dataROC <- rbind(dataIPCAROC, dataPCAROC)
dataROC$Neurons <- factor(dataROC$Neurons,
                          levels = c(10, 15, 20, 25, 30, 35, 40, 45),
                          labels = c("隐神经元数量：10", "隐神经元数量：15", "隐神经元数量：20", "隐神经元数量：25", "隐神经元数量：30", "隐神经元数量：35", "隐神经元数量：40", "隐神经元数量：45"))
dataROC$Method <- factor(dataROC$Method, levels = c("PCA", "IPCA"), ordered = TRUE)
gpROCbyMethod <- ggplot(data = dataROC %>% filter(Type=="mean")) +
  geom_line(aes(x = FPR, y = TPR, color = Method), size = 2) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c(0, "25%", "50%", "75%", "100%")) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c(0, "25%", "50%", "75%", "100%")) +
  scale_color_discrete(name = "模型:", breaks = c("PCA", "IPCA"), labels = c("PCA-BPNN", "CIPCA-BPNN")) +
  labs(x = "假正率（False positive rate, FPR）", y = "真正率（True positive rate, TPR）") +
  facet_wrap(.~Neurons, nrow = 4) +
  theme(axis.title.x = element_text(family = "RMN", size = 25),
        axis.text.x = element_text(family = "RMN", size = 25),
        axis.title.y = element_text(family = "RMN", size = 25),
        axis.text.y = element_text(family = "RMN", size = 25),
        strip.text = element_text(family = "RMN", size = 25),
        legend.title = element_text(family = "RMN", size = 25),
        legend.text = element_text(family = "RMN", size = 25),
        legend.position = "bottom")
gpROCbyMethod
ggsave(gpROCbyMethod, filename = "C:/Users/ylc/Desktop/ROC曲线图.tiff", dpi = 600, width = 10, height = 14)
