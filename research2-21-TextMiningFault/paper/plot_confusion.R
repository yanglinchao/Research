setwd("C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault/paper")

library(ggplot2)
library(dplyr)
windowsFonts(RMN = windowsFont("Times New Roman"))


plot_size <- 28

# handle
data <- read.csv("result_handle_confusion.csv")
data$true[which(data$true==1)] <- "one"
data$true[which(data$true==2)] <- "two"
data$true[which(data$true==3)] <- "three"
data$pred[which(data$pred==1)] <- "one"
data$pred[which(data$pred==2)] <- "two"
data$pred[which(data$pred==3)] <- "three"
true <- factor(data$true, levels = c("one", "two", "three"), label = c("1", "2", "3"), ordered = TRUE)
pred <- factor(data$pred, levels = c("three", "two", "one"), label = c("3", "2", "1"), ordered = TRUE)
confusion <- as.data.frame(table(pred, true))
tile <- ggplot(data = confusion, aes(x = true, y = pred, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1, family = "RMN", size = 9) +
  scale_fill_gradient(low = "#F0FFFF", high = "#838BBB") +
  labs(x = "Real label", y = "Prediction") +
  theme(strip.text.y = element_text(family = "RMN", size = plot_size),
        axis.title.x = element_text(family = "RMN", size = plot_size),
        axis.text.x = element_text(family = "RMN", size = plot_size),
        axis.title.y = element_text(family = "RMN", size = plot_size),
        axis.text.y = element_text(family = "RMN", size = plot_size),
        axis.ticks = element_blank(),
        strip.text = element_text(family = "RMN", size = plot_size),
        legend.title = element_text(family = "RMN", size = plot_size),
        legend.text = element_text(family = "RMN", size = plot_size),
        legend.position = "none",
        panel.background = element_rect(fill = "#FFFFFF"))
tile
ggsave(tile, dpi = 1200, height = 8, width = 8, filename = paste("C:/Users/ylc/Desktop/handle_confusion.tiff", sep = ""))

# site
data <- read.csv("result_site_confusion.csv")
data$true[which(data$true==3)] <- "one"
data$true[which(data$true==4)] <- "two"
data$true[which(data$true==5)] <- "three"
data$true[which(data$true==6)] <- "four"
data$pred[which(data$pred==3)] <- "one"
data$pred[which(data$pred==4)] <- "two"
data$pred[which(data$pred==5)] <- "three"
data$pred[which(data$pred==6)] <- "four"
true <- factor(data$true, levels = c("one", "two", "three", "four"), label = c("1", "2", "3", "4"), ordered = TRUE)
pred <- factor(data$pred, levels = c("four", "three", "two", "one"), label = c("4", "3", "2", "1"), ordered = TRUE)

confusion <- as.data.frame(table(pred, true))
tile <- ggplot(data = confusion, aes(x = true, y = pred, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1, family = "RMN", size = 9) +
  scale_fill_gradient(low = "#F0FFFF", high = "#838BBB") +
  labs(x = "Real label", y = "Prediction") +
  theme(strip.text.y = element_text(family = "RMN", size = plot_size),
        axis.title.x = element_text(family = "RMN", size = plot_size),
        axis.text.x = element_text(family = "RMN", size = plot_size),
        axis.title.y = element_text(family = "RMN", size = plot_size),
        axis.text.y = element_text(family = "RMN", size = plot_size),
        axis.ticks = element_blank(),
        strip.text = element_text(family = "RMN", size = plot_size),
        legend.title = element_text(family = "RMN", size = plot_size),
        legend.text = element_text(family = "RMN", size = plot_size),
        legend.position = "none",
        panel.background = element_rect(fill = "#FFFFFF"))
tile
ggsave(tile, dpi = 1200, height = 8, width = 8, filename = paste("C:/Users/ylc/Desktop/site_confusion.tiff", sep = ""))
