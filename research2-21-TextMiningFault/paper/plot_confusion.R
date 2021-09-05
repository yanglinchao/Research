setwd("C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault/paper")

library(ggplot2)
library(dplyr)
windowsFonts(RMN = windowsFont("Times New Roman"))


plot_size <- 25

# handle
data <- read.csv("result_handle_confusion.csv")
true <- factor(data$true)
pred <- factor(data$pred)

confusion <- as.data.frame(table(pred, true))
tile <- ggplot(data = confusion, aes(x = true, y = pred, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1, family = "RMN", size = 8) +
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
true <- factor(data$true, levels = c(3, 4, 5, 6), labels = c(1, 2, 3, 4))
pred <- factor(data$pred, levels = c(3, 4, 5, 6), labels = c(1, 2, 3, 4))

confusion <- as.data.frame(table(pred, true))
tile <- ggplot(data = confusion, aes(x = true, y = pred, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1, family = "RMN", size = 8) +
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
