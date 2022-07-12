setwd("C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault/paper")

library(ggplot2)
library(dplyr)
windowsFonts(RMN = windowsFont("Times New Roman"))

data <- read.csv("result_handle.csv")
names(data) <- c("result", "index", "cut_type", "vectorsize", "algorithm", "paramter", "type", "k")
data$type <- factor(data$type, levels = c("control1", "control2", "goal"),
                    labels = c("Model 1", "Model 2", "Model 3"), ordered = TRUE)
data <- as_tibble(data)


plot_size <- 35

# »­LDA
# Accuracy
plot_index <- "Accuracy"
plot_cut_type <- "LDA"
data_plot <- data %>% filter(index == plot_index & cut_type == plot_cut_type)
bar <- ggplot(data = (data_plot), aes(x = vectorsize, y = result, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Model:") +
  scale_y_continuous(breaks = c(0, 0.3, 0.6, 0.9),
                     labels = c(0, 0.3, 0.6, 0.9),
                     limits = c(0, 0.9)) +
  scale_x_continuous(breaks = c(50, 100, 150, 200, 250),
                     labels = c(50, 100, 150, 200, 250),
                     limits = c(25, 275)) +
  labs(x = "Text vector length", y = plot_index) +
  theme(axis.title.x = element_text(family = "RMN", size = plot_size),
        axis.text.x = element_text(family = "RMN", size = plot_size),
        axis.title.y = element_text(family = "RMN", size = plot_size),
        axis.text.y = element_text(family = "RMN", size = plot_size),
        strip.text = element_text(family = "RMN", size = plot_size),
        legend.title = element_text(family = "RMN", size = plot_size),
        legend.text = element_text(family = "RMN", size = plot_size),
        legend.position = "bottom")
bar
ggsave(bar, dpi = 1200, height = 8, width = 7, filename = paste("C:/Users/ylc/Desktop/", plot_cut_type, "_", plot_index, ".tiff", sep = ""))

# Precision
plot_index <- "Precision"
plot_cut_type <- "LDA"
data_plot <- data %>% filter(index == plot_index & cut_type == plot_cut_type)
bar <- ggplot(data = (data_plot), aes(x = vectorsize, y = result, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Model:") +
  scale_y_continuous(breaks = c(0, 0.3, 0.6, 0.9),
                     labels = c(0, 0.3, 0.6, 0.9),
                     limits = c(0, 0.9)) +
  scale_x_continuous(breaks = c(50, 100, 150, 200, 250),
                     labels = c(50, 100, 150, 200, 250),
                     limits = c(25, 275)) +
  labs(x = "Text vector length", y = plot_index) +
  theme(axis.title.x = element_text(family = "RMN", size = plot_size),
        axis.text.x = element_text(family = "RMN", size = plot_size),
        axis.title.y = element_text(family = "RMN", size = plot_size),
        axis.text.y = element_text(family = "RMN", size = plot_size),
        strip.text = element_text(family = "RMN", size = plot_size),
        legend.title = element_text(family = "RMN", size = plot_size),
        legend.text = element_text(family = "RMN", size = plot_size),
        legend.position = "bottom")
bar
ggsave(bar, dpi = 1200, height = 8, width = 7, filename = paste("C:/Users/ylc/Desktop/", plot_cut_type, "_", plot_index, ".tiff", sep = ""))

# Recall
plot_index <- "Recall"
plot_cut_type <- "LDA"
data_plot <- data %>% filter(index == plot_index & cut_type == plot_cut_type)
bar <- ggplot(data = (data_plot), aes(x = vectorsize, y = result, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Model:") +
  scale_y_continuous(breaks = c(0, 0.3, 0.6, 0.9),
                     labels = c(0, 0.3, 0.6, 0.9),
                     limits = c(0, 0.9)) +
  scale_x_continuous(breaks = c(50, 100, 150, 200, 250),
                     labels = c(50, 100, 150, 200, 250),
                     limits = c(25, 275)) +
  labs(x = "Text vector length", y = plot_index) +
  theme(axis.title.x = element_text(family = "RMN", size = plot_size),
        axis.text.x = element_text(family = "RMN", size = plot_size),
        axis.title.y = element_text(family = "RMN", size = plot_size),
        axis.text.y = element_text(family = "RMN", size = plot_size),
        strip.text = element_text(family = "RMN", size = plot_size),
        legend.title = element_text(family = "RMN", size = plot_size),
        legend.text = element_text(family = "RMN", size = plot_size),
        legend.position = "bottom")
bar
ggsave(bar, dpi = 1200, height = 8, width = 7, filename = paste("C:/Users/ylc/Desktop/", plot_cut_type, "_", plot_index, ".tiff", sep = ""))

# F1-Score
plot_index <- "F1-Score"
plot_cut_type <- "LDA"
data_plot <- data %>% filter(index == plot_index & cut_type == plot_cut_type)
bar <- ggplot(data = (data_plot), aes(x = vectorsize, y = result, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Model:") +
  scale_y_continuous(breaks = c(0, 0.3, 0.6, 0.9),
                     labels = c(0, 0.3, 0.6, 0.9),
                     limits = c(0, 0.9)) +
  scale_x_continuous(breaks = c(50, 100, 150, 200, 250),
                     labels = c(50, 100, 150, 200, 250),
                     limits = c(25, 275)) +
  labs(x = "Text vector length", y = plot_index) +
  theme(axis.title.x = element_text(family = "RMN", size = plot_size),
        axis.text.x = element_text(family = "RMN", size = plot_size),
        axis.title.y = element_text(family = "RMN", size = plot_size),
        axis.text.y = element_text(family = "RMN", size = plot_size),
        strip.text = element_text(family = "RMN", size = plot_size),
        legend.title = element_text(family = "RMN", size = plot_size),
        legend.text = element_text(family = "RMN", size = plot_size),
        legend.position = "bottom")
bar
ggsave(bar, dpi = 1200, height = 8, width = 7, filename = paste("C:/Users/ylc/Desktop/", plot_cut_type, "_", plot_index, ".tiff", sep = ""))