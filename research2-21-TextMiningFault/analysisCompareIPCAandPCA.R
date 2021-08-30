# Compare PCA and IPCA

load("C:/Users/ylc/jgy/research/16-CIPCABPNN/test/sourceIntervalFlightData.RData")
load("C:/Users/ylc/jgy/research/16-CIPCABPNN/test/dataFNSlice.RData")
library(ggplot2)
library(dplyr)
library(psych)
windowsFonts(RMN = windowsFont("Times New Roman"))

# Step1
# Variance Explained

# IPCA
# Remove label data from list data that cannot be used for principal component analysis
listIPCAFlightFN <- listFlightFN
listIPCAFlightFN$dataLower <- listIPCAFlightFN$dataLower[, -c(1:4)]
listIPCAFlightFN$dataUpper <- listIPCAFlightFN$dataUpper[, -c(1:4)]
########Interval data principal component analysis
# Perform principal component analysis on fault data
listIPCAFNFD <- IntervalPCA(List = listIPCAFlightFN)
# Look at the principal component variance to interpret the results
listIPCAFNFD$ExplainedResults
# write.csv(listIPCAFNFD$ExplainedResults, "IPCAExplained.csv", row.names = FALSE)

# PCA
# Principal component analysis
dataFNSlice4PCA <- dataFNSlice[, -c(1:4)]
dataFNSlice4PCA <- data.frame(apply(dataFNSlice4PCA, MARGIN = 2, scale))
pcFN <- principal(dataFNSlice4PCA, nfactors = 30)
dataPCAExplainFN <- tbl_df(t(pcFN$Vaccounted))
dataPCAExplainFN <- dataPCAExplainFN %>% arrange(desc(`Proportion Explained`))
# write.csv(dataPCAExplainFN, "PCAExplained.csv", row.names = FALSE)

# Sorting data
dataCompareIPCAandPCA <- data.frame(NumComponent = rep(1:30, times = 4),
                                    Method = rep(c("IPCA", "PCA"), each = 60),
                                    Type = rep(c("ProportionExplaned", "CumulativeExplained"), each = 30, times = 2),
                                    Value = c(listIPCAFNFD$ExplainedResults$ProportionExplained[1:30], listIPCAFNFD$ExplainedResults$CumulativeExplained[1:30], dataPCAExplainFN$`Proportion Explained`, dataPCAExplainFN$`Cumulative Proportion`))
dataCompareIPCAandPCA$Value <- round(dataCompareIPCAandPCA$Value, 2)

# Drawn the chart
# gpCompareIPCAandPCA <- ggplot(data = dataCompareIPCAandPCA[which(dataCompareIPCAandPCA$Type=="CumulativeExplained"), ], aes(x = NumComponent, y = Value, color = Method)) +
#   geom_line(size = 1) +
#   geom_point(aes(shape = Method), size = 2) +
#   scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
#   scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
#   labs(x = "Number of Components", y = "Cumulative Explained") +
#   theme(axis.title.x = element_text(family = "RMN", size = 15),
#         axis.text.x = element_text(family = "RMN", size = 15),
#         axis.title.y = element_text(family = "RMN", size = 15),
#         axis.text.y = element_text(family = "RMN", size = 15),
#         legend.title = element_text(family = "RMN", size = 15),
#         legend.text = element_text(family = "RMN", size = 15))
gpCompareIPCAandPCA <- ggplot() +
  geom_point(data = dataCompareIPCAandPCA[which(dataCompareIPCAandPCA$Type=="CumulativeExplained"), ], aes(x = NumComponent, y = Value, shape = Method), size = 2) +
  geom_line(data = dataCompareIPCAandPCA[which((dataCompareIPCAandPCA$Method == "IPCA")&(dataCompareIPCAandPCA$Type == "CumulativeExplained")), ], aes(x = NumComponent, y = Value), size = 1.05) +
  geom_line(data = dataCompareIPCAandPCA[which((dataCompareIPCAandPCA$Method == "PCA")&(dataCompareIPCAandPCA$Type == "CumulativeExplained")), ], aes(x = NumComponent, y = Value), size = 1.05) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  labs(x = "主成分数量", y = "方差累计解释比例") +
  scale_shape_discrete(name = "方法:", breaks = c("IPCA", "PCA"), labels = c("CIPCA", "PCA")) +
  theme(axis.title.x = element_text(family = "RMN", size = 15),
        axis.text.x = element_text(family = "RMN", size = 15),
        axis.title.y = element_text(family = "RMN", size = 15),
        axis.text.y = element_text(family = "RMN", size = 15),
        legend.title = element_text(family = "RMN", size = 15),
        legend.text = element_text(family = "RMN", size = 15))
gpCompareIPCAandPCA
# ggsave(gpCompareIPCAandPCA, filename = "C:/Users/ylc/Desktop/CIPCA和PCA主成分比较.tiff", dpi = 1200, width = 14, height = 7.127)



# # Compare the component score
# 
# # Caculate the score of 2 factors
# # IPCA
# listScoreIPCA <- IntervalPCAScore(listIPCAFlightFN, listIPCAFNFD, n=30)
# dataScoreIPCA <- data.frame(cbind(listScoreIPCA$dataLower[, c(1:30)], listScoreIPCA$dataUpper[, c(1:30)]))
# names(dataScoreIPCA) <- c(paste("Lower", seq(1:30), sep = ""), paste("Upper", seq(1:30), sep = ""))
# dataScoreIPCA <- data.frame(apply(dataScoreIPCA, MARGIN = 2, scale))
# dataScoreIPCA$Type2 <- factor(as.numeric(listFlightFN$dataLower$type != 0), levels = c(0, 1), labels = c("Normal", "Fault"))
# # PCA
# dataScorePCA <- data.frame(pcFN$scores)
# names(dataScorePCA) <- c(paste("C", seq(1:30), sep = ""))
# dataScorePCA <- data.frame(apply(dataScorePCA, MARGIN = 2, scale))
# dataScorePCA$Type2 <- factor(as.numeric(listFlightFN$dataLower$type != 0), levels = c(0, 1), labels = c("Normal", "Fault"))
# 
# # Compare C1 and C2
# # IPCA
# dataScoreIPCAC12 <- data.frame(cbind(dataScoreIPCA[, c(1, 2)], dataScoreIPCA[, c(31, 32)]))
# dataScoreIPCAC12$Type2 <- dataScoreIPCA$Type2
# gpScoreIPCAC12 <- ggplot(data = dataScoreIPCAC12) +
#   geom_rect(aes(xmin = Lower1, xmax = Upper1, ymin = Lower2, ymax = Upper2, fill = Type2), alpha = I(1/5))
# gpScoreIPCAC12
# # PCA
# dataScorePCA12 <- dataScorePCA[, c(1, 2)]
# dataScorePCA12$Type2 <- dataScorePCA$Type2
# gpScorePCA12 <- ggplot(data = dataScorePCA12) +
#   geom_point(aes(x = C1, y = C2, color = Type2), alpha = I(1/5))
# gpScorePCA12

