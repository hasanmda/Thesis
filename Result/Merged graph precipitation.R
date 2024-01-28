rm (list = ls())

library(ggplot2)
library(lubridate)
library(dplyr)
library(cowplot)

#enter directory
setwd("directory")

avg_P <- function(data, start_year, end_year) {
  
  row_means <- rowMeans(data[, 2:108], na.rm = TRUE)
  
  average_P <- data.frame(Date = data$time, Mean_P = row_means)
  
  str(average_P)
  
  average_df <- average_P %>%
    mutate(Year = year(Date), Month = month(Date))
  
  str(average_df)
  
  monthly_means <- average_df %>%
    group_by(Year, Month) %>%  
    summarize(Mean_Precipitation = mean(Mean_P, na.rm = TRUE))
  
  monthly_means$Month <- factor(month.name[monthly_means$Month], levels = month.name)
  
  monthly_means_selective <- monthly_means[monthly_means$Year >= start_year & monthly_means$Year <= end_year, ]
  
  avg_monthly_precip <- aggregate(Mean_Precipitation ~ Month, data = monthly_means_selective, FUN = mean)

}
data_1 <- read.csv("Precipitation 2.6.csv")
data_2 <- read.csv("Precipitation 4.5.csv")
data_3 <- read.csv("Precipitation 8.5.csv")
data_4 <- read.csv("Precipitation HadGEM.csv")
data_5 <- read.csv("Precipitation MPI.csv")

data_11 <- avg_P(data_1, 2010, 2029)
data_12 <- avg_P(data_1, 2050, 2069)
C1 <- data.frame(Month = data_11$Month, C11 = data_11[,-1], C12 = data_12[,-1])

data_21 <- avg_P(data_2, 2010, 2029)
data_22 <- avg_P(data_2, 2050, 2069)
C2 <- data.frame(Month = data_21$Month, C21 = data_21[,-1], C22 = data_22[,-1])

data_31 <- avg_P(data_3, 2010, 2029)
data_32 <- avg_P(data_3, 2050, 2069)
C3 <- data.frame(Month = data_31$Month, C31 = data_31[,-1], C32 = data_32[,-1])

data_41 <- avg_P(data_4, 2010, 2029)
data_42 <- avg_P(data_4, 2050, 2069)
C4 <- data.frame(Month = data_41$Month, C41 = data_41[,-1], C42 = data_42[,-1])

data_51 <- avg_P(data_5, 2010, 2029)
data_52 <- avg_P(data_5, 2050, 2069)
C5 <- data.frame(Month = data_51$Month, C51 = data_51[,-1], C52 = data_52[,-1])


cp1 <- data.frame(
  Month = rep(data_11$Month, 2),
  Precipitation = c(data_11[,-1], data_12[,-1]),
  Year = rep(c("2010-2029", "2050-2069"), each = 12))
cp2 <- data.frame(
  Month = rep(data_21$Month, 2),
  Precipitation = c(data_21[,-1], data_22[,-1]),
  Year = rep(c("2010-2029", "2050-2069"), each = 12))
cp3 <- data.frame(
  Month = rep(data_31$Month, 2),
  Precipitation = c(data_31[,-1], data_32[,-1]),
  Year = rep(c("2010-2029", "2050-2069"), each = 12))
cp4 <- data.frame(
  Month = rep(data_41$Month, 2),
  Precipitation = c(data_41[,-1], data_42[,-1]),
  Year = rep(c("2010-2029", "2050-2069"), each = 12))
cp5 <- data.frame(
  Month = rep(data_51$Month, 2),
  Precipitation = c(data_51[,-1], data_52[,-1]),
  Year = rep(c("2010-2029", "2050-2069"), each = 12))

P1 <- ggplot(cp1, aes(x = Month, y = Precipitation, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.8) +
  labs(title = "ICHEC-EC-EARTH RCP 2.6",
       x = "Month",
       y = "Average Precipitation (mm)") +
  scale_fill_manual(values = c("2010-2029"="lightblue","2050-2069"="firebrick")) +
  theme_minimal() +
  theme_classic() +
  theme(legend.position = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))
P2 <- ggplot(cp2, aes(x = Month, y = Precipitation, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.8) +
  labs(title = "ICHEC-EC-EARTH RCP 4.5",
       x = "Month",
       y = "Average Precipitation (mm)") +
  scale_fill_manual(values = c("2010-2029"="lightblue","2050-2069"="firebrick")) +
  theme_minimal() +
  theme_classic() +
  theme(legend.position = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))
P3 <- ggplot(cp3, aes(x = Month, y = Precipitation, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.8) +
  labs(title = "ICHEC-EC-EARTH RCP 8.5",
       x = "Month",
       y = "Average Precipitation (mm)") +
  scale_fill_manual(values = c("2010-2029"="lightblue","2050-2069"="firebrick")) +
  theme_minimal() +
  theme_classic() +
  theme(legend.position = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))
P4 <- ggplot(cp4, aes(x = Month, y = Precipitation, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.8) +
  labs(title = "MOHC-HadGEM",
       x = "Month",
       y = "Average Precipitation (mm)") +
  scale_fill_manual(values = c("2010-2029"="lightblue","2050-2069"="firebrick")) +
  theme_minimal() +
  theme_classic() +
  theme(legend.position = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))
P5 <- ggplot(cp5, aes(x = Month, y = Precipitation, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.8) +
  labs(title = "MPI",
       x = "Month",
       y = "Average Precipitation (mm)") +
  scale_fill_manual(values = c("2010-2029"="lightblue","2050-2069"="firebrick")) +
  theme_minimal() +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))


cowplot::plot_grid(
  cowplot::plot_grid(P1, P2, P3, P4, ncol = 2, nrow = 2),
  cowplot::plot_grid(NULL, P5, NULL, rel_widths = c(0.5, 1, 0.5), nrow = 1),
  nrow = 2,
  rel_heights = c(2, 1)
)
