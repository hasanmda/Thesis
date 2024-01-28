rm (list = ls())

library(gridExtra)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyverse)

setwd("D:/BTU Cottbuss/Thesis/Data analysed/All data")

plot_P <- function(data, start_year, end_year) {
  str(data)
  graph_Veg_period <- data[data$Year >= start_year & data$Year <= end_year, ]
  ggplot(graph_Veg_period, aes(x = Year, y = DaysToReach200)) +
    geom_bar(stat = "identity", color = "darkgreen", fill = "lightgreen")+
    geom_smooth(aes(group = 1), method = "loess", se = FALSE, color = "blue")+
    scale_x_continuous(breaks = seq(start_year, end_year, by = 1)) +
    labs(
      x = "Year",
      y = "Days required to reach\nstart of vegetation period",
      color = "Year") +
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}
data_1 <- read.csv("Vegetation_Period_2.6.csv")
data_2 <- read.csv("Vegetation_Period_4.5.csv")
data_3 <- read.csv("Vegetation_Period_8.5.csv")
data_4 <- read.csv("Vegetation_Period_HadGEM.csv")
data_5 <- read.csv("Vegetation_Period_MPI.csv")

Plot_1 <- plot_P(data_1, 2050, 2069) + ggtitle("ICHEC-EC-EARTH RCP-2.6")+ theme(plot.title = element_text(hjust = 0.5))
Plot_2 <- plot_P(data_2, 2050, 2069) + ggtitle("ICHEC-EC-EARTH RCP-4.5")+ theme(plot.title = element_text(hjust = 0.5))
Plot_3 <- plot_P(data_3, 2050, 2069) + ggtitle("ICHEC-EC-EARTH RCP-8.5")+ theme(plot.title = element_text(hjust = 0.5))
Plot_4 <- plot_P(data_4, 2050, 2069) + ggtitle("MOHC-HadGEM")+ theme(plot.title = element_text(hjust = 0.5))
Plot_5 <- plot_P(data_5, 2050, 2069) + ggtitle("MPI") + theme(plot.title = element_text(hjust = 0.5))

cowplot::plot_grid(
  cowplot::plot_grid(Plot_1, Plot_2, Plot_3, Plot_4, ncol = 2, nrow = 2),
  cowplot::plot_grid(NULL, Plot_5, NULL, rel_widths = c(0.5, 1, 0.5), nrow = 1),
  nrow = 2,
  rel_heights = c(2, 1)
)

merged_data <- merge(data_1 [,-2], data_2[,-2], by = "Year")
merged_data <- merge(merged_data, data_3[,-2], by = "Year")
merged_data <- merge(merged_data, data_4[,-2], by = "Year")
merged_data <- merge(merged_data, data_5[,-2], by = "Year")

write.csv(merged_data, file = "Vegetation Merged.csv", row.names = FALSE)
Merged_data <- merge(data_1[ ,-2], data_2[ ,-2], data_3[ ,-2], data_4[ ,-2], data_5[ ,-2],
                     by.x = "Year",
                     by.y = "Year", all.x = all, all.y = all)
