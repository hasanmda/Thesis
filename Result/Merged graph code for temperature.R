rm (list = ls())

library(gridExtra)
library(ggplot2)
library(lubridate)
library(dplyr)
library(grid)
library(cowplot)

#enter directory
setwd("directory")

plot_max <- function(data, start_year, end_year) {
  
  row_means <- rowMeans(data[, 2:108], na.rm = TRUE) -273.15
  
  average_T <- data.frame(Date = data$date, Mean_T = row_means)
  
  average_df <- average_T %>%
    mutate(Year = year(Date), Month = month(Date))
  
  yearly_means <- average_df %>%
    group_by(Year) %>%
    summarize(Mean_Temperature = mean(Mean_T, na.rm = TRUE))
  
  graph_years <- yearly_means[yearly_means$Year >= start_year & yearly_means$Year <= end_year, ]
  
  ggplot(graph_years, aes(x = Year, y = Mean_Temperature, color = Year)) +
    geom_point(size = 3) + geom_line(color = "red") +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    scale_color_gradient(low = "blue", high = "red") +
    guides(color = FALSE) +
    scale_x_continuous(breaks = seq(start_year, end_year, by = 2)) +
    labs(
      x = "Year",
      y = "Mean Temperature (°C)",
      color = "Year"
    ) +
    theme_classic() 
}
data_1 <- read.csv("Cell Average Maximum Temperature_2.6.csv")
data_2 <- read.csv("Cell Average Maximum Temperature_4.5.csv")
data_3 <- read.csv("Cell Average Maximum Temperature_8.5.csv")
data_4 <- read.csv("Cell Average Maximum Temperature_HadGEM.csv")
data_5 <- read.csv("Cell Average Maximum Temperature_MPI.csv")

#enter time span, x= starting year, y= ending year
Plot_1 <- plot_max(data_1, x, y) + ggtitle("ICHEC-EC-EARTH RCP-2.6")+ theme(plot.title = element_text(hjust = 0.5))
Plot_2 <- plot_max(data_2, x, y) + ggtitle("ICHEC-EC-EARTH RCP-4.5")+ theme(plot.title = element_text(hjust = 0.5))
Plot_3 <- plot_max(data_3, x, y) + ggtitle("ICHEC-EC-EARTH RCP-8.5")+ theme(plot.title = element_text(hjust = 0.5))
Plot_4 <- plot_max(data_4, x, y) + ggtitle("MOHC-HadGEM")+ theme(plot.title = element_text(hjust = 0.5))
Plot_5 <- plot_max(data_5, x, y) + ggtitle("MPI") + theme(plot.title = element_text(hjust = 0.5))

cowplot::plot_grid(
  cowplot::plot_grid(Plot_1, Plot_2, Plot_3, Plot_4, ncol = 2, nrow = 2),
  cowplot::plot_grid(NULL, Plot_5, NULL, rel_widths = c(0.5, 1, 0.5), nrow = 1),
  nrow = 2,
  rel_heights = c(2, 1)
)

