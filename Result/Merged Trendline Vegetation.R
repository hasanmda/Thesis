rm (list = ls())

library(gridExtra)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyverse)
library(patchwork)

setwd("D:/BTU Cottbuss/Thesis/Data analysed/All data")

data_1 <- read.csv("Vegetation_Period_2.6.csv") %>%
  filter(Year >= 2010 & Year <= 2029)
data_2 <- read.csv("Vegetation_Period_4.5.csv") %>%
  filter(Year >= 2010 & Year <= 2029)
data_3 <- read.csv("Vegetation_Period_8.5.csv")  %>%
  filter(Year >= 2010 & Year <= 2029)
data_4 <- read.csv("Vegetation_Period_HadGEM.csv")  %>%
  filter(Year >= 2010 & Year <= 2029)
data_5 <- read.csv("Vegetation_Period_MPI.csv")  %>%
  filter(Year >= 2010 & Year <= 2029)

data_6 <- read.csv("Vegetation_Period_2.6.csv") %>%
  filter(Year >= 2050 & Year <= 2069)
data_7 <- read.csv("Vegetation_Period_4.5.csv") %>%
  filter(Year >= 2050 & Year <= 2069)
data_8 <- read.csv("Vegetation_Period_8.5.csv")  %>%
  filter(Year >= 2050 & Year <= 2069)
data_9 <- read.csv("Vegetation_Period_HadGEM.csv")  %>%
  filter(Year >= 2050 & Year <= 2069)
data_10 <- read.csv("Vegetation_Period_MPI.csv")  %>%
  filter(Year >= 2050 & Year <= 2069)

c <- data.frame(Year = data_6$Year, D1 = data_6[,-2], D2 = data_7[,-2],
                D3 = data_8[,-2], D4 = data_9[,-2], D5 = data_10[,-2]
                )
write.csv(c, file = "Merged Veg.csv", row.names = FALSE)
figure1 <- ggplot() + 
  geom_smooth(data = data_1, aes(x= Year, y= DaysToReach200, color = "black")) + 
  geom_smooth(data = data_2, aes(x= Year, y= DaysToReach200, color = "red")) +
  geom_smooth(data = data_3, aes(x= Year, y= DaysToReach200, color = "green")) + 
  geom_smooth(data = data_4, aes(x= Year, y= DaysToReach200, color = "purple")) + 
  geom_smooth(data = data_5, aes(x= Year, y= DaysToReach200, color = "blue")) +
  theme_classic()+
  labs(x="Year", y="Days required to reach\nstart of vegetation period")+
  scale_color_manual(
    name = "Model",
    values = c("black", "red", "green", "purple", "blue"),
    labels = c("ICHEC-EC-EARTH RCP-2.6", "ICHEC-EC-EARTH RCP-4.5", "ICHEC-EC-EARTH RCP-8.5", "MOHC-HadGEM", "MPI"))

figure1

figure2 <- ggplot() + 
  geom_smooth(data = data_6, aes(x= Year, y= DaysToReach200, color = "black")) + 
  geom_smooth(data = data_7, aes(x= Year, y= DaysToReach200, color = "red")) +
  geom_smooth(data = data_8, aes(x= Year, y= DaysToReach200, color = "green")) + 
  geom_smooth(data = data_9, aes(x= Year, y= DaysToReach200, color = "purple")) + 
  geom_smooth(data = data_10, aes(x= Year, y= DaysToReach200, color = "blue")) +
  theme_classic()+
labs(x="Year", y="Days required to reach\nstart of vegetation period")+
  scale_color_manual(
    name = "Model",
    values = c("black", "red", "green", "purple", "blue"),
    labels = c("ICHEC-EC-EARTH RCP-2.6", "ICHEC-EC-EARTH RCP-4.5", "ICHEC-EC-EARTH RCP-8.5", "MOHC-HadGEM", "MPI"))

figure2
merged_plot <- plot_grid(
  figure1 + theme(legend.position="none"),  
  figure2 + theme(legend.position="left"),
  ncol = 2,
  align = "h")
merged_plot
