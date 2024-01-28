rm(list = ls())
library(ncdf4)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(chron)

#enter directory
directory<- setwd("directory")
#enter NC File
df<- nc_open("NC File")
varnames<-df$var
names(varnames)

time <- ncvar_get(df,"time")
head(time)
time_vals<- time[]

tunits <- ncatt_get(df,"time","units")
ref_date <- as.Date("1949-12-01")
ref_time <- as.POSIXct(ref_date)

time_units <- "days"
time_dates <- ref_time + time_vals * as.difftime(1, units = time_units)
head(time_dates)

myfunction <- function(var1, var2)
{
  nc <- nc_open("NC File")
variable <- ncvar_get(nc, "pr")
col_indices <- c(var1, var1)
row_indices <- c(var2,var2)
avg_daypr <- numeric()
for (i in 1:length(ncvar_get(nc, "time"))) {
  cell_values <- variable[ col_indices, row_indices, i]
  avg_daypr[i] <- mean(cell_values)
}
nc_close(nc)
print(avg_daypr)
df_cellpr<- data.frame(time_dates, avg_daypr)

df_pr<- df_cellpr %>% rename("time"="time_dates", "Precipitation"= "avg_daypr")
class(df_pr$time)
df_pr$time<- as.Date(df_pr$time)

return(df_pr)
}

#enter coordinates, e.g. (x,y)
need <- myfunction(x, y)

#write csv file for each climate cell
write.csv(need, "cell no.csv", row.names = FALSE)

