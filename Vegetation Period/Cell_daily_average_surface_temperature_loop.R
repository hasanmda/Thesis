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
variable <- ncvar_get(nc, "ts")
col_indices <- c(var1, var1)
row_indices <- c(var2,var2)
avg_dayctsur <- numeric()
for (i in 1:length(ncvar_get(nc, "time"))) {
  cell_values <- variable[ col_indices, row_indices, i]
  avg_dayctsur[i] <- mean(cell_values)
}
nc_close(nc)
print(avg_dayctsur)
df_celltsur<- data.frame(time_dates, avg_dayctsur)

df_ctsur<- df_celltsur %>% rename("time"="time_dates", "Sur_Temp"= "avg_dayctsur")
class(df_ctsur$time)
df_ctsur$time<- as.Date(df_ctsur$time)

return(df_ctsur)
}

#enter cell coordinates, e.g. (x,y)
need <- myfunction(x, y)

#enter cell no for individual cell
write.csv(need, "cell no.csv", row.names = FALSE)
