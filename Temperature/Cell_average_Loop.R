rm(list = ls())
library(ncdf4)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(chron)

#enter directory
directory<- setwd("directory name")

#enter NC file
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
#enter temperature type, e.g. - tasmax, tasmin, ts
variable <- ncvar_get(nc, "temperature type")
col_indices <- c(var1, var1)
row_indices <- c(var2,var2)
avg_dayctmin <- numeric()
for (i in 1:length(ncvar_get(nc, "time"))) {
  cell_values <- variable[ col_indices, row_indices, i]
  avg_dayct[i] <- mean(cell_values)
}
nc_close(nc)
print(avg_dayct)
df_cellt<- data.frame(time_dates, avg_dayct)


df_ct<- df_cellt %>% rename("time"="time_dates", "temparature type_Temp"= "avg_dayct")
class(df_ct$time)
df_ct$time<- as.Date(df_ct$time)

df_ct$year <- as.numeric(format(df_ct$time, "%Y"))
df_ct_yr<- aggregate(avg_dayct ~ year, data = df_ct, mean)
names(df_ct)


df_ct$month <- as.numeric(format(df_ct$time, "%m"))
df_ct_mon<- aggregate(temperature type_Temp ~ month + year, data = df_ct, mean)
df_ct_mon$date<- as.Date(paste(df_ct_mon$year, df_ct_mon$month, "01", sep = "-"))
df_ct_mon<- df_ct_mon[ , -c(1,2)]
df_ct_mon<- select(df_ct_mon, date, temperature type_Temp)

return(df_ct_mon)
}

#cell coordinates, (x,y)
need <- myfunction(x, y)

#simulate and #write a csv file for 107 cells
write.csv(need, "cellno.csv", row.names = FALSE)

