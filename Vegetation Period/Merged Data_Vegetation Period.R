rm (list = ls())

#enter directory
path <- "directory"

files <- list.files(path = path, pattern = ".csv")

merged_data <- data.frame(date = character(), stringsAsFactors = FALSE)

for (i in seq_along(files)) {
  temp_data <- read.csv(paste(path, files[i], sep = "/"))
  col_name <- paste0("Cell_", i)
 names(temp_data)[names(temp_data) == "Sur_Temp"] <- col_name
 if (i == 1) {
   merged_data <- temp_data
 } else {
  merged_data <- merge(merged_data, temp_data, by = "time")
 }
}

#merge all the files in a single csv file
write.csv(merged_data, file = "merged_data.csv", row.names = FALSE)

data <- read.csv("merged_data.csv")

dates <- data[, 1]
temperatures <- data[, -1]

average_temperatures <- apply(temperatures, 1, mean)

kelvin_to_celsius <- function(kelvin) {
  celsius <- kelvin - 273.15
  return(celsius)
}
k2c <- kelvin_to_celsius(average_temperatures)

result <- data.frame(Date = dates, Avg_Temp = k2c)


write.csv(result, file = "Cell_avg_Vegetation.csv", row.names = FALSE)

df <- read.csv("Cell_avg_Vegetation.csv")

result_df <- data.frame(Year = numeric(), T = numeric(), DaysToReach200 = numeric())

for (year in 1969:2079) {
  cumulative_sum <- 0
  days_to_reach_200 <- 0
  
  year_data <- df[format(as.Date(df$Date), "%Y") == year, ]
  
  for (i in 1:nrow(year_data)) {
    month <- as.integer(format(as.Date(year_data$Date[i]), "%m"))
    x <- year_data$Avg_Temp[i]
    c <- 0
    
    if (month == 1) {
      c <- 0.5
    } else if (month == 2) {
      c <- 0.75
    } else {
      c <- 1
    }
    
    product <- x * c
    cumulative_sum <- cumulative_sum + product
    days_to_reach_200 <- days_to_reach_200 + 1
    
    if (cumulative_sum >= 200) {
      break
    }
  }
  result_df <- rbind(result_df, data.frame(Year = year, T = cumulative_sum, DaysToReach200 = days_to_reach_200))
}
View(result_df)
write.csv(result_df, file = "Vegetation_Period.csv", row.names = FALSE)
