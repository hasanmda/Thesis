rm (list = ls())
#give the directory of the csv files
path <- "directory"

files <- list.files(path = path, pattern = ".csv")

merged_data <- data.frame(date = character(), stringsAsFactors = FALSE)

for (i in seq_along(files)) {
  temp_data <- read.csv(paste(path, files[i], sep = "/"))
  col_name <- paste0("Cell_", i)
  #put temperature type
 names(temp_data)[names(temp_data) == "temperature type_Temp"] <- col_name 
 if (i == 1) {
   merged_data <- temp_data
 } else {
  merged_data <- merge(merged_data, temp_data, by = "date")
 }
 }

#write a combined csv file 
write.csv(merged_data, file = "merged_data.csv", row.names = FALSE)
