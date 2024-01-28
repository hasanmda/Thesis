rm (list = ls())
#enter directory
path <- "directory"

files <- list.files(path = path, pattern = ".csv")

merged_data <- data.frame(date = character(), stringsAsFactors = FALSE)

for (i in seq_along(files)) {
  pr_data <- read.csv(paste(path, files[i], sep = "/"))
  col_name <- paste0("Cell_", i)
 names(pr_data)[names(pr_data) == "Precipitation"] <- col_name
 if (i == 1) {
   merged_data <- pr_data
 } else {
  merged_data <- merge(merged_data, pr_data, by = "time")
 }
 }

#merged all the cell data into one common csv file
write.csv(merged_data, file = "merged data.csv", row.names = FALSE)
