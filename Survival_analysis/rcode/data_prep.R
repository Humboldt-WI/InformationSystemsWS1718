
### PREPARATION ----
# DOWNLOAD PACKAGES
if(!require("data.table")) install.packages("data.table"); library("data.table")
memory.limit(30000)

filenames <- list.files("./Survival_analysis/Q1-Q4-2016", 
                        pattern="*.csv", full.names=TRUE)

hdd_data <- read.csv(file=filenames[1], header = TRUE)
hdd_data <- data.table(hdd_data)
allserials <- unique(hdd_data$serial_number)
allserials <- data.table(allserials)
saveRDS(hdd_data, paste0("./Survival_analysis/data_2016_rds/",toString(hdd_data$date[1]),".rds"))

for (i in 2:length(filenames)){
  data <- read.csv(file=filenames[i], header = TRUE)
  data <- data[data$serial_number %in% allserials$allserials,]
#  saveRDS(data, paste0("./Survival_analysis/data_2016_rds/",toString(data$date[1]),".rds"))
#  hdd_data <- rbind(hdd_data, data)
  print(i)
  print(NROW(unique(data$serial_number)))
}

filenamesrds <- list.files("./Survival_analysis/data_2016_rds", 
                        pattern="*.rds", full.names=TRUE)



for (i in 1:length(filenamesrds)){
  if(i==1){
    data <- readRDS(filenamesrds[i])
  }
  
  else if(i>1){
    dataap <- readRDS(filenamesrds[i])
    data <- rbind(data, dataap)
  }
  
}
saveRDS(data, paste0("./Survival_analysis/data_2016_rds/","all_2016",".rds"))
