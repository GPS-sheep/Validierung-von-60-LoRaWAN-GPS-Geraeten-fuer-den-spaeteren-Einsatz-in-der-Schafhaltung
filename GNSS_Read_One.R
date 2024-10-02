rm(list=ls()) # remove everything out of the workspace

library(tidyverse)

setwd("~/Documents/01_Uni/Master_Kiel/00_Master Arbeit")

{
# Device naming works as "Device Mapping", naming all 60 Devices. 
{
  device_mapping <- c(
    "0185EDE5" = 01,
    "018746F6" = 02,
    "0185EDC8" = 03,
    "0185EDD6" = 04,
    "018746FA" = 05,
    "0185EDE3" = 6,
    "018746EC" = 7,
    "0185EDEE" = 8,
    "0185EDC6" = 9,
    "0185EDC7" = 10,
    "0185EDCD" = 11,
    "018746EE" = 12,
    "018746FB" = 13,
    "018746E8" = 14,
    "0185EDEF" = 15,
    "0185EDDE" =	16,
    "01874703"=	17,
    "018746FC"=	18,
    "0185EDE1"=	19,
    "018746F8"=	20,
    "01874660"=	22,
    "01874764"=	23,
    "0187475D"=	24,
    "0187475A"=	25,
    "01874763"=	26,
    "01874751"=	27,
    "01874752"=	28,
    "01874769"=	29,
    "0187475F"=	30,
    "01874765"=	31,
    "01874773"=	32,
    "0187474D"=	33,
    "0187475C"=	34,
    "0187465D"=	35,
    "0187465F"=	36,
    "0187474F"=	37,
    "01874762"=	38,
    "01874754"=	39,
    "0187475E"=	40,
    "01874766"=	41,
    "01874770"=	42,
    "0187465E"=	43,
    "01874662"=	44,
    "01874775"=	45,
    "01874757"=	46,
    "0187465B"=	47,
    "01874661"=	48,
    "01874771"=	49,
    "01874777"=	50,
    "0187476D"=	51,
    "0187474C"=	52,
    "0187475B"=	53,
    "01874761"=	54,
    "0187476E"=	55,
    "01874759"=	56,
    "01874772"=	57,
    "01874755" = 58,
    "0187474B" = 59,
    "0187476A" = 60
  )
  
}

# Define a function to process each CSV file
 # This creates a dataframe named data. It has all the columns of the csv
 # in addition it has the columns created by data$COLUMN with first all NAs till
 # there a values assigned to the df.

process_csv <- function(file_path, device_mapping) {
  data <- read_csv(file_path)
  data$File_Dat <- NA
  data$Trial <- NA
  data$DevicesOut <- NA
  data$Gateway <- NA
  data$Bag <- NA
  data$DeviceNumber <- device_mapping[data$'Device Address']
  
  # Adjust columns for each CSV file
  # copy the following block, paste after the last assigned bag and remove the 
  # "#".
  # else if (grepl ("File_ending.csv", file_path)) {
    # data$Trial <- "Trial" #which Trial this csv was part of
    # data$DevivesOut <- Number # Number of Devices deployed 
    # data$Gateway <- "Gateway " #Which Gateway this csv is from
    # data$Bag[data$DeviceNumber %in% c(Device Numbers)] <- "BAG"  # for each Bag one line with c containing the Device Numbers, Bag saying what bag it was in.
    # if there was no Bag, simply put <- "No Bag" instead of [data$DeviceNumber ...]
      #}
  if (grepl("2023-08-07_Fensterbank", file_path)) {
    data$File_Dat <- "08_07"
    data$Trial <- "0.1"
    data$DevicesOut <- 5
    data$Gateway <- "Gateway NULL"
    data$Bag <- "Fensterbank"
  }
  else if (grepl("2023-08-07_Smart.csv", file_path)) {
  data$File_Dat <- "08_07"
  data$Trial <- "0.2"
  data$DevicesOut <- 30
  data$Gateway <- "Gateway NULL"
  data$Bag <- "Kofferraum"
} 
  else if (grepl("2023-08-28.csv", file_path)) {
  data$File_Dat <- "08_28"
  data$Trial <- "1.1"
  data$DevicesOut <- 29
  data$Gateway <- "Gateway NULL"
  data$Bag[data$DeviceNumber %in% c(13,14,17,08,07)] <- "Vorne 1"
  data$Bag[data$DeviceNumber %in% c(28,16,11,09,06)] <- "Vorne 2"
  data$Bag[data$DeviceNumber %in% c(21,20,23,26,15)] <- "Seite 1"
  data$Bag[data$DeviceNumber %in% c(30,27,03,19,01)] <- "Seite 2"
  data$Bag[data$DeviceNumber %in% c(12,24,29,25,10)] <- "Hinten 1"
  data$Bag[data$DeviceNumber %in% c(18,05,04,22)] <- "Hinten 2"
}
  else if (grepl("2023-08-29.csv", file_path)) {
    data$File_Dat <- "08_29"
    data$Trial <- "1.1"
    data$DevicesOut <- 30
    data$Gateway <- "Gateway NULL"
    data$Bag[data$DeviceNumber %in% c(40,43,46,33,44)] <- "Vorne 1"
    data$Bag[data$DeviceNumber %in% c(49,51,59,36,53)] <- "Vorne 2"
    data$Bag[data$DeviceNumber %in% c(50,41,57,38,56)] <- "Seite 1"
    data$Bag[data$DeviceNumber %in% c(31,34,37,47,32)] <- "Seite 2"
    data$Bag[data$DeviceNumber %in% c(55,42,39,58,60)] <- "Hinten 1"
    data$Bag[data$DeviceNumber %in% c(52,35,45,48,54)] <- "Hinten 2"
  }
  else if (grepl("2023-08-30.csv", file_path)) {
    data$File_Dat <- "08_30"
    data$Trial <- "1.1"
    data$DevicesOut <- 29
    data$Gateway <- "Gateway NULL"
    data$Bag[data$DeviceNumber %in% c(02,22,21,26)] <- "Vorne 1"
    data$Bag[data$DeviceNumber %in% c(20,05,30,10,15)] <- "Vorne 2"
    data$Bag[data$DeviceNumber %in% c(24,04,16,07,25)] <- "Seite 1"
    data$Bag[data$DeviceNumber %in% c(18,28,17,14,29)] <- "Seite 2"
    data$Bag[data$DeviceNumber %in% c(01,11,08,27,03)] <- "Hinten 1"
    data$Bag[data$DeviceNumber %in% c(09,23,19,13,06)] <- "Hinten 2"
  }
  else if (grepl("2023-08-31.csv", file_path)) {
    data$File_Dat <- "08_31"
    data$Trial <- "1.1"
    data$DevicesOut <- 30
    data$Gateway <- "Gateway NULL"
    data$Bag[data$DeviceNumber %in% c(37,31,34,50,57)] <- "Vorne 1"
    data$Bag[data$DeviceNumber %in% c(60,35,42,48,39)] <- "Vorne 2"
    data$Bag[data$DeviceNumber %in% c(44,52,45,53,55)] <- "Seite 1"
    data$Bag[data$DeviceNumber %in% c(43,33,59,54,58)] <- "Seite 2"
    data$Bag[data$DeviceNumber %in% c(36,46,49,32,38)] <- "Hinten 1"
    data$Bag[data$DeviceNumber %in% c(40,51,41,56,47)] <- "Hinten 2"
  }
  else if (grepl("2023-09-01.csv", file_path)) {
    data$File_Dat <- "09_01"
    data$Trial <- "1.2"
    data$DevicesOut <- 15
    data$Gateway <- "Gateway NULL"
    data$Bag[data$DeviceNumber %in% c(18,01,29,04)] <- "Vorne"
    data$Bag[data$DeviceNumber %in% c(09,13,08,10,15)] <- "Seite"
    data$Bag[data$DeviceNumber %in% c(16,07,26,12,20)] <- "Hinten"
  }
  else if (grepl("2023-09-03.csv", file_path)) {
    data$File_Dat <- "09_03"
    data$Trial <- "1.2" #which Trial this csv was part of
    data$DevicesOut <- 15 # Number of Devices deployed 
    data$Gateway <- "Gateway NULL" #Which Gateway this csv is from
    data$Bag[data$DeviceNumber %in% c(58,32,38,45,47)] <- "Vorne"
    data$Bag[data$DeviceNumber %in% c(49,40,39,46,48)] <- "Seite"
    data$Bag[data$DeviceNumber %in% c(60,43,34,57,53)] <- "Hinten"
  }
  else if (grepl("2023-09-04.csv", file_path)) {
    data$File_Dat <- "09_04"
    data$Trial <- "1.2" #which Trial this csv was part of
    data$DevicesOut <- 15 # Number of Devices deployed 
    data$Gateway <- "Gateway NULL" #Which Gateway this csv is from
    data$Bag[data$DeviceNumber %in% c(19,03,25,30,24)] <- "Vorne"
    data$Bag[data$DeviceNumber %in% c(11,22,09,05,23)] <- "Seite"
    data$Bag[data$DeviceNumber %in% c(21,14,07,28,17)] <- "Hinten"
  }
  else if (grepl("2023-09-05.csv", file_path)) {
    data$File_Dat <- "09_05"
    data$Trial <- "1.2"
    data$DevicesOut <- 15
    data$Gateway <- "Gateway NULL"
    data$Bag[data$DeviceNumber %in% c(41,55,52,56,54)] <- "Vorne"
    data$Bag[data$DeviceNumber %in% c(36,35,31,51,42)] <- "Seite"
    data$Bag[data$DeviceNumber %in% c(33,59,37,44,50)] <- "Hinten"
  }
  else if (grepl("2023-09-15.csv", file_path)) {
    data$File_Dat <- "09_15"
    data$Trial <- "1.3"
    data$DevicesOut <- 60
    data$Gateway <- "Gateway NULL"
    data$Bag <- "Pritsche"
  }
  
  # Add more conditions for other CSV files as needed
  
  # Modify other columns
 
  return(data)
}

# Set the directory where your CSV files are located
csv_dir <- "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/Stationary/CSV Data/One Gate"

# Initialize an empty list to store processed data frames
data_list <- list()

# Loop through CSV files in the directory
csv_files <- dir(csv_dir, pattern = ".csv")
for (file in csv_files) {
  file_path <- file.path(csv_dir, file)
  processed_data <- process_csv(file_path, device_mapping)
  data_list[[file]] <- processed_data
}

#csv_files_to_process <- c(
#  "GNSS_tracker_data_2023-08-07_Fensterbank.csv",
#  "GNSS_tracker_data_2023-08-07_SMART.csv",
#  "GNSS_tracker_data_2023-08-28.csv"
#)

#for (file in csv_files_to_process) {
#  file_path <- file.path(csv_dir, file)
#  processed_data <- process_csv(file_path, device_mapping)
#  data_list[[file]] <- processed_data
#}

# Combine all data frames into one
final_data <- bind_rows(data_list)

# View the final_data
head(final_data)


final_data$Date_Time <- as.POSIXct(final_data$`Date and time`, format="%Y-%m-%d %H:%M:%S")
final_data$Lat <- final_data$`Latitute [°]`
final_data$Lon <- final_data$`Longitude [°]`
final_data$V <- final_data$`Battery Voltage[V]`
final_data$Temp <- final_data$`Temperature [°C]`
final_data$Hum <- final_data$`Humidity [%]`
final_data[final_data == 0] <- NA
final_data$Lat[final_data$Lat < 0.001] <- NA
final_data$Lon[final_data$Lon < 0.001] <- NA


}



# Filter and select columns in one step
data_select_one <- subset(final_data, !is.na(Bag), select = c(Date_Time, DeviceNumber, Lat, Lon, V, File_Dat, Bag, Trial, DevicesOut, Gateway, RSSI, SNR, Temp, Hum))



# View the data_select
#head(data_select)

write_csv(data_select_one, file = "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/GatewayCSV/One_Gateway.csv")
