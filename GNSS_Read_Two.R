rm(list=ls()) # remove everything out of the workspace

library(tidyverse)
library(lubridate)
library(dplyr)

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
    if (grepl("2023-09-25_LoRa Gateway1.csv", file_path)) {
      data$File_Dat <- "09_25"
      data$Trial <- "2.0"
      data$DevicesOut <- 30
      data$Gateway <- "Gateway 1"
      data$Bag <- "Fensterbank"
    }
    else if (grepl("2023-09-25_LoRa Gateway2.csv", file_path)) {
      data$File_Dat <- "09_25"
      data$Trial <- "2.0"
      data$DevicesOut <- 30
      data$Gateway <- "Gateway 2"
      data$Bag <- "Fensterbank"
    }
    else if (grepl("2023-09-27_LoRa Gateway1.csv", file_path)) {
      data$File_Dat <- "09_27"
      data$Trial <- "2.0"
      data$DevicesOut <- 30
      data$Gateway <- "Gateway 1"
      data$Bag <- "Fensterbank"
    }
    else if (grepl("2023-09-27_LoRa Gateway2.csv", file_path)) {
      data$File_Dat <- "09_27"
      data$Trial <- "2.0"
      data$DevicesOut <- 30
      data$Gateway <- "Gateway 2"
      data$Bag <- "Fensterbank"
    }
    else if(grepl("2023-10-10_1.csv", file_path)) {
      data$File_Dat <- "10_10"
      data$Trial <- "2.1"
      data$DevicesOut <- 30
      data$Gateway <- "Gateway 1"
      data$Bag[data$DeviceNumber %in% c(42,06,16,20,38)] <- "Vorne 1"
      data$Bag[data$DeviceNumber %in% c(37,39,03,35,17)] <- "Vorne 2"
      data$Bag[data$DeviceNumber %in% c(15,10,48,01,24)] <- "Seite 1"
      data$Bag[data$DeviceNumber %in% c(12,54,52,57,13)] <- "Seite 2"
      data$Bag[data$DeviceNumber %in% c(04,02,60,08,55)] <- "Hinten 1"
      data$Bag[data$DeviceNumber %in% c(53,27,18,28,31)] <- "Hinten 2"
    }  
    else if (grepl("2023-10-10_2.csv", file_path)) {
      data$File_Dat <- "10_10"
      data$Trial <- "2.1"
      data$DevicesOut <- 30
      data$Gateway <- "Gateway 2"
      data$Bag[data$DeviceNumber %in% c(42,06,16,20,38)] <- "Vorne 1"
      data$Bag[data$DeviceNumber %in% c(37,39,03,35,17)] <- "Vorne 2"
      data$Bag[data$DeviceNumber %in% c(15,10,48,01,24)] <- "Seite 1"
      data$Bag[data$DeviceNumber %in% c(12,54,52,57,13)] <- "Seite 2"
      data$Bag[data$DeviceNumber %in% c(04,02,60,08,55)] <- "Hinten 1"
      data$Bag[data$DeviceNumber %in% c(53,27,18,28,31)] <- "Hinten 2"
    }
    else if (grepl("2023-10-11_1.csv", file_path)) {
      data$File_Dat <- "10_11"
      data$Trial <- "2.1"
      data$DevicesOut <- 30
      data$Gateway <- "Gateway 1"
      data$Bag[data$DeviceNumber %in% c(09,25,41,23,46)] <- "Vorne 1"
      data$Bag[data$DeviceNumber %in% c(49,50,26,40,30)] <- "Vorne 2"
      data$Bag[data$DeviceNumber %in% c(21,33,11,05,59)] <- "Seite 1"
      data$Bag[data$DeviceNumber %in% c(45,44,19,14,58)] <- "Seite 2"
      data$Bag[data$DeviceNumber %in% c(43,51,47,29,36)] <- "Hinten 1"
      data$Bag[data$DeviceNumber %in% c(07,22,34,32,54)] <- "Hinten 2"
    }
    else if (grepl("2023-10-11_2.csv", file_path)) {
      data$File_Dat <- "10_11"
      data$Trial <- "2.1"
      data$DevicesOut <- 30
      data$Gateway <- "Gateway 2"
      data$Bag[data$DeviceNumber %in% c(09,25,41,23,46)] <- "Vorne 1"
      data$Bag[data$DeviceNumber %in% c(49,50,26,40,30)] <- "Vorne 2"
      data$Bag[data$DeviceNumber %in% c(21,33,11,05,59)] <- "Seite 1"
      data$Bag[data$DeviceNumber %in% c(45,44,19,14,58)] <- "Seite 2"
      data$Bag[data$DeviceNumber %in% c(43,51,47,29,36)] <- "Hinten 1"
      data$Bag[data$DeviceNumber %in% c(07,22,34,32,54)] <- "Hinten 2"
    }
    else if (grepl("2023-10-13_1.csv", file_path)) {
      data$File_Dat <- "10_13"
      data$Trial <- "2.1"
      data$DevicesOut <- 30
      data$Gateway <- "Gateway 1"
      data$Bag[data$DeviceNumber %in% c(13,06,03,12,42)] <- "Vorne 1"
      data$Bag[data$DeviceNumber %in% c(48,53,08,35,38)] <- "Vorne 2"
      data$Bag[data$DeviceNumber %in% c(60,55,39,28,52)] <- "Seite 1"
      data$Bag[data$DeviceNumber %in% c(10,27,20,56,04)] <- "Seite 2"
      data$Bag[data$DeviceNumber %in% c(01,16,15,17,31)] <- "Hinten 1"
      data$Bag[data$DeviceNumber %in% c(57,13,02,18,37)] <- "Hinten 2"
    }
    else if (grepl("2023-10-13_2.csv", file_path)) {
      data$File_Dat <- "10_13"
      data$Trial <- "2.1"
      data$DevicesOut <- 30
      data$Gateway <- "Gateway 2"
      data$Bag[data$DeviceNumber %in% c(13,06,03,12,42)] <- "Vorne 1"
      data$Bag[data$DeviceNumber %in% c(48,53,08,35,38)] <- "Vorne 2"
      data$Bag[data$DeviceNumber %in% c(60,55,39,28,52)] <- "Seite 1"
      data$Bag[data$DeviceNumber %in% c(10,27,20,56,04)] <- "Seite 2"
      data$Bag[data$DeviceNumber %in% c(01,16,15,17,31)] <- "Hinten 1"
      data$Bag[data$DeviceNumber %in% c(57,13,02,18,37)] <- "Hinten 2"
    }
    else if (grepl("2023-10-16_1.csv", file_path)) {
      data$File_Dat <- "10_16"
      data$Trial <- "2.1"
      data$DevicesOut <- 30
      data$Gateway <- "Gateway 1"
      data$Bag[data$DeviceNumber %in% c(46,34,29,44,14)] <- "Vorne 1"
      data$Bag[data$DeviceNumber %in% c(43,19,51,09,58)] <- "Vorne 2"
      data$Bag[data$DeviceNumber %in% c(40,36,54,45,41)] <- "Seite 1"
      data$Bag[data$DeviceNumber %in% c(59,05,25,47,22)] <- "Seite 2"
      data$Bag[data$DeviceNumber %in% c(50,21,33,30,07)] <- "Hinten 1"
      data$Bag[data$DeviceNumber %in% c(11,26,23,32,49)] <- "Hinten 2"
    }  
    else if (grepl("2023-10-16_2.csv", file_path)) {
      data$File_Dat <- "10_16"
      data$Trial <- "2.1"
      data$DevicesOut <- 30
      data$Gateway <- "Gateway 2"
      data$Bag[data$DeviceNumber %in% c(46,34,29,44,14)] <- "Vorne 1"
      data$Bag[data$DeviceNumber %in% c(43,19,51,09,58)] <- "Vorne 2"
      data$Bag[data$DeviceNumber %in% c(40,36,54,45,41)] <- "Seite 1"
      data$Bag[data$DeviceNumber %in% c(59,05,25,47,22)] <- "Seite 2"
      data$Bag[data$DeviceNumber %in% c(50,21,33,30,07)] <- "Hinten 1"
      data$Bag[data$DeviceNumber %in% c(11,26,23,32,49)] <- "Hinten 2"
    }
    else if (grepl("2023-10-17_1.csv", file_path)) {
      data$File_Dat <- "10_17"
      data$Trial <- "2.2"
      data$DevicesOut <- 15
      data$Gateway <- "Gateway 1"
      data$Bag[data$DeviceNumber %in% c(48,15,37,27,10)] <- "Vorne"
      data$Bag[data$DeviceNumber %in% c(02,60,20,24,08)] <- "Seite"
      data$Bag[data$DeviceNumber %in% c(16,17,13,39,53)] <- "Hinten"
    }
    else if (grepl("2023-10-18_1.csv", file_path)) {
      data$File_Dat <- "10_18"
      data$Trial <- "2.2"
      data$DevicesOut <- 15
      data$Gateway <- "Gateway 1"
      data$Bag[data$DeviceNumber %in% c(33,11,40,05,14)] <- "Vorne"
      data$Bag[data$DeviceNumber %in% c(07,23,41,34,19)] <- "Seite"
      data$Bag[data$DeviceNumber %in% c(43,44,25,22,30)] <- "Hinten"
    }
    else if (grepl("2023-10-18_2.csv", file_path)) {
      data$File_Dat <- "10_18"
      data$Trial <- "2.2"
      data$DevicesOut <- 15
      data$Gateway <- "Gateway 2"
      data$Bag[data$DeviceNumber %in% c(33,11,40,05,14)] <- "Vorne"
      data$Bag[data$DeviceNumber %in% c(07,23,41,34,19)] <- "Seite"
      data$Bag[data$DeviceNumber %in% c(43,44,25,22,30)] <- "Hinten"
    } 
    else if (grepl("2023-10-20_1.csv", file_path)) {
      data$File_Dat <- "10_20"
      data$Trial <- "2.2"
      data$DevicesOut <- 15
      data$Gateway <- "Gateway 1"
      data$Bag[data$DeviceNumber %in% c(28,31,06,01,42)] <- "Vorne"
      data$Bag[data$DeviceNumber %in% c(12,38,55,18,56)] <- "Seite"
      data$Bag[data$DeviceNumber %in% c(52,57,03,35,04)] <- "Hinten"
    }
    else if (grepl("2023-10-20_2.csv", file_path)) {
      data$File_Dat <- "10_20"
      data$Trial <- "2.2"
      data$DevicesOut <- 15
      data$Gateway <- "Gateway 2"
      data$Bag[data$DeviceNumber %in% c(28,31,06,01,42)] <- "Vorne"
      data$Bag[data$DeviceNumber %in% c(12,38,55,18,56)] <- "Seite"
      data$Bag[data$DeviceNumber %in% c(52,57,03,35,04)] <- "Hinten"
    }
    else if (grepl("2023-10-21_1.csv", file_path)) {
      data$File_Dat <- "10_21"
      data$Trial <- "2.2"
      data$DevicesOut <- 15
      data$Gateway <- "Gateway 1"
      data$Bag[data$DeviceNumber %in% c(54,26,32,45,09)] <- "Vorne"
      data$Bag[data$DeviceNumber %in% c(51,29,49,21,47)] <- "Seite"
      data$Bag[data$DeviceNumber %in% c(58,46,59,50,36)] <- "Hinten"
    }
    else if (grepl("2023-10-21_2.csv", file_path)) {
      data$File_Dat <- "10_21"
      data$Trial <- "2.2"
      data$DevicesOut <- 15
      data$Gateway <- "Gateway 2"
      data$Bag[data$DeviceNumber %in% c(54,26,32,45,09)] <- "Vorne"
      data$Bag[data$DeviceNumber %in% c(51,29,49,21,47)] <- "Seite"
      data$Bag[data$DeviceNumber %in% c(58,46,59,50,36)] <- "Hinten"
    }
    else if (grepl("2023-10-25_1.csv", file_path)) {
      data$File_Dat <- "10_25"
      data$Trial <- "2.2"
      data$DevicesOut <- 15
      data$Gateway <- "Gateway 1"
      data$Bag[data$DeviceNumber %in% c(33,03,31,81,55)] <- "Vorne"
      data$Bag[data$DeviceNumber %in% c(02,10,20,44,28)] <- "Seite"
      data$Bag[data$DeviceNumber %in% c(07,25,41,24,27)] <- "Hinten"
    }
    else if (grepl("2023-10-25_2.csv", file_path)) {
      data$File_Dat <- "10_25"
      data$Trial <- "2.2"
      data$DevicesOut <- 15
      data$Gateway <- "Gateway 2"
      data$Bag[data$DeviceNumber %in% c(33,03,31,81,55)] <- "Vorne"
      data$Bag[data$DeviceNumber %in% c(02,10,20,44,28)] <- "Seite"
      data$Bag[data$DeviceNumber %in% c(07,25,41,24,27)] <- "Hinten"
    }
    else if (grepl("2023-10-26_1.csv", file_path)) {
      data$File_Dat <- "10_26"
      data$Trial <- "2.1"
      data$DevicesOut <- 30
      data$Gateway <- "Gateway 1"
      data$Bag[data$DeviceNumber %in% c(08,34,22,38,13)] <- "Vorne 1"
      data$Bag[data$DeviceNumber %in% c(11,43,35,19,01)] <- "Vorne 2"
      data$Bag[data$DeviceNumber %in% c(17,48,60,23,53)] <- "Seite 1"
      data$Bag[data$DeviceNumber %in% c(06,52,40,12,16)] <- "Seite 2"
      data$Bag[data$DeviceNumber %in% c(42,05,04,57,47)] <- "Hinten 1"
      data$Bag[data$DeviceNumber %in% c(56,39,14,30,15)] <- "Hinten 2"
    }
    else if (grepl("2023-10-26_2.csv", file_path)) {
      data$File_Dat <- "10_26"
      data$Trial <- "2.1"
      data$DevicesOut <- 30
      data$Gateway <- "Gateway 2"
      data$Bag[data$DeviceNumber %in% c(08,34,22,38,13)] <- "Vorne 1"
      data$Bag[data$DeviceNumber %in% c(11,43,35,19,01)] <- "Vorne 2"
      data$Bag[data$DeviceNumber %in% c(17,48,60,23,53)] <- "Seite 1"
      data$Bag[data$DeviceNumber %in% c(06,52,40,12,16)] <- "Seite 2"
      data$Bag[data$DeviceNumber %in% c(42,05,04,57,47)] <- "Hinten 1"
      data$Bag[data$DeviceNumber %in% c(56,39,14,30,15)] <- "Hinten 2"
    }
    else if (grepl("2023-10-27_1.csv", file_path)) {
      data$File_Dat <- "10_27"
      data$Trial <- "2.31"
      data$DevicesOut <- 60
      data$Gateway <- "Gateway 1"
      data$Bag <- "Pritsche"
    }
    else if (grepl("2023-10-27_2.csv", file_path)) {
      data$File_Dat <- "10_27"
      data$Trial <- "2.31"
      data$DevicesOut <- 60
      data$Gateway <- "Gateway 2"
      data$Bag <- "Pritsche"
    }
      else if (grepl("2023-11-01_1.csv", file_path)) {
        data$File_Dat <- "11_01"
        data$Trial <- "2.32"
        data$DevicesOut <- 60
        data$Gateway <- "Gateway 1"
        data$Bag <- "Pritsche"
     }
      else if (grepl("2023-11-01_2.csv", file_path)) {
        data$File_Dat <- "11_01"
        data$Trial <- "2.32"
        data$DevicesOut <- 60
        data$Gateway <- "Gateway 2"
        data$Bag <- "Pritsche"
      }
        
    # Add more conditions for other CSV files as needed
    
    # Modify other columns
    
    return(data)
  }
  
  # Set the directory where your CSV files are located
  csv_dir <- "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/Stationary/CSV Data/Two Gates"
  
  # Initialize an empty list to store processed data frames
  data_list <- list()
  
  # Loop through CSV files in the directory
  csv_files <- dir(csv_dir, pattern = ".csv")
 for (file in csv_files) {
    file_path <- file.path(csv_dir, file)
    processed_data <- process_csv(file_path, device_mapping)
    data_list[[file]] <- processed_data
  }
  
 # csv_files_to_process <- c(
  #  "GNSS_tracker_data_2023-10-16_1.csv",
  #  "GNSS_tracker_data_2023-10-16_2.csv"
  #  "GNSS_tracker_data_2023-08-28.csv"
  #)
  
  #for (file in csv_files_to_process) {
   # file_path <- file.path(csv_dir, file)
   # processed_data <- process_csv(file_path, device_mapping)
   # data_list[[file]] <- processed_data
  #}
  
  # Combine all data frames into one
  final_data <- bind_rows(data_list)
  
  # View the final_data
  head(final_data)
  
  #final_data$Date_Time <- as.POSIXct(final_data$, format="%Y-%m-%d %H:%M:%S")
  final_data$Date_Time <- final_data$`Date and time`
  final_data$Lat <- final_data$`Latitude [°]`
  final_data$Lon <- final_data$`Longitude [°]`
  final_data$V <- final_data$`Battery Voltage[V]`
  final_data$Temp <- final_data$`Temperature [°C]`
  final_data$Hum <- final_data$`Humidity [%]`
  final_data[final_data == 0] <- NA
  final_data$Lat[final_data$Lat < 0.001] <- NA
  final_data$Lon[final_data$Lon < 0.001] <- NA
}

final_data$Date_Time <- as.POSIXct(final_data$Date_Time, format="%Y-%m-%d %H:%M:%S")

data_select_two <- subset(final_data, !is.na(Bag), select = c(Date_Time, DeviceNumber, Lat, Lon, V, File_Dat, Bag, Trial, DevicesOut, Gateway, RSSI, SNR, Temp, Hum))


# View the data_select
#head(data_select)


# Define a time window (5 seconds in this case)
time_window <- 15  # 5 seconds


# Function to remove duplicates with different Gateway values
remove_duplicates <- function(data_select_two) {
  data_select_two %>%
    group_by(DeviceNumber) %>%
    mutate(
      is_duplicate = c(FALSE, diff(Date_Time) <= time_window),
      is_duplicate_different_gateway = n_distinct(Gateway) > 1
    ) %>%
    filter(!(is_duplicate & is_duplicate_different_gateway)) %>%
    ungroup() %>%
    select(-is_duplicate, -is_duplicate_different_gateway)
}
 
# Apply the function to remove duplicates
data_select_two_no_duplicates <- remove_duplicates(data_select_two)

# Count how many duplicates were removed
duplicates_erased <- nrow(data_select_two) - nrow(data_select_two_no_duplicates)
cat("Number of duplicates erased:", duplicates_erased, "\n")

write_csv(data_select_two, file = "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/GatewayCSV/Two_Gateways.csv")

