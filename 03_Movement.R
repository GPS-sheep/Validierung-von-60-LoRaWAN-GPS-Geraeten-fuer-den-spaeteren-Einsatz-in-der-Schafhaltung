# movement data analysis

rm(list=ls())

#libraries
library('tidyverse')
library('lubridate')
library('ggplot2')

setwd("~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/")
TrackerD_data_1 <- read_csv("~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/Recorded Data/Movement Data/23.11.20/GNSS_tracker_data_2023-11-20_1.csv")
TrackerD_data_2 <- read_csv("~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/Recorded Data/Movement Data/23.11.20/GNSS_tracker_data_2023-11-20_2.csv")
data <- bind_rows(TrackerD_data_1, TrackerD_data_2)

ref_data <- read_csv("~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/Recorded Data/Movement Data/Bei Uwe laufen.csv")
# Basic data treatment
data$`Date and time` <- as.POSIXct(data$`Date and time`, format="%Y-%m-%d %H:%M:%S")
data$Lat <- data$`Latitude [°]`
data$Lon <- data$`Longitude [°]`
data$V <- data$`Battery Voltage[V]`
data$Temp <- data$`Temperature [°C]`
data$Hum <- data$`Humidity [%]`
data[data == 0] <- NA
data <- subset(data, select = c('Date and time', 'Device Address', 'Lat', 'Lon', 'V', 'RSSI', 'SNR', 'Flag', 'Hum', 'Temp'))

# summary of basic data input
{
  summary_df <- data %>% 
    arrange(`Device Address`, `Date and time`) %>% 
    group_by(`Device Address`) %>% 
    mutate(
      TimeDifference = difftime(lead(`Date and time`), `Date and time`, units = "hours"),
      TimeDifference = ifelse(is.na(TimeDifference), 0, as.numeric(TimeDifference))
    ) %>% 
    summarize(
      N = n(),
      NAs = sum(is.na(Lat) | is.na(Lon)),
      Total_hours = sum(max(TimeDifference)),
      Total_minutes = sum(TimeDifference * 60)
    ) %>% 
    ungroup()
}

{# remove NAs and duplicates
  # first remove NAs then duplicates
  #create a copy of data created as thedatacopy
  
  thedatacopy <- data
  data <- na.omit(thedatacopy)
  
  # remove duplicates
  # GPS devices are set to retrieve data all 20s removing duplicates for a time window in 5s removes duplicates
  
  
  # create a timewindow to remove duplicated recordings
  time_window <- 3 #5 second time window
  
  
  # create a function to fin duplicates in set time window
  find_duplicates <- function(data) {
    data %>%
      group_by(`Device Address`) %>%
      mutate(
        time_diff = c(0, diff(`Date and time`)),
        is_duplicate = time_diff <= time_window
      ) %>%
      filter(!is_duplicate) %>%
      ungroup() %>%
      select(-time_diff, -is_duplicate)
  }
  data_clean <- find_duplicates(data)
  
  # count the removed duplicates
  duplicates_erased <- nrow(data) - nrow(data_clean)
  cat("Number of duplicates erased:", duplicates_erased, "\n") # show erased duplicates
}

device <- c('0185EDE3')
data_true <- subset(data, data$"Device Address" == "0185EDE3")
p <- ggplot() +
  geom_point(data = data_true, aes(x = Lon, y = Lat), color = "red", size = 0.5) +
  geom_point(data = ref_data, aes(x = Longitude, y = Latitude), color = "black", size = 2) +
  theme_minimal()
  
print(p)

ggsave(filename = "Bewegung_ein_Gerät.png", plot = p, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/Plots/03_Movement", height=10, width=16, bg = "white", units="cm")
