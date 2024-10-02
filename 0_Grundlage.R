# The statistical analysis of the transferrates and basic functions of the GPS Devices

library('lubridate')
library('tidyverse')
library('multcompView')
library('writexl')
library('ggplot2')
library('hms')


rm(list=ls()) # remove everything out of the workspace

{
  setwd("~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/")
  data_one <- read_csv("~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/Gateway CSV/One_Gateway.csv")
  data_two <- read_csv("~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/Gateway CSV/Two_Gateways.csv")
  data <- bind_rows(data_one, data_two)
}

setwd("~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data")

# combine Gateway 1 and 2 to Two Gateways as Gates where it is spelled and as 1 and 2 for N_Gate
# Gateway NULL = One Gateway
{
  data <- data %>%
  mutate(N_Gate = case_when(
    grepl("Gateway 1", Gateway) | grepl("Gateway 2", Gateway) ~ "2",
    grepl("Gateway NULL", Gateway) ~ "1",
    TRUE ~ Gateway
  ))
data <- data %>%
  mutate(Gates = case_when(
    grepl("Gateway 1", Gateway) | grepl("Gateway 2", Gateway) ~ "Two Gateways",
    grepl("Gateway NULL", Gateway) ~ "One Gateway",
    TRUE ~ Gateway
  ))
data <- data %>% 
  mutate(Pos = case_when(
    grepl("Vorne 1", Bag) | grepl("Vorne 2", Bag) | grepl("Vorne", Bag) ~ "50m",
    grepl("Seite 1", Bag) | grepl("Seite 2", Bag) | grepl("Seite", Bag) ~ "90m",
    grepl("Hinten 1", Bag) | grepl("Hinten 2", Bag) | grepl("Hinten", Bag) ~ "115m",
    grepl("Pritsche", Bag)  ~ "36m"
  ))
}
data <- data %>% 
  filter(Bag != "Fensterbank")

#!!!!SKIP!!!!
### Summary of the data - skip if already done one time
{
# to summarize and retrieve n, write the column(s) that are to be counted. 
# Counted n for: Gateway, Device - Day(File_Dat), Device - Gateway, 
# to include the correct counting of the total hours each device was active
# calculate the total hours for each Device for each Day

# create a dataframe with the total hours and minutes of each device per each date
{
summary_df <- data %>% 
  arrange(DeviceNumber, File_Dat, Date_Time) %>% 
  group_by(DeviceNumber, File_Dat, Bag, Trial, DevicesOut, Gateway, Gates, Pos) %>% 
  mutate(
    TimeDifference = difftime(lead(Date_Time), Date_Time, units = "hours"),
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

# write the xlsx and csv files for each variable to count Ns and NAs and the active hours

write_summary_files <- function(variable_names, custom_suffix) {
  variable_summary <- summary_df %>%
     group_by(across(all_of(variable_names), .names = "{.col}")) %>%
      summarize(
       Total_N = sum(N),
        Total_NAs = sum(NAs),
        Active_hours = sum(Total_hours),
       Active_minutes = sum(Active_hours * 60)
      )
  
custom_names <- paste0(paste(variable_names, collapse = "_"), custom_suffix)
  write_excel_csv(variable_summary, paste0(custom_names, "_N.csv"))
  write_xlsx(variable_summary, paste0(custom_names, "_N.xlsx"))
  }

# Iterate over the variables for summarization
{
variables_to_summarize <- list(
  list(variable_names = c("DeviceNumber"), custom_suffix = ""),
  list(variable_names = c("DeviceNumber", "File_Dat"), custom_suffix = "Day"),
  list(variable_names = c("DeviceNumber", "Gateway"), custom_suffix = "Gate"),
  list(variable_names = c("DeviceNumber", "Gates"), custom_suffix = "Gates"),
  list(variable_names = c("DeviceNumber", "Trial"), custom_suffix = "Trial")
  )
}

for (combination in variables_to_summarize) {
  write_summary_files(combination$variable_names, combination$custom_suffix)
}

# for summaries that do not get grouped by Devices use the following code to only count sum the max hour of one device per File_Dat
write_summary_files <- function(variable_names, custom_suffix) {
  variable_summary <- summary_df %>%
    filter(!(Trial %in% c("2", "2.31", "2.32", "1.3", "0.1", "0.2"))) %>%  # Exclude specific trials
    group_by(across(all_of(variable_names), .names = "{.col}")) %>%
    summarise(
      Total_N = sum(N),
      Total_NAs = sum(NAs),
      Active_hours = max(Total_hours),  # Use max to get the highest total hours within each group
      Active_minutes = Active_hours * 60
    )
  
  
  
  custom_names <- paste0(paste(variable_names, collapse = "_"), custom_suffix)
  write.csv(variable_summary, paste0(custom_names, "_N_2.csv"))
  write_xlsx(variable_summary, paste0(custom_names, "_N_2.xlsx"))
}

{
  variables_to_summarize <- list(
    list(variable_names = c("Bag", "File_Dat"), custom_suffix = "Bag_Date"),
    list(variable_names = c("File_Dat", "DevicesOut"), custom_suffix = "Date"),
    list(variable_names = c("Gateway"), custom_suffix = "Gate"),
    list(variable_names = c("Gates"), custom_suffix = "Gates"),
    list(variable_names = c("Trial"), custom_suffix = "Trial"),
    list(variable_names = c("Pos"), custom_suffix = "Pos"),
    list(variable_names = c("Trial", "Pos"), custom_suffix = "Trial_Pos")
  )
}

for (combination in variables_to_summarize) {
  write_summary_files(combination$variable_names, combination$custom_suffix)
}

}

# Continue here
# Cleaning the Data
{# remove NAs and duplicates
# first remove NAs then duplicates
#create a copy of data created as thedatacopy

thedatacopy <- data
data <- na.omit(thedatacopy)

NA_erased <- nrow(thedatacopy) - nrow(data)
cat('NA entfernt:', NA_erased, "\n")

# remove duplicates
# GPS devices are set to retrieve data all 20s removing duplicates for a time window in 5s removes duplicates


# create a timewindow to remove duplicated recordings
time_window <- 3 #5 second time window


# create a function to fin duplicates in set time window
find_duplicates <- function(data) {
  data %>%
    group_by(DeviceNumber) %>%
    mutate(
      time_diff = c(0, diff(Date_Time)),
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

# use data_clean from here out on 

# look at data over time
# create subset of data to manipulate
# excluding Fensterbank tests
# create subsets of data that are to be classed (V and t)

{
classed_df <- data_clean %>%
  arrange(DeviceNumber, File_Dat, Date_Time) %>%
  group_by(DeviceNumber, File_Dat) %>%
  mutate(
    TimeWindow5 = cut(
      x = Date_Time,
      breaks = seq(from = min(Date_Time), to = max(Date_Time) + 300, by = 300),
      labels = seq(0, max(as.numeric(Date_Time)) - min(as.numeric(Date_Time)), by = 300),
      include.lowest = TRUE
    ),
    TimeClass5min = as.numeric(as.factor(TimeWindow5))  # Divide by 60 to convert seconds to minutes
  ) %>%
  mutate(
    TimeWindow30 = cut(
      x = Date_Time,
      breaks = seq(from = min(Date_Time), to = max(Date_Time) + 1800, by = 1800),
      labels = seq(0, max(as.numeric(Date_Time)) - min(as.numeric(Date_Time)), by = 1800),
      include.lowest = TRUE
    ),
    TimeClass30min = as.numeric(as.factor(TimeWindow30))  # Divide by 60 to convert seconds to minutes
  ) %>%
  mutate(
    VoltageClass = ceiling((4.1 - V) / 0.1),
    TempClass = cut(Temp, breaks = seq(4.2, max(Temp) + 2, by = 2), labels = FALSE),
    HumClass = cut(Hum, breaks = seq(21.7, max(Hum) + 2, by = 2), labels = FALSE)
  ) %>%
    group_by(DeviceNumber, File_Dat, TimeClass5min, TimeClass30min, VoltageClass, TempClass, HumClass) %>%
  reframe(
    Date_Time = Date_Time,
    Lat = Lat,
    Lon = Lon,
    Hum = Hum,
    Temp = Temp,
    RSSI = RSSI,
    SNR = SNR,
    V = V,
    Trial = Trial,
    Bag = Bag,
    Pos = Pos,
    Gates = Gates,
    N_Gate = N_Gate,
    Gateway = Gateway,
    DevicesOut = DevicesOut
  ) %>%
  ungroup()


exclude <- c(1.3, 2.31, 2.32)

}

# add column TIME where only the Time is displayed and add column active s where the active seconds of each Device is displayed
{
# add active_s
classed_df <- classed_df  %>% 
  arrange(File_Dat, DeviceNumber, Date_Time) %>% 
  group_by(File_Dat, DeviceNumber) %>% 
  mutate(first_time = first(Date_Time),             # Get the first observation time within the group
         active_s = as.numeric(difftime(Date_Time, first_time, units = "secs"))) %>%  # Calculate time difference
  ungroup() %>%
  select(-first_time)

# add Time
classed_df <- classed_df %>% 
  mutate(Time = as_hms(format(Date_Time, "%H:%M:%S")))
}

# Create Active Hours
Times_Devices <- classed_df %>% 
  group_by(DeviceNumber, File_Dat, Trial) %>% 
  summarise(First_Rec = min(Date_Time),
            Last_Rec = max(Date_Time),
            Active_Hour = difftime(max(Date_Time), min(Date_Time), units= "hours")) %>% 
  ungroup()
head(Times_Devices)
#write_csv(Times_Devices, file = "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/Active_Times.csv")
#write_xlsx(Times_Devices, path = "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/Active_Times.xlsx")

# cumulative hours of all Devices per Trial
Trial_Times <- Times_Devices %>% 
  group_by(Trial) %>% 
  summarise(Cum_h = sum(Active_Hour))


# create graphs
# folder it saves them in
setwd("~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/00_Grundlagen/Plots")

classed_df <- classed_df %>% filter(!Trial %in% exclude)

# Factor for plots
classed_df$Trial <- factor(classed_df$Trial, levels = c('1.1', '1.2', '2.1', '2.2'))
classed_df$Pos <- factor(classed_df$Pos, levels = c('50m', '90m', '115m'))


# Empfang (RSSI) über V, nach Versuchen
{
plot_RSSI_V <- ggplot(classed_df %>% filter(!Trial %in% exclude), aes(x = VoltageClass, y = RSSI, color = factor(Trial))) +
    stat_smooth(method = "gam", formula = y ~ s(x, k = 5), se = FALSE) +
  labs(x = "Spannungsklassen der Batterie", y = "RSSI", color = "Versuch") +
    theme_minimal()
print(plot_RSSI_V)
ggsave(filename = "Abb22_RSSI_V_lines.png", plot=plot_RSSI_V, 
       path = 
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/00_Grundlagen/Plots/1.3_Abhaengigkeiten",
       height=10, width=16, bg = "white", units="cm")
}

# Empfang (RSSI) über V, getrennt Versuche und Positionen
{
plot_RSSI_V_II <- ggplot(classed_df %>% filter(!Trial %in% exclude), aes(x = VoltageClass, y = RSSI, color = factor(Pos))) +
    stat_smooth(method = "gam", formula = y ~ s(x, k = 5), se = FALSE) +
    facet_grid(Trial ~ .) +
    labs(x = "Spannungsklassen der Batterie", y = "RSSI", color = "Position") +
    theme_minimal() +
    theme(
      strip.background = element_rect(
        color="black", size=1.5, linetype="solid"
      )
    )
print(plot_RSSI_V_II)
ggsave(filename = "Abb23_RSSI_V_VersuchPosition.png", plot=plot_RSSI_V_II, 
       path = 
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/00_Grundlagen/Plots/1.3_Abhaengigkeiten",
       height=10, width=16, bg = "white", units="cm")
}

# Empfang (RSSI) über V, nach Entferung (alle Versuche kombiniert)
{
plot_RSSI_V_III <- ggplot(classed_df %>% filter(!Trial %in% exclude), aes(x = VoltageClass, y = RSSI, color = factor(Pos))) +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 5), se = FALSE) +
  labs(x = "Spannungsklassen der Batterie", y = "RSSI", color = "Position") +
    theme_minimal()
  
print(plot_RSSI_V_III)
ggsave(filename = "Abb24_RSSI_V_Position.png", plot=plot_RSSI_V_III,
       path = 
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/00_Grundlagen/Plots/1.3_Abhaengigkeiten",
       
              height=10, width=16, bg = "white", units="cm")
}

subsub <- classed_df %>%
  filter(Trial == 2.1, Pos == "115m")
plot_Time <- ggplot(subsub, aes(TimeClass5min, V)) +
  geom_point(color="purple") +
  stat_smooth(data = subset(subsub,
                            TimeClass5min < 350),
              color="green")
print(plot_Time)

# Versuch und RSSI
{plot_RSSI_Trial <- ggplot(classed_df %>% filter(!Trial %in% exclude), aes(x = as.factor(Trial), y = RSSI, color = factor(Pos))) +
  geom_boxplot() +
  labs(x = "Versuch", y = "RSSI", color = "Position") +
    theme_minimal()
print(plot_RSSI_Trial)
ggsave(filename = "Abb12_RSSI_zu_Versuchen.png", plot = plot_RSSI_Trial,
       path =
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/00_Grundlagen/Plots/1.3_Abhaengigkeiten",
       height=10, width=16, bg = "white", units="cm")
}

{plot_RSSI_Pos <- ggplot(classed_df %>% filter(!Trial %in% exclude), aes(x = as.factor(Pos), y = RSSI, color = factor(Trial))) +
  geom_boxplot() +
  labs(x = "Position", y = "RSSI", color = "Versuch")+
    theme_minimal()
print(plot_RSSI_Pos)
ggsave(filename = "Abb13_RSSI_Pos.png", plot = plot_RSSI_Pos,
       path =
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/00_Grundlagen/Plots/1.3_Abhaengigkeiten",
       height=10, width=16, bg = "white", units="cm")
}

# Zeitverlauf der Versuche, V Beobachtung
{
# Liniendiagram Spannungsverlauf zu Zeitklassen
  # 5 min
plot_timeV <- ggplot(classed_df %>% filter(!Trial %in% exclude), aes(x = TimeClass5min, y = V, color = factor(Pos))) +
  geom_smooth() +
  facet_grid(Trial ~ .) +
  labs(x= "Zeitklasse [5min]", y= "Voltage", color= "Position") +
    theme_minimal()  +
    theme(
      strip.background = element_rect(
        color="black", size=1.5, linetype="solid"
      )
    )
print(plot_timeV)
ggsave(filename = "Abb14_SpannungzuZeitPositionenVersuche_5min.png", plot = plot_timeV,
       path = 
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/00_Grundlagen/Plots/1.3_Abhaengigkeiten",
       height=10, width=16, bg = "white", units="cm")
  # 30 min
plot_timeV_II <- ggplot(classed_df %>% filter(!Trial %in% exclude),
                        aes(x = TimeClass30min, y = V, color = factor(Pos))) +
  geom_smooth() +
  facet_grid(Trial ~ .) +
  labs(x= "Zeitklasse [30min]", y= "Voltage", color= "Position") +
  theme_minimal() +
  theme(
    strip.background = element_rect(
      color="black", size=1.5, linetype="solid"
    )
  )
print(plot_timeV_II)
ggsave(filename = "Abb15_SpannungZeitPosition_30min.png", plot = plot_timeV_II,
       path = 
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/00_Grundlagen/Plots/1.3_Abhaengigkeiten",
       height=10, width=16, bg = "white", units="cm")

# bis Minute 350
plot_timeV_III <- ggplot(classed_df %>% filter(!Trial %in% exclude),
                         aes(x = TimeClass5min, y = V, color = factor(Pos))) +
  geom_smooth(data = subset(classed_df,
                              TimeClass5min < 350)) +
  facet_grid(Trial ~ .) +
  labs(x= "Zeitklasse [5min]", y= "Voltage", color= "Position") +
  theme_minimal() +
  theme(
    strip.background = element_rect(
      color="black", size=1.5, linetype="solid"
    )
  )
print(plot_timeV_III)
ggsave(filename = "Abb16_SpannungZeitPosition_unter350min.png", plot = plot_timeV_III,
       path = 
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/00_Grundlagen/Plots/1.3_Abhaengigkeiten",
       height=10, width=16, bg = "white", units="cm")

# Punkt Diagram V über Zeit ab 400 min
plot_time_IX <- ggplot(classed_df %>% filter(!Trial %in% exclude), aes(x = TimeClass5min, y = V, color = factor(Pos))) +
  geom_point(data = subset(classed_df,
                           TimeClass5min > 350)) +
  facet_grid(Trial ~ .) +
  labs(x= "Zeitklasse [5min]", y= "Voltage", color= "Position") +
  theme_minimal() +
  theme(
    strip.background = element_rect(
      color="black", size=1.5, linetype="solid"
    )
  )
print(plot_time_IX)
ggsave(filename = "Abb17_SpannungZeitPositionPunkte_ab350_5min.png", plot = plot_time_IX,
         path = 
           "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/00_Grundlagen/Plots/1.3_Abhaengigkeiten",
         height=10, width=16, bg = "white", units="cm")

plot_time_X <- ggplot(classed_df %>% filter(!Trial %in% exclude), aes(x = TimeClass5min, y = RSSI, color = factor(Pos))) +
  geom_point(data = subset(classed_df,
                           TimeClass5min > 350)) +
  facet_grid(Trial ~ .) +
  labs(x= "Zeitklasse [5min]", y= "RSSI", color= "Position")
print(plot_time_X)
}

# V Boxplots
{
counted_V <- classed_df %>%
  group_by(VoltageClass, DeviceNumber, File_Dat, Trial) %>%
  summarize(Count = n())

# Create boxplot
plot_VtoN <- ggplot(counted_V, aes(x = as.factor(VoltageClass), y = Count)) +
  geom_boxplot() +
  labs(x = "Spannungsklassen", y = "Anzahl der Aufnahmen (n)") +
  theme_minimal()
plot_VtoN
ggsave(filename = "Abb11_Anzahl_Aufnahmen_Spannungsklassen_Boxplot.png", plot = plot_VtoN, height=10, width=16, bg = "white", units="cm")
}



# Alle Devices über die Versuche
{
# Boxplots für jedes Device pro Zeitklasse (30min) 
counted_t <- classed_df %>% 
  group_by(TimeClass30min, DeviceNumber, File_Dat, Trial, Pos) %>% 
  summarise(Count = n())

unique_Devices <- unique(counted_t$DeviceNumber) # create Value with all Unique Deviceumbers

plot_lists <- list() # create a list to store individual plots

max_count <- max(counted_t$Count)

for(device in unique_Devices) {
  sub_counted_t <- counted_t %>% filter(DeviceNumber == device)
  
plot_t_to_n <- ggplot(sub_counted_t, aes(x = as.factor(TimeClass30min), y = Count)) +
  geom_boxplot() +
  labs(x = "TimeClass30min", y = "Count [n]") +
  ggtitle(paste("Boxplot for Gerät", device)) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, max_count))

plot_lists[[as.character(device)]] <- plot_t_to_n  
}

for (device in unique_Devices) {
  current_plot <- plot_lists[[as.character(device)]]
  #print(current_plot)
  #ggsave(
  #  file.path("Devices N to t", paste0("Device_", device, "_Count_t.png")),
  #  plot = current_plot,
  #  height = 20,
  #  width = 40,
  #  bg = "white",
  #  units = "cm",
  #  device = "png"
  #)
 }

}

# For Trials instead of timeclasses
{
counted_t <- classed_df %>% 
  group_by(TimeClass30min, DeviceNumber, File_Dat, Trial) %>% 
  summarise(Count = n())

unique_Devices <- unique(counted_t$DeviceNumber) # create Value with all Unique Deviceumbers

plot_lists <- list() # create a list to store individual plots

max_count <- max(counted_t$Count)

for(device in unique_Devices) {
  sub_counted_t <- counted_t %>% filter(DeviceNumber == device)
  
  plot_t_to_n <- ggplot(sub_counted_t, aes(x = as.factor(Trial), y = Count)) +
    geom_boxplot() +
    labs(x = "Trial", y = "Count [n]") +
    ggtitle(paste("Boxplot for Gerät", device, "& 30min Zeitklassifizierug")) +
    theme_minimal() +
    coord_cartesian(ylim = c(0, max_count))
  
  plot_lists[[as.character(device)]] <- plot_t_to_n  
}

for (device in unique_Devices) {
  current_plot <- plot_lists[[as.character(device)]]
  print(current_plot)
  # ggsave(
  #  file.path("Devices n to Trial", paste0("Device_", device, "_Count_Trial.png")),
  #  plot = current_plot,
  #  height = 20,
  #  width = 40,
  #  bg = "white",
  #  units = "cm",
  #  device = "png"
  #)
}
}


# Versuch und Positions Vergleich zu n
{
counted_Trial <- classed_df %>% 
  group_by(DeviceNumber, File_Dat, Trial, Pos) %>% 
  summarise(Count = n())
counted_Trial <- counted_Trial %>% 
  filter(!Pos == "36m")

plot_TrialPos_n <- ggplot(counted_Trial, aes(x = Pos, y = Count)) +
  geom_boxplot() +
  facet_grid(Trial ~ .) +
  labs(x='Position', y='Anzahl der Aufnahmen (n)') + 
  theme_minimal()
  print(plot_TrialPos_n)
#ggsave(filename = "Abb9_Gesamte_Anzahl_Aufnahmen_zu_Poisitionen_Boxplot.png", plot=plot_TrialPos_n, height=10, width=16, bg = "white", units="cm")

plot_PosTrial_n <- ggplot(counted_Trial, aes(x = as.factor(Trial), y = Count)) +
  geom_boxplot() +
  facet_grid(Pos ~ .) +
  labs(x='Versuch', y='Anzahl der Aufnahmen (n)') + 
  theme_minimal()
print(plot_PosTrial_n)
#ggsave(filename = "Abb10_Gesamte_Anzahl_Aufnahmen_zu_Versuchen_Boxplot.png", plot=plot_PosTrial_n, height=10, width=16, bg = "white", units="cm")
}

# Temperatur zu Zeit und Übertragung...
{# Calculate dynamic mean of temperature for every 60 seconds
subsetted <- subset(classed_df, Trial == "2.2", select = c(File_Dat, Date_Time, Temp))

df_temp <- subsetted %>%
  group_by(File_Dat, Time_Min = cut(Date_Time, breaks = "1 min")) %>%
  summarize(Temperature_Mean = mean(Temp, na.rm = TRUE), 
            .groups = "drop")

# Calculate average number of observations per minute
df_n <- subsetted %>%
  group_by(File_Dat, Time_Min = cut(Date_Time, breaks = "1 min")) %>%
  summarize(Average_n = n() / 60,  # Divide by 60 to get per minute
            .groups = "drop")

# Merge the two dataframes on File_Dat and Time_Min
df_merged <- merge(df_temp, df_n, by = c("File_Dat", "Time_Min"), all.x = TRUE)

# Plot the data
plot_temp_t_n <- ggplot(df_merged, aes(x = Time_Min, y = Temperature_Mean, group = File_Dat, color = File_Dat)) +
  geom_line() +
  geom_line(aes(y = Average_n * max(Temperature_Mean) / max(Average_n)), linetype = "dashed", color = "blue") +
  scale_y_continuous(name = "Temperature [°C]", sec.axis = sec_axis(~ . * max(df_merged$Average_n) / max(df_merged$Temperature_Mean), name = "Durschnittliche Übertragung pro Minute")) +
  labs(x = "Uhrzeit") +
  theme_minimal() 
print(plot_temp_t_n)

ggsave(filename = "Abb25_Temperatur_recorded_n.png", plot=plot_temp_t_n,
       path =
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/00_Grundlagen/Plots/1.3_Abhaengigkeiten",
       height=10, width=16, bg = "white", units="cm")
}

# Versuch Vergleich, Zeitklassen
# ???

#Time and active seconds
classed_df$Time <- as.POSIXct(classed_df$Time, format = "%H:%M%:%S")

setwd("~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data")

#write_csv(classed_df, file = "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/00_Data.csv")
