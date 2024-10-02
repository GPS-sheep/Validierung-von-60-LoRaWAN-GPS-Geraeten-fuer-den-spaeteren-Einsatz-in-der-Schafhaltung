# Analyze the accuracy and precision of the GPS Devices


rm(list=ls()) # remove everything out of the workspace

library(dplyr)
library(ggplot2)
library(lubridate)
library(writexl)

setwd("/Users/sven-olebuhrmann/Documents/01_Uni/Master_Kiel/00_Master Arbeit/03_R") # set the working dorectory - file location


# load in the reference points recorded by the Emlid Device, named reference
# Referenz datapoints

#### Transferrates ####

# load in csv
# Clean = no outliers after IQR
# calced = standard dataframe with outliers

{
data_clean <- read_csv("/Users/sven-olebuhrmann/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Data_accuracy.csv")
data_calced <- read_csv("/Users/sven-olebuhrmann/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Data_w_dist.csv")
}
# create factors to have ordered display in plots
{
data_calced$Pos <- factor(data_calced$Pos, levels = c("50m", "90m", "115m"))
data_calced$Trial <- factor(data_calced$Trial, levels = c("1.1", "1.2", "2.1", "2.2"))
data_clean$Pos <- factor(data_clean$Pos, levels = c("50m", "90m", "115m"))
data_clean$Trial <- factor(data_clean$Trial, levels = c("1.1", "1.2", "2.1", "2.2"))
}

# standardize the data that only data from hour 1 till the 20th hour is looked at
# With outliers
{
data_std <- data_calced %>% 
  group_by(File_Dat) %>% 
  filter(Date_Time >= (min(Date_Time) + hours(1)) & Date_Time <= (min(Date_Time) + hours(20)))  %>% 
  ungroup()
}
# W/o outliers
{
data_std_clean <- data_clean %>% 
  group_by(File_Dat) %>% 
  filter(Date_Time >= (min(Date_Time) + hours(1)) & Date_Time <= (min(Date_Time) + hours(20)))  %>% 
  ungroup()
}

# write csv for standarized df
setwd("~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data")

#write_csv(data_std_clean, file = "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/02_Data_std.csv")

# Boxplots for the Trials

### Create subsets and boxplots for transferrates in comparison of parameters of
### interest

#### Observatios per minute for Trials ####
### With Outliers

# Count the number of observations per minute for each GPS device and trial
# With outliers
# create df with n per min and n per h, start and end time divided by observations
{
sum_Trial_with <- data_std %>%
    group_by(DeviceNumber, Trial) %>%
    summarize(
      "N" = n(),              # Count all observations
      "Start_Time" = min(Date_Time), #Start time and date
      "End_Time" = max(Date_Time), # Finds the end time and date
      "Total_Hours" = as.integer(difftime(End_Time, Start_Time, units = "hours")),    # Calculate total hours
      "Total_Minutes" = as.integer(difftime(End_Time, Start_Time, units = "mins")),    # Calculate total minutes
      "Observations_hourly" = N / Total_Hours, # Calculate observations per hour
      "Observations" = N / Total_Minutes# Calculate observations per minute
    )
  }

# Create the boxplot
Trial_permin_with <- ggplot(sum_Trial_with, aes(x = as.factor(Trial),
                                                     y = Observations)) +
  geom_boxplot() +
  labs(x = "Versuch",
       y = "Übertragungsrate [n/min]") +
  theme_minimal()
print(Trial_permin_with)
ggsave(filename = "Abb30_Versuch_Transferrate_mitAus.png",
       plot = Trial_permin_with,
       path =
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/02_Transferrate/Plots",
       height=10, width=16, bg = "white", units="cm")

# Without outliers
# create df with n per min and n per h, start and end time divided by observations
{
  sum_Trial_clean <- data_std_clean %>%
    group_by(DeviceNumber, Trial) %>%
    summarize(
      "N" = n(),              # Count all observations
      "Start_Time" = min(Date_Time), #Start time and date
      "End_Time" = max(Date_Time), # Finds the end time and date
      "Total_Hours" = as.integer(difftime(End_Time, Start_Time, units = "hours")),    # Calculate total hours
      "Total_Minutes" = as.integer(difftime(End_Time, Start_Time, units = "mins")),    # Calculate total minutes
      "Observations_hourly" = N / Total_Hours, # Calculate observations per hour
      "Observations" = N / Total_Minutes# Calculate observations per minute
    )
}

# Create the boxplot
Trial_permin_clean <- ggplot(sum_Trial_clean, aes(x = as.factor(Trial),
                                                     y = Observations)) +
  geom_boxplot() +
  labs(x = "Versuch",
       y = "Übertragungsrate [n/min]") +
  theme_minimal()
print(Trial_permin_clean)
ggsave(filename = "Abb31_Versuch_Transferrate_clean.png",
       plot = Trial_permin_clean,
       path =
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/02_Transferrate/Plots",
       height=10, width=16, bg = "white", units="cm")


#### Observatios per minute for GNSS Systeme ####
### With Outliers
# Count the number of observations per minute for each GPS device and trial
# With outliers
# create df with n per min and n per h, start and end time divided by observations
{
GNSS_n_p_min_with <- data_std %>%
  group_by(DeviceNumber, Trial, Gateway) %>%
  summarize(
      "N" = n(),              # Count all observations
      "Start_Time" = min(Date_Time), #Start time and date
      "End_Time" = max(Date_Time), # Finds the end time and date
      "Total_Hours" = as.integer(difftime(End_Time, Start_Time, units = "hours")),    # Calculate total hours
      "Total_Minutes" = as.integer(difftime(End_Time, Start_Time, units = "mins")),    # Calculate total minutes
      "Observations_hourly" = N / Total_Hours, # Calculate observations per hour
      "Observations" = N / Total_Minutes# Calculate observations per minute
    )
}

# Create the boxplot
GNSS_permin_with <- ggplot(GNSS_n_p_min_with, aes(x = as.factor(Gateway),
                                                     y = Observations)) +
  geom_boxplot() +
  labs(x = "GNSS-System",
       y = "Übertragungsrate [n/min]") +
  theme_minimal()
print(GNSS_permin_with)
ggsave(filename = "Abb32_GNSS_Transferrate_mitAus.png",
       plot = GNSS_permin_with,
       path =
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/02_Transferrate/Plots",
       height=10, width=16, bg = "white", units="cm")

# Without outliers
GNSS_n_p_min_clean <- data_std_clean %>%
  group_by(DeviceNumber, Trial, Gateway) %>%
  summarize(
    "N" = n(),              # Count all observations
    "Start_Time" = min(Date_Time), #Start time and date
    "End_Time" = max(Date_Time), # Finds the end time and date
    "Total_Hours" = as.integer(difftime(End_Time, Start_Time, units = "hours")),    # Calculate total hours
    "Total_Minutes" = as.integer(difftime(End_Time, Start_Time, units = "mins")),    # Calculate total minutes
    "Observations_hourly" = N / Total_Hours, # Calculate observations per hour
    "Observations" = N / Total_Minutes# Calculate observations per minute
  )

# Create the boxplot
GNSS_permin_clean <- ggplot(GNSS_n_p_min_clean, aes(x = as.factor(Gateway),
                                                       y = Observations)) +
  geom_boxplot() +
  labs(x = "GNSS-System",
       y = "Übertragungsrate [n/min]") +
  theme_minimal()
print(GNSS_permin_clean)
ggsave(filename = "Abb33_GNSS_Transferrate_clean.png",
       plot = GNSS_permin_clean,
       path =
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/02_Transferrate/Plots",
       height=10, width=16, bg = "white", units="cm")



#### Observatios per minute for Temp Class ####
### With Outliers

# Count the number of observations per minute for each GPS device and trial
# With outliers
temp_n_p_min_with <- data_std %>%
  group_by(DeviceNumber, Trial, TempClass) %>%
  summarize(
    "N" = n(),              # Count all observations
    "Start_Time" = min(Date_Time), #Start time and date
    "End_Time" = max(Date_Time), # Finds the end time and date
    "Total_Hours" = as.integer(difftime(End_Time, Start_Time, units = "hours")),    # Calculate total hours
    "Total_Minutes" = as.integer(difftime(End_Time, Start_Time, units = "mins")),    # Calculate total minutes
    "Observations_hourly" = N / Total_Hours, # Calculate observations per hour
    "Observations" = N / Total_Minutes# Calculate observations per minute
  )

# Create the boxplot
temp_permin_with <- ggplot(temp_n_p_min_with, aes(x = as.factor(TempClass),
                                                  y = Observations)) +
  geom_boxplot() +
  labs(x = "Temperatur-Klassen",
       y = "Übertragungsrate [n/min]") +
  theme_minimal()
print(temp_permin_with)
ggsave(filename = "Abb34_Temp_Transferrate_mitAus.png",
       plot = temp_permin_with,
       path =
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/02_Transferrate/Plots",
       height=10, width=16, bg = "white", units="cm")

# Without outliers
temp_n_p_min_clean <- data_std_clean %>%
  group_by(DeviceNumber, Trial, TempClass) %>%
  summarize(
    "N" = n(),              # Count all observations
    "Start_Time" = min(Date_Time), #Start time and date
    "End_Time" = max(Date_Time), # Finds the end time and date
    "Total_Hours" = as.integer(difftime(End_Time, Start_Time, units = "hours")),    # Calculate total hours
    "Total_Minutes" = as.integer(difftime(End_Time, Start_Time, units = "mins")),    # Calculate total minutes
    "Observations_hourly" = N / Total_Hours, # Calculate observations per hour
    "Observations" = N / Total_Minutes# Calculate observations per minute
  )

# Create the boxplot
temp_permin_clean <- ggplot(temp_n_p_min_clean, aes(x = as.factor(TempClass),
                                                    y = Observations)) +
  geom_boxplot() +
  labs(x = "Temperatur-Klassen",
       y = "Übertragungsrate [n/min]") +
  theme_minimal()
print(temp_permin_clean)
ggsave(filename = "Abb35_temp_Transferrate_clean.png",
       plot = temp_permin_clean,
       path =
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/02_Transferrate/Plots",
       height=10, width=16, bg = "white", units="cm")

#### Observatios per minute for V Class ####
### With Outliers

# Count the number of observations per minute for each GPS device and trial
# With outliers
V_n_p_min_with <- data_std %>%
  group_by(DeviceNumber, Trial, VoltageClass) %>%
  summarize(
    "N" = n(),              # Count all observations
    "Start_Time" = min(Date_Time), #Start time and date
    "End_Time" = max(Date_Time), # Finds the end time and date
    "Total_Hours" = as.integer(difftime(End_Time, Start_Time, units = "hours")),    # Calculate total hours
    "Total_Minutes" = as.integer(difftime(End_Time, Start_Time, units = "mins")),    # Calculate total minutes
    "Observations_hourly" = N / Total_Hours, # Calculate observations per hour
    "Observations" = N / Total_Minutes# Calculate observations per minute
  )

# Create the boxplot
V_permin_with <- ggplot(V_n_p_min_with, aes(x = as.factor(VoltageClass),
                                          y = Observations)) +
  geom_boxplot() +
  labs(x = "Spannungsklassen",
       y = "Übertragungsrate [n/min]") +
  theme_minimal()
print(V_permin_with)
ggsave(filename = "Abb36_V_Transferrate_mitAus.png",
       plot = V_permin_with,
       path =
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/02_Transferrate/Plots",
       height=10, width=16, bg = "white", units="cm")

# Without outliers
V_n_p_min_clean <- data_std_clean %>%
  group_by(DeviceNumber, Trial, VoltageClass) %>%
  summarize(
    "N" = n(),              # Count all observations
    "Start_Time" = min(Date_Time), #Start time and date
    "End_Time" = max(Date_Time), # Finds the end time and date
    "Total_Hours" = as.integer(difftime(End_Time, Start_Time, units = "hours")),    # Calculate total hours
    "Total_Minutes" = as.integer(difftime(End_Time, Start_Time, units = "mins")),    # Calculate total minutes
    "Observations_hourly" = N / Total_Hours, # Calculate observations per hour
    "Observations" = N / Total_Minutes# Calculate observations per minute
  )


# Create the boxplot
V_permin_clean <- ggplot(V_n_p_min_clean, aes(x = as.factor(VoltageClass),
                                            y = Observations)) +
  geom_boxplot() +
  labs(x = "Spannungsklassen",
       y = "Übertragungsrate [n/min]") +
  theme_minimal()
print(V_permin_clean)
ggsave(filename = "Abb37_V_Transferrate_clean.png",
       plot = V_permin_clean,
       path =
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/02_Transferrate/Plots",
       height=10, width=16, bg = "white", units="cm")


#### Observatios per minute for Positionen ####
### With Outliers

# Count the number of observations per minute for each GPS device and trial
# With outliers
Pos_n_p_min_with <- data_std %>%
  group_by(DeviceNumber, Trial, Pos) %>%
  summarize(
    "N" = n(),              # Count all observations
    "Start_Time" = min(Date_Time), #Start time and date
    "End_Time" = max(Date_Time), # Finds the end time and date
    "Total_Hours" = as.integer(difftime(End_Time, Start_Time, units = "hours")),    # Calculate total hours
    "Total_Minutes" = as.integer(difftime(End_Time, Start_Time, units = "mins")),    # Calculate total minutes
    "Observations_hourly" = N / Total_Hours, # Calculate observations per hour
    "Observations" = N / Total_Minutes# Calculate observations per minute
  )


# Create the boxplot
Pos_permin_with <- ggplot(Pos_n_p_min_with, aes(x = as.factor(Pos),
                                          y = Observations)) +
  geom_boxplot() +
  labs(x = "Position",
       y = "Übertragungsrate [n/min]") +
  theme_minimal()
print(Pos_permin_with)
ggsave(filename = "Abb38_Pos_Transferrate_mitAus.png",
       plot = Pos_permin_with,
       path =
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/02_Transferrate/Plots",
       height=10, width=16, bg = "white", units="cm")

# Without outliers
Pos_n_p_min_clean <- data_std_clean %>%
  group_by(DeviceNumber, Trial, Pos) %>%
  summarize(
    "N" = n(),              # Count all observations
    "Start_Time" = min(Date_Time), #Start time and date
    "End_Time" = max(Date_Time), # Finds the end time and date
    "Total_Hours" = as.integer(difftime(End_Time, Start_Time, units = "hours")),    # Calculate total hours
    "Total_Minutes" = as.integer(difftime(End_Time, Start_Time, units = "mins")),    # Calculate total minutes
    "Observations_hourly" = N / Total_Hours, # Calculate observations per hour
    "Observations" = N / Total_Minutes# Calculate observations per minute
  )


# Create the boxplot
Pos_permin_clean <- ggplot(Pos_n_p_min_clean, aes(x = as.factor(Pos),
                                            y = Observations)) +
  geom_boxplot() +
  labs(x = "Position",
       y = "Übertragungsrate [n/min]") +
  theme_minimal()
print(Pos_permin_clean)
ggsave(filename = "Abb39_Pos_Transferrate_clean.png",
       plot = Pos_permin_clean,
       path =
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/02_Transferrate/Plots",
       height=10, width=16, bg = "white", units="cm")


# Create the boxplot incl Trials
Pos_permin_clean <- ggplot(Pos_n_p_min_clean, aes(x = as.factor(Pos),
                                                  y = Observations)) +
  geom_boxplot() +
  labs(x = "Position",
       y = "Übertragungsrate [n/min]") +
  facet_wrap(~Trial) +
  theme_minimal()
print(Pos_permin_clean)
ggsave(filename = "Abb39_Pos_Transferrate_clean.png",
       plot = Pos_permin_clean,
       path =
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/02_Transferrate/Plots",
       height=10, width=16, bg = "white", units="cm")






#### excluded cause wrong, keeping it for future reference if true ####
# Create a new column for date and minute part of Date_Time
data_std <- data_std %>%
  mutate(Date_Minute = floor_date(Date_Time, unit = "minute"))
data_std_clean <- data_std_clean %>%
  mutate(Date_Minute = floor_date(Date_Time, unit = "minute"))

Trials_n_p_min_with <- data_std %>%
  group_by(Trial, DeviceNumber, Date_Minute) %>%
  summarise(Observations = n()) %>%
  ungroup()


#### Observatios per minute for Distanzen ####
### With Outliers

# Count the number of observations per minute for each GPS device and trial
# With outliers
dist_n_p_min_with <- data_std %>%
  group_by(DeviceNumber, Trial, m_class) %>%
  summarize(
    "N" = n(),              # Count all observations
    "Start_Time" = min(Date_Time), #Start time and date
    "End_Time" = max(Date_Time), # Finds the end time and date
    "Total_Hours" = as.integer(difftime(End_Time, Start_Time, units = "hours")),    # Calculate total hours
    "Total_Minutes" = as.integer(difftime(End_Time, Start_Time, units = "mins")),    # Calculate total minutes
    "Observations_hourly" = N / Total_Hours, # Calculate observations per hour
    "Observations" = N / Total_Minutes# Calculate observations per minute
  )


# Create the boxplot
dist_permin_with <- ggplot(dist_n_p_min_with, aes(x = as.factor(m_class),
                                                y = Observations)) +
  geom_boxplot() +
  labs(x = "Entfernung zum Referenzpunkt [m, gerundet]",
       y = "Übertragungsrate [n/min]") +
  theme_minimal()
print(dist_permin_with)
ggsave(filename = "Abb40_dist_Transferrate_mitAus.png",
       plot = Pos_permin_with,
       path =
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/02_Transferrate/Plots",
       height=10, width=16, bg = "white", units="cm")

# Without outliers
dist_n_p_min_clean <- data_std_clean %>%
  group_by(DeviceNumber, Trial, m_class) %>%
  summarize(
    "N" = n(),              # Count all observations
    "Start_Time" = min(Date_Time), #Start time and date
    "End_Time" = max(Date_Time), # Finds the end time and date
    "Total_Hours" = as.integer(difftime(End_Time, Start_Time, units = "hours")),    # Calculate total hours
    "Total_Minutes" = as.integer(difftime(End_Time, Start_Time, units = "mins")),    # Calculate total minutes
    "Observations_hourly" = N / Total_Hours, # Calculate observations per hour
    "Observations" = N / Total_Minutes# Calculate observations per minute
  )


# Create the boxplot
dist_permin_clean <- ggplot(dist_n_p_min_clean, aes(x = as.factor(m_class),
                                                  y = Observations)) +
  geom_boxplot() +
  labs(x = "Entfernung zum Referenzpunkt [m, gerundet]",
       y = "Übertragungsrate [n/min]") +
  theme_minimal()
print(dist_permin_clean)
ggsave(filename = "Abb41_dist_Transferrate_clean.png",
       plot = Pos_permin_clean,
       path =
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/02_Transferrate/Plots",
       height=10, width=16, bg = "white", units="cm")






#### excluded cause wrong, keeping it for future reference if true ####
# Create a new column for date and minute part of Date_Time
data_std <- data_std %>%
  mutate(Date_Minute = floor_date(Date_Time, unit = "minute"))
data_std_clean <- data_std_clean %>%
  mutate(Date_Minute = floor_date(Date_Time, unit = "minute"))

Trials_n_p_min_with <- data_std %>%
  group_by(Trial, DeviceNumber, Date_Minute) %>%
  summarise(Observations = n()) %>%
  ungroup()

#### part of deskriptive Statistics ####
## mit ausreißer
{
  summary_df <- data_std %>% 
    arrange(DeviceNumber, Date_Time) %>% 
    group_by(DeviceNumber, Trial, Gateway, Pos) %>% 
    summarize(
      "N" = n(),              # Count all observations
      "Start_Time" = min(Date_Time), #Start time and date
      "End_Time" = max(Date_Time), # Finds the end time and date
      "Total_Hours" = as.integer(difftime(End_Time, Start_Time, units = "hours")),    # Calculate total hours
      "Total_Minutes" = as.integer(difftime(End_Time, Start_Time, units = "mins")),    # Calculate total minutes
      "Observations_hourly" = N / Total_Hours, # Calculate observations per hour
      "Observations" = N / Total_Minutes# Calculate observations per minute
    )
}

# for summaries that do not get grouped by Devices use the following code to only count sum the max hour of one device per File_Dat
setwd("~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/00_Grundlagen/Summary - mit Ausreißer")
write_summary_files <- function(variable_names, custom_suffix) {
  variable_summary <- summary_df %>%
    group_by(across(all_of(variable_names), .names = "{.col}")) %>%
    summarise(
      Total_N = sum(N),
      Total_h = sum(Total_Hours)
    )
  
  
  
  custom_names <- paste0(paste(variable_names, collapse = "_"), custom_suffix)
  write.csv(variable_summary, paste0(custom_names, "_N_2.csv"))
  write_xlsx(variable_summary, paste0(custom_names, "_N_2.xlsx"))
}

{
  variables_to_summarize <- list(
    list(variable_names = c("Trial"), custom_suffix = "Trial"),
    list(variable_names = c("Pos"), custom_suffix = "Pos"),
    list(variable_names = c("Trial", "Pos"), custom_suffix = "Trial_Pos"),
    list(variable_names = c("Gateway"), custom_suffix = "Gate")
  )
}

for (combination in variables_to_summarize) {
  write_summary_files(combination$variable_names, combination$custom_suffix)
}

 ## ohne ausreißer
{
  summary_df_clean <- data_std_clean %>% 
    arrange(DeviceNumber, Date_Time) %>% 
    group_by(DeviceNumber, Trial, Gateway, Pos) %>% 
    summarize(
      "N" = n(),              # Count all observations
      "Start_Time" = min(Date_Time), #Start time and date
      "End_Time" = max(Date_Time), # Finds the end time and date
      "Total_Hours" = as.integer(difftime(End_Time, Start_Time, units = "hours")),    # Calculate total hours
      "Total_Minutes" = as.integer(difftime(End_Time, Start_Time, units = "mins")),    # Calculate total minutes
      "Observations_hourly" = N / Total_Hours, # Calculate observations per hour
      "Observations" = N / Total_Minutes# Calculate observations per minute
    )
}

# for summaries that do not get grouped by Devices use the following code to only count sum the max hour of one device per File_Dat
setwd("~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/00_Grundlagen/Summary - ohne Ausreißer")
write_summary_files <- function(variable_names, custom_suffix) {
  variable_summary <- summary_df_clean %>%
    group_by(across(all_of(variable_names), .names = "{.col}")) %>%
    summarise(
      Total_N = sum(N),
      Total_h = sum(Total_Hours)
    )
  
  
  
  custom_names <- paste0(paste(variable_names, collapse = "_"), custom_suffix)
  write.csv(variable_summary, paste0(custom_names, "_N.csv"))
  write_xlsx(variable_summary, paste0(custom_names, "_N.xlsx"))
}

{
  variables_to_summarize <- list(
    list(variable_names = c("Trial"), custom_suffix = "Trial"),
    list(variable_names = c("Pos"), custom_suffix = "Pos"),
    list(variable_names = c("Trial", "Pos"), custom_suffix = "Trial_Pos"),
    list(variable_names = c("Gateway"), custom_suffix = "Gate")
  )
}

for (combination in variables_to_summarize) {
  write_summary_files(combination$variable_names, combination$custom_suffix)
}

