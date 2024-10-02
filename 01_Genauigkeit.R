# Analyze the accuracy and precision of the GPS Devices


rm(list=ls()) # remove everything out of the workspace

library(ggplot2)
library(tidyverse)
library(geosphere)
library(ggforce)
library('writexl')

setwd("/Users/sven-olebuhrmann/Documents/01_Uni/Master_Kiel/00_Master Arbeit/03_R") # set the working dorectory - file location


# load in the reference points recorded by the Emlid Device, named reference
# Referenz datapoints
{
referenz <- read_csv("/Users/sven-olebuhrmann/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/2023-08-28_EMLID REACH Daten.csv")

# remove all columns but names and Lat and Lon
df_ref <- referenz %>% 
  select(Position = Description, Longitude, Latitude)
df_ref <- na.omit(df_ref)
df_ref <- df_ref %>%
  mutate(Position = ifelse(Position == "uwe 1 (Vorne)", "Vorne", Position),
         Position = ifelse(Position == "uwe 2 (linke Seite)", "Seite", Position),
         Position = ifelse(Position == "uwe 3 (Hinten)", "Hinten", Position))
df_ref <- df_ref %>% 
  rename(Lat = Latitude,
         Lon = Longitude)
df_ref <- df_ref %>% 
  mutate(row_number = row_number())
df_ref <- df_ref %>%
  filter(!(row_number() %in% c(2, 4, 6)))
df_ref <- df_ref %>%
  select(-row_number)
}


# load data created before by R Session 00_Data
{
data <- read_csv("/Users/sven-olebuhrmann/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/00_Data.csv")

#create a value named Position, excluding 36m since this does not have a reference Point!
#data$Position <- ifelse(data$Pos == "50m", "Vorne",
#                        ifelse(data$Pos == "90m", "Seite",
#                              ifelse(data$Pos == "115m", "Hinten", NA)))

# create levels for R to distinguish the levels of distance/Trials and create proper plots, it is for later
data$Pos <- factor(data$Pos, levels = c("50m", "90m", "115m"))
data$Trial <- factor(data$Trial, levels = c("1.1", "1.2", "2.1", "2.2"))
}

# calculate and wirte distance of the reference point to the point taken by the EMLID
{#write the functio to calculate the distance
calculate_distance <- function(lat1, lon1, lat2, lon2) {
  dist <- distHaversine(c(lon1, lat1), c(lon2, lat2))
  return(dist)
} 

#create subsets for each Position
{
lost_sub <- data %>% 
    filter(!(Pos %in% c("50m", "90m", "115m")))
  
  
# Vorne
{sub_Vorne <- data %>% 
    filter(Pos == "50m")

df_ref_Vorne <- df_ref %>% 
    filter(Position == "Vorne")
}
# Seite
{sub_Seite <- data %>% 
    filter(Pos == "90m")
  df_ref_Seite <- df_ref %>% 
    filter(Position == "Seite")
}
# Hinten
{sub_Hinten <- data %>% 
    filter(Pos == "115m")
  df_ref_Hinten <- df_ref %>% 
    filter(Position == "Hinten")
}
}

#create reference points for each position
ref_point_Vorne <- c(df_ref_Vorne$Lon, df_ref_Vorne$Lat)
ref_point_Seite <- c(df_ref_Seite$Lon, df_ref_Seite$Lat)
ref_point_Hinten <- c(df_ref_Hinten$Lon, df_ref_Hinten$Lat)

# map and calculate each Subset through
sub_Vorne$dist <- mapply(calculate_distance, sub_Vorne$Lat, sub_Vorne$Lon, 
                         ref_point_Vorne[2], ref_point_Vorne[1])

sub_Seite$dist <- mapply(calculate_distance, sub_Seite$Lat, sub_Seite$Lon, 
                         ref_point_Seite[2], ref_point_Seite[1])

sub_Hinten$dist <- mapply(calculate_distance, sub_Hinten$Lat, sub_Hinten$Lon, 
                         ref_point_Hinten[2], ref_point_Hinten[1])

# create one dataframe again
data_calced <- bind_rows(sub_Hinten, sub_Seite, sub_Vorne)
}

#### Covariance-Coefficiet base ####
# Streuung als Boxplot für Versuche inkl. Variationskoeffizient
# Boxplots für Positionen in Versuche alle in einem Plot

# Kovarianzkoeffizient und Boxplots before IQR
{
# CV calculation
calculate_cv <- function(x) {
  mean_x <- mean(x)
  sd_x <- sd(x)
  cv <- (sd_x / mean_x)
  return(cv)
}

# create new dataframes with Variationscoefficient 
# Each Trial
# create a boxplot with CV for first assessment

cv_values_Trial <- data_calced %>% 
  group_by(Trial) %>% 
  summarize(CV = calculate_cv(dist))
cv_values_Trial$CV <- round(cv_values_Trial$CV, 2)

# each Trial
{cv_Trials_bp <- ggplot(data_calced, aes(x = as.factor(Trial), y = dist)) +
  geom_boxplot() +
  geom_text(data = cv_values_Trial, aes(label = CV, y = max(data_calced$dist) + 100), position = position_dodge(2), vjust = 0) + 
  labs(x = 'Versuch', y = 'Distanz zum Referenzpunkt [m]') +
  theme_minimal()
  print(cv_Trials_bp)
#ggsave(filename = "Abb14_Distanz_Versuch_cv_uncleaned.png", plot = cv_Trials_bp, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Genauigkeit/Plots", height=10, width=16, bg = "white", units="cm")
}
# each Position and each Trial
cv_values_Pos_Trial <- data_calced %>% 
  group_by(Pos, Trial) %>% 
  summarise(CV = calculate_cv(dist))

cv_values_Pos_Trial$CV <- round(cv_values_Pos_Trial$CV, 2)

Versuche <- c("1.1", "1.2", "2.1", "2.2")
names(Versuche) <- c("Versuch 1.1", "Versuch 1.2", "Versuch 2.1", "Versuch 2.2")

cv_Pos_Trials_bp <- ggplot(data_calced, aes(x = as.factor(Pos), y = dist)) +
  geom_boxplot() +
  facet_wrap(~Trial) +
  geom_text(data = cv_values_Pos_Trial, aes(label = CV, y = max(data_calced$dist) - 500), vjust = 0) + 
  labs(x = 'Position', y = 'Distanz zum Referenzpunkt [m]') +
  theme_minimal() +
  theme(
    strip.background = element_rect(
      color="black", size=1.5, linetype="solid"
    )
  )
print(cv_Pos_Trials_bp)

#ggsave(filename = "Abb15_BP_VersuchePosition_Distanz_cv_uncleaned.png", plot = cv_Pos_Trials_bp, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Genauigkeit/Plots/01_Genauigkeit", height=10, width=16, bg = "white", units="cm")
}

#### erase outliers for complete trial ####
# Ausreißer identifiziere und entfernen, entfernt ebenfalls die aufnahmen aus Versuch 1.3 und 2.3, da keine Position angegeben!
# create distance classes by 1m intervall
{
  # create distance classes of 1m
  {
    max_dist <- max(data_calced$dist)
    data_calced <-  data_calced %>% 
      mutate(m_class = cut(dist, breaks = c(0, seq(1, max_dist + 1, by = 1)), labels = FALSE))
  }
  
#create a copy of raw data
data_calced_copy <- data_calced

## extract all Trials and Positions as df

  
# Percentil for outliers
#upper_bound <- quantile(data_calced$dist, 0.975)
#upper_bound # 16.02.2023 current data says that outliers are with 72.12732 (m) distance to the reference point and above.

# Using IQR for Outliers
out_IQR <- boxplot.stats(data_calced$dist)$out
out_IQR_ind <-which(data_calced$dist %in% out_IQR)
data_calced[out_IQR_ind,]

# only IQR for upper IQR (as it is the only outlying data)
{
#iqr <- IQR(data_calced$dist)
#upper_IQR <- quantile(data_calced$dist, 0.75) + 1.5 * iqr
#out_upper_IQR <- data_calced$dist[data_calced$dist > upper_IQR]
#out_upper_IQR_ind <- which(data_calced$dist %in% out_upper_IQR)
#data_IQR <- data_calced[-out_upper_IQR_ind,]
#outlier_data <- data_calced[out_upper_IQR_ind,]
 }
 
# delete outliers in dataframe and create for each method a new dataframe
#data_97percentil <- data_calced %>% 
  #filter(dist <= upper_bound)
data_IQR <- data_calced[-out_IQR_ind,]
#outlier_data <- data_calced[-out_upper_IQR_ind ,]

# count the removed outliers
#outliers_erased_percentil <- nrow(data_calced_copy) - nrow(data_97percentil)
#cat("Number of outliers erased of 97.5 percentil:", outliers_erased_percentil, "\n") # show erased duplicates

outliers_erased_IQR <- nrow(data_calced_copy) - nrow(data_IQR)
cat("Number of outliers erased of IQR:", outliers_erased_IQR, "\n") # show erased duplicates

#outliers_erased_IQR_1 <- nrow(data_calced_copy) - nrow(outlier_data)
#cat("Number of outliers erased of IQR:", outliers_erased_IQR_1, "\n") # show erased duplicates
}

### IQR 1 and IQR are for comparison as one only used the upper percentil to erase data. shows data close to reference were not erased.
# percentil 97,5 were not sufficient enough in my opinion as IQR is stronger

# show how many data points were erased by Position and by Trial
# how many data points were removed by IQR by Trial and Position
# sub_Hinten/Seite/Vorne have all datapoints, created before IQR
# data_relevant for all data = N total
# outlier data for all data cut off by IQR = N cut by IQR
# data_IQR for all data in N clean and 'usable'

N_total_sum <- data_calced %>%
  group_by(Trial, Pos) %>% 
  summarize(
    N = n()
  ) %>%
  ungroup()

str(N_total_sum)

N_outlier <- data_calced[out_IQR_ind,] %>%
  group_by(Trial, Pos) %>%
  summarize(
    'N cut' = n()
  ) %>%
  ungroup()
str(N_outlier)

N_cleaned <- data_IQR %>%
  group_by(Trial, Pos) %>%
  summarize(
    N_clean = n()
  ) %>%
  ungroup()
N_summary <- left_join(N_total_sum, N_outlier, N_cleaned, by = c("Trial", "Pos"))
N_summary <- left_join(N_summary, N_cleaned, by = c("Trial", "Pos"))
N_summary <- N_summary %>%
  mutate(Percentage = (N_clean / N) * 100)

#write_excel_csv(N_summary, file = "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/Plots/01_Genauigkeit/N_summary.csv")
#write_xlsx(N_summary, path = "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/Plots/01_Genauigkeit/N_summary.xlsx")

# Streuung als Boxplot für Versuche inkl. Variationskoeffizient OHNE Ausreißer
# Boxplots für Positionen in Versuche alle in einem Plot

# Kovarianzkoeffizient und Boxplots
{
# create new dataframes with Variationscoefficient 
# Each Trial
# create a boxplot with CV for first assessment
cv_values_IQR <- data_IQR %>% 
  group_by(Trial) %>% 
  summarise(CV = calculate_cv(dist))

cv_values_IQR$CV <- round(cv_values_IQR$CV, 2)

  # each Trial
  cv_Trials_bp_IQR <- ggplot(data_IQR, aes(x = as.factor(Trial), y = dist)) +
    geom_boxplot() +
    geom_text(data = cv_values_IQR, aes(label = CV, y = max(data_IQR$dist) + 15)) + 
    labs(x = 'Versuch', y = 'Distanz zum Referenzpunkt [m]') +
    theme_minimal()
  print(cv_Trials_bp_IQR)
  
  #ggsave(filename = "Abb16_Distanz_Versuche_cv_cleaned.png", plot = cv_Trials_bp_IQR, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Genauigkeit/Plots", height=10, width=16, bg = "white", units="cm")

  #boxplot für percentil

    #cv_values_Trial_perc <- data_97percentil %>% 
    #group_by(Trial) %>% 
    #summarize(CV = calculate_cv(dist))
  #cv_values_Trial_perc$CV <- round(cv_values_Trial_perc$CV)
  
  # each Trial
  #cv_Trials_bp_perc <- ggplot(data_97percentil, aes(x = as.factor(Trial), y = dist)) +
    #geom_boxplot() +
    #geom_text(data = cv_values_Trial_perc, aes(label = CV, y = max(data_97percentil$dist) + 75), position = position_dodge(0.9), vjust = 0) + 
    #labs(title = "Boxplot mit CV von Versuchen ohne Ausreißer (Percentil)") +
    #ylab("Distanz zum Referenzpunkt") +
    #xlab("Versuch")
  #print(cv_Trials_bp_perc)
  
  #ggsave(filename = "BP_Trial_CV_perc.png", plot = cv_Trials_bp_perc, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/Plots/01_Genauigkeit", height=10, width=16, bg = "white", units="cm")
  
  # each Position and each Trial
  cv_values_Pos_Trial_IQR <- data_IQR %>% 
    group_by(Pos, Trial) %>% 
    summarise(CV = calculate_cv(dist))
  
  cv_values_Pos_Trial_IQR$CV <- round(cv_values_Pos_Trial_IQR$CV, 2)
  
  cv_Pos_Trials_bp_IQR <- ggplot(data_IQR, aes(x = as.factor(Pos), y = dist)) +
    geom_boxplot() +
    facet_wrap(~Trial) +
    geom_text(data = cv_values_Pos_Trial_IQR, aes(label = CV, y = max(data_IQR$dist)+35), nudge_y = -30) + 
    labs(x = 'Position', y = 'Distanz zum Referenzpunkt [m]') +
    theme_minimal() +
    theme(
      strip.background = element_rect(
        color="black", size=1.5, linetype="solid"
      )
    )
    print(cv_Pos_Trials_bp_IQR)
  
 # ggsave(filename = "Abb17_BP_VersuchePositionen_ohneAusreißer_cv.png", plot = cv_Pos_Trials_bp_IQR, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Genauigkeit/Plots", height=10, width=16, bg = "white", units="cm")

# Boxplots für Genauigkeit und SNR/RSSI, VClass
# SNR nichgt als Boxplot darstellbar

BP_dist_to_RSSI <- ggplot(data_IQR, aes(x = as.factor(m_class), y = RSSI)) +
  geom_boxplot() +
  labs(title = "Boxplot RSSI zu Abstandsklassen (1m) von Versuchen") +
  ylab("RSSI") +
  xlab("Abstand zum Referenzpunkt")
print(BP_dist_to_RSSI)
#ggsave(filename = "BP_RSSI zu Abstand.png", plot = BP_dist_to_RSSI, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/Plots/01_Genauigkeit", height=10, width=16, bg = "white", units="cm")


Pos_to_RSSI <- ggplot(data_IQR, aes(x = Pos, y = RSSI, fill = Trial)) +
  geom_boxplot() +
  labs(title = "Boxplot RSSI zu Positionen") +
  ylab("RSSI") +
  xlab("Posititionen")
print(Pos_to_RSSI)
#ggsave(filename = "BP_Position zu RSSI.png", plot = Pos_to_RSSI, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/Plots/01_Genauigkeit", height=10, width=16, bg = "white", units="cm")


dist_to_Vclass <- ggplot(data_IQR, aes(x = as.factor(VoltageClass), y = m_class, fill = Pos)) +
  geom_boxplot() +
  facet_wrap(~Trial) +
  ylab("Abstand zum Referenzpunt [m]") +
  xlab("Spannungsklassen") +
  theme_minimal() +
  theme(
    strip.background = element_rect(
      color="black", size=1.5, linetype="solid"
    )
  )
print(dist_to_Vclass)
ggsave(filename = "Abb28_SpannungzuZeitPositionenVersuche_5min.png", plot = dist_to_Vclass,
       path = 
         "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/00_Grundlagen/Plots/1.3_Abhaengigkeiten",
       height=10, width=16, bg = "white", units="cm")

}

# DRMS, CEP und CEP accuracy
{
DRMS <- function (Lon, Lat) {
  sdx <- sd(Lon)
  sdy <- sd(Lat)
  drms <- sqrt(sdx^2 + sdy^2)
  return(drms)
}
CEP <- function(Lon, Lat) {
  sdx <- sd(Lon)
  sdy <- sd(Lat)
  cep <- 0.62*sdy + 0.56*sdx
  return(cep)
}
CEP_accuracy <- function(Lon, Lat) {
  sdx <- sd(Lon)
  sdy <- sd(Lat)
  acc_cep <- sdy/sdx
  return(acc_cep)
}
}

# Mercator
# Add Mercator (x,y)
{
mercator_projection <- function(lat, lon) {
  R <- 6371 # Earth's radius in kilometers
  x <- R * lon * pi / 180
  y <- R * log(tan((90 + lat) * pi / 360))
  return(list(x = x, y = y))
}

cartesian_coords <- mercator_projection(data_IQR$Lat, data_IQR$Lon)
data_IQR$y <- cartesian_coords$y
data_IQR$x <- cartesian_coords$x


cartesian_coords_ref <- mercator_projection(df_ref$Lat, df_ref$Lon)

# Replace Lat and Lon columns with Cartesian coordinates
df_ref$y <- cartesian_coords_ref$y
df_ref$x <- cartesian_coords_ref$x

# Check if coordinates line up
{
Controlplot <- plot(df_ref$x, df_ref$y, col = "blue", 
     xlab = "X", ylab = "Y")

# Add coordinates in degrees
points(df_ref$Lon, df_ref$Lat, pch = 20)
points(df_ref$Lat, df_ref$Lon, pch = 20, col = "red")

# Add legend
legend("topright", legend = c("Cartesian Coordinates", "Degrees Coordinates"),
       col = c("blue", "red"), pch = c(1, 20))
} # currently does not print red dots, lined up before, does not work anymore... Yuck


}

accuracy_measures <- data_IQR %>%
  group_by(Trial, Pos) %>%
  summarize(
    DRMS = DRMS(x, y),
    CEP = CEP(x, y),
    CEP_accuracy = CEP_accuracy(x, y)
  )

# seperate Trials, positions
trials <- c("1.1", "1.2", "2.1", "2.2")
positions <- c("50m", "90m", "115m")
position_names <- c("Vorne", "Seite", "Hinten") 

# create plots by running through seperate positions and trials, creates distance plots and 
for (trial in trials) {
  for (pos_index in seq_along(positions)) {
    pos <- positions[pos_index]
    position_name <- position_names[pos_index]
    
    # Filter data for the current trial and position
    filtered_data <- subset(data_IQR, Trial == trial & Pos == pos)
    filtered_accuracy <- subset(accuracy_measures, Trial == trial & Pos == pos)
    filtered_ref <- subset(df_ref, Position == position_name)
    
    # Plot the data
    plot_title <- paste("GPS Data of Trial", trial, "at", pos, "with CEP and DRMS")
    p <- ggplot() +
      geom_point(data = filtered_data, aes(x = x, y = y), color = "red", size = 0.5) +
      geom_point(data = filtered_ref, aes(x = x, y = y), color = "black", size = 2) +
      geom_circle(data = filtered_ref, aes(x0 = x, y0 = y, r = filtered_accuracy$CEP), color = "green", fill = NA, size = 1) +
      geom_circle(data = filtered_ref, aes(x0 = x, y0 = y, r = filtered_accuracy$DRMS), color = "blue", fill = NA, size = 1) +
      labs(x = "x-Koordinate", y = "y-Koordinate") +
      theme_minimal()
    
    print(p)
    # Save the plot
    plot_filename <- paste("Trial", trial, "at", pos, ".png", sep = "_")
    ggsave(plot_filename, plot = p, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Genauigkeit/Plots/GPS CEP DMRS", height=10, width=16, bg = "white", units="cm")
  }
}


# complete plot for Trial 1.1
{alldone <- subset(data_IQR, Trial == "1.1")
alldone_p <- subset(accuracy_measures, Trial == "1.1")
front <- subset(alldone, Pos == "50m")
middle <- subset(alldone, Pos == "90m")
back <- subset(alldone, Pos == "115m")


colors <- c('50m' = 'antiquewhite4', '90m' = 'aquamarine4', '115m' = 'bisque4', 'Referenzpunkt' = 'black', 'CEP' = 'green', 'DRMS' = 'blue')

plotted_all <- ggplot(fill = 'colors') +
  geom_point(data = front, aes(x=x, y=y), color = 'antiquewhite4', size = 0.05) +
  geom_point(data = middle, aes(x=x, y=y), color = 'aquamarine4', size = 0.05) +
  geom_point(data = back, aes(x=x, y=y), color = 'burlywood4', size = 0.05) +
  geom_point(data = df_ref, aes(x=x, y=y), color = 'black', size = 1) +
  geom_circle(data = df_ref, aes(x0 = x, y0 = y, r = alldone_p$CEP), color = "green", fill = NA, size = 1) +
  geom_circle(data = df_ref, aes(x0 = x, y0 = y, r = alldone_p$DRMS), color = "blue", fill = NA, size = 1) +
  labs(x = "X",
       y = "Y") +
  scale_color_manual(breaks = c('CEP', 'DRMS'),
                     values = c('CEP' = 'green', 'DRMS' = 'blue')) +
  theme(legend.position = 'bottom')
print(plotted_all)

#ggsave(filename = 'Abb29ish_CEP_DRMS_Trial11.png', plot = plotted_all, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Genauigkeit/Plots", height=10, width=16, bg = "white", units="cm")
}

# complete plot for trial 1.2
{
  alldone <- subset(data_IQR, Trial == "1.2")
  alldone_p <- subset(accuracy_measures, Trial == "1.2")
  front <- subset(alldone, Pos == "50m")
  middle <- subset(alldone, Pos == "90m")
  back <- subset(alldone, Pos == "115m")
  
  
  colors <- c('50m' = 'antiquewhite4', '90m' = 'aquamarine4', '115m' = 'bisque4', 'Referenzpunkt' = 'black', 'CEP' = 'green', 'DRMS' = 'blue')
  
  plotted_all <- ggplot(fill = 'colors') +
    geom_point(data = front, aes(x=x, y=y), color = 'antiquewhite4', size = 0.05) +
    geom_point(data = middle, aes(x=x, y=y), color = 'aquamarine4', size = 0.05) +
    geom_point(data = back, aes(x=x, y=y), color = 'burlywood4', size = 0.05) +
    geom_point(data = df_ref, aes(x=x, y=y), color = 'black', size = 1) +
    geom_circle(data = df_ref, aes(x0 = x, y0 = y, r = alldone_p$CEP), color = "green", fill = NA, size = 1) +
    geom_circle(data = df_ref, aes(x0 = x, y0 = y, r = alldone_p$DRMS), color = "blue", fill = NA, size = 1) +
    labs(x = "X",
         y = "Y") +
    scale_color_manual(breaks = c('CEP', 'DRMS'),
                       values = c('CEP' = 'green', 'DRMS' = 'blue')) +
    theme(legend.position = 'bottom')
  print(plotted_all)
  
 #ggsave(filename = 'Abb29ish_CEP_DRMS_Trial12.png', plot = plotted_all, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Genauigkeit/Plots", height=10, width=16, bg = "white", units="cm")
}

# complete plot for trial 1.2
{
  alldone <- subset(data_IQR, Trial == "2.1")
  alldone_p <- subset(accuracy_measures, Trial == "2.1")
  front <- subset(alldone, Pos == "50m")
  middle <- subset(alldone, Pos == "90m")
  back <- subset(alldone, Pos == "115m")
  
  
  colors <- c('50m' = 'antiquewhite4', '90m' = 'aquamarine4', '115m' = 'bisque4', 'Referenzpunkt' = 'black', 'CEP' = 'green', 'DRMS' = 'blue')
  
  plotted_all <- ggplot(fill = 'colors') +
    geom_point(data = front, aes(x=x, y=y), color = 'antiquewhite4', size = 0.05) +
    geom_point(data = middle, aes(x=x, y=y), color = 'aquamarine4', size = 0.05) +
    geom_point(data = back, aes(x=x, y=y), color = 'burlywood4', size = 0.05) +
    geom_point(data = df_ref, aes(x=x, y=y), color = 'black', size = 1) +
    geom_circle(data = df_ref, aes(x0 = x, y0 = y, r = alldone_p$CEP), color = "green", fill = NA, size = 1) +
    geom_circle(data = df_ref, aes(x0 = x, y0 = y, r = alldone_p$DRMS), color = "blue", fill = NA, size = 1) +
    labs(x = "X",
         y = "Y") +
    scale_color_manual(breaks = c('CEP', 'DRMS'),
                       values = c('CEP' = 'green', 'DRMS' = 'blue')) +
    theme(legend.position = 'bottom')
  print(plotted_all)
  
 #ggsave(filename = 'Abb29ish_CEP_DRMS_Trial21.png', plot = plotted_all, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Genauigkeit/Plots", height=10, width=16, bg = "white", units="cm")
}

# complete Plot for Trial 2.2
{alldone <- subset(data_IQR, Trial == "2.2")
alldone_p <- subset(accuracy_measures, Trial == "2.2")
front <- subset(alldone, Pos == "50m")
middle <- subset(alldone, Pos == "90m")
back <- subset(alldone, Pos == "115m")


colors <- c('50m' = 'antiquewhite4', '90m' = 'aquamarine4', '115m' = 'bisque4', 'Referenzpunkt' = 'black', 'CEP' = 'green', 'DRMS' = 'blue')

plotted_all <- ggplot(fill = 'colors') +
  geom_point(data = front, aes(x=x, y=y), color = 'antiquewhite4', size = 0.05) +
  geom_point(data = middle, aes(x=x, y=y), color = 'aquamarine4', size = 0.05) +
  geom_point(data = back, aes(x=x, y=y), color = 'burlywood4', size = 0.05) +
  geom_point(data = df_ref, aes(x=x, y=y), color = 'black', size = 1) +
  geom_circle(data = df_ref, aes(x0 = x, y0 = y, r = alldone_p$CEP), color = "green", fill = NA, size = 1) +
  geom_circle(data = df_ref, aes(x0 = x, y0 = y, r = alldone_p$DRMS), color = "blue", fill = NA, size = 1) +
  labs(x = "X",
       y = "Y") +
  scale_color_manual(breaks = c('CEP', 'DRMS'),
                     values = c('CEP' = 'green', 'DRMS' = 'blue')) +
  theme(legend.position = 'bottom')
print(plotted_all)
#ggsave(filename = 'Abb29ish_CEP_DRMS_Trial22.png', plot = plotted_all, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Genauigkeit/Plots", height=10, width=16, bg = "white", units="cm")
}


#### erase outliers per each Trial, uses same names for dataframes as before!!! ####
# create distance classes by 1m intervall
  # create distance classes of 1m
  {
    max_dist <- max(data_calced$dist)
    data_calced <-  data_calced %>% 
      mutate(m_class = cut(dist, breaks = c(0, seq(1, max_dist + 1, by = 1)), labels = FALSE))
  }

# erase outliers
{
  #create a copy of raw data
  data_calced_copy <- data_calced
  
  # seperate all Trials into one subset
  ## Trial 1.1
  Trial_1.1 <- subset(data_calced_copy, Trial == "1.1")
  
  ## Trial 1.2
  Trial_1.2 <- subset(data_calced_copy, Trial == "1.2")
  
  ## Trial 2.1
  Trial_2.1 <- subset(data_calced_copy, Trial == "2.1")
  
  ## Trial 2.2
  Trial_2.2 <- subset(data_calced_copy, Trial == "2.2")
  
  # Using IQR for Outliers for each seperate dataframe
  # Trial 1.1
  {out_IQR_1.1 <- boxplot.stats(Trial_1.1$dist)$out
  out_IQR_ind_1.1 <-which(Trial_1.1$dist %in% out_IQR_1.1)
  Trial_1.1[out_IQR_ind_1.1,]
  
  data_1.1_IQR <- Trial_1.1[-out_IQR_ind_1.1,]
  
  outliers_erased_1.1 <- nrow(Trial_1.1) - nrow(data_1.1_IQR)
  cat("Number of outliers erased of IQR:", outliers_erased_1.1, "\n") # show erased duplicates
  }
  
  # Trial 1.2
  {
    out_IQR_1.2 <- boxplot.stats(Trial_1.2$dist)$out
  out_IQR_ind_1.2 <-which(Trial_1.2$dist %in% out_IQR_1.2)
  Trial_1.2[out_IQR_ind_1.2,]
  
  data_1.2_IQR <- Trial_1.2[-out_IQR_ind_1.2,]
  
  outliers_erased_1.2 <- nrow(Trial_1.2) - nrow(data_1.2_IQR)
  cat("Number of outliers erased of IQR:", outliers_erased_1.2, "\n") # show erased duplicates
  }
  
  # Trial 2.1
  {out_IQR_2.1 <- boxplot.stats(Trial_2.1$dist)$out
    out_IQR_ind_2.1 <-which(Trial_2.1$dist %in% out_IQR_2.1)
    Trial_2.1[out_IQR_ind_2.1,]
    
    data_2.1_IQR <- Trial_2.1[-out_IQR_ind_2.1,]
    
    outliers_erased_2.1 <- nrow(Trial_2.1) - nrow(data_2.1_IQR)
    cat("Number of outliers erased of IQR:", outliers_erased_2.1, "\n") # show erased duplicates
  }
  
  # Trial 2.2
  {
    out_IQR_2.2 <- boxplot.stats(Trial_2.2$dist)$out
    out_IQR_ind_2.2 <-which(Trial_2.2$dist %in% out_IQR_2.2)
    Trial_2.2[out_IQR_ind_2.2,]
    
    data_2.2_IQR <- Trial_2.2[-out_IQR_ind_2.2,]
    
    outliers_erased_2.2 <- nrow(Trial_2.2) - nrow(data_2.2_IQR)
    cat("Number of outliers erased of IQR:", outliers_erased_2.2, "\n") # show erased duplicates
  }
  
  data_IQR <- rbind(data_1.1_IQR, data_1.2_IQR, data_2.1_IQR, data_2.2_IQR)
}

# Streuung als Boxplot für Versuche inkl. Variationskoeffizient OHNE Ausreißer
# Boxplots für Positionen in Versuche alle in einem Plot

# Kovarianzkoeffizient und Boxplots
{
  # create new dataframes with Variationscoefficient 
  # Each Trial
  # create a boxplot with CV for first assessment
  cv_values_IQR <- data_IQR %>% 
    group_by(Trial) %>% 
    summarise(CV = calculate_cv(dist))
  
  cv_values_IQR$CV <- round(cv_values_IQR$CV, 2)
  
  # each Trial
  cv_Trials_bp_IQR <- ggplot(data_IQR, aes(x = as.factor(Trial), y = dist)) +
    geom_boxplot() +
    geom_text(data = cv_values_IQR, aes(label = CV, y = max(data_IQR$dist) + 15)) + 
    labs(x = 'Versuch', y = 'Distanz zum Referenzpunkt [m]') +
    theme_minimal()
  print(cv_Trials_bp_IQR)
  
  #ggsave(filename = "Abb16a_Distanz_Versuche_cv_cleaned.png", plot = cv_Trials_bp_IQR, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Genauigkeit/Plots", height=10, width=16, bg = "white", units="cm")
  
    # each Position and each Trial
  cv_values_Pos_Trial_IQR <- data_IQR %>% 
    group_by(Pos, Trial) %>% 
    summarise(CV = calculate_cv(dist))
  
  cv_values_Pos_Trial_IQR$CV <- round(cv_values_Pos_Trial_IQR$CV, 2)
  
  cv_Pos_Trials_bp_IQR <- ggplot(data_IQR, aes(x = as.factor(Pos), y = dist)) +
    geom_boxplot() +
    facet_wrap(~Trial) +
    geom_text(data = cv_values_Pos_Trial_IQR, aes(label = CV, y = max(data_IQR$dist)+35), nudge_y = -30) + 
    labs(x = 'Position', y = 'Distanz zum Referenzpunkt [m]') +
    theme_minimal() +
    theme(
      strip.background = element_rect(
        color="black", size=1.5, linetype="solid"
      )
    )
  print(cv_Pos_Trials_bp_IQR)
  
  #ggsave(filename = "Abb17a_BP_VersuchePositionen_ohneAusreißer_cv.png", plot = cv_Pos_Trials_bp_IQR, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Genauigkeit/Plots", height=10, width=16, bg = "white", units="cm")
  
  # Boxplots für Genauigkeit und SNR/RSSI, VClass
  # SNR nichgt als Boxplot darstellbar
  
  BP_dist_to_RSSI <- ggplot(data_IQR, aes(x = as.factor(m_class), y = RSSI)) +
    geom_boxplot() +
    labs(title = "Boxplot RSSI zu Abstandsklassen (1m) von Versuchen") +
    ylab("RSSI") +
    xlab("Abstand zum Referenzpunkt")
  print(BP_dist_to_RSSI)
  #ggsave(filename = "BP_RSSI zu Abstand.png", plot = BP_dist_to_RSSI, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/Plots/01_Genauigkeit", height=10, width=16, bg = "white", units="cm")
  
  
  Pos_to_RSSI <- ggplot(data_IQR, aes(x = Pos, y = RSSI, fill = Trial)) +
    geom_boxplot() +
    labs(title = "Boxplot RSSI zu Positionen") +
    ylab("RSSI") +
    xlab("Posititionen")
  print(Pos_to_RSSI)
  #ggsave(filename = "BP_Position zu RSSI.png", plot = Pos_to_RSSI, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/Plots/01_Genauigkeit", height=10, width=16, bg = "white", units="cm")
  
  
  dist_to_Vclass <- ggplot(data_IQR, aes(x = as.factor(VoltageClass), y = m_class, fill = Pos)) +
    geom_boxplot() +
    facet_wrap(~Trial) +
    ylab("Abstand zum Referenzpunt [m]") +
    xlab("Spannungsklassen") +
    theme_minimal() +
    theme(
      strip.background = element_rect(
        color="black", size=1.5, linetype="solid"
      )
    )
  print(dist_to_Vclass)
  #ggsave(filename = "Abb28a_SpannungzuZeitPositionenVersuche_5min.png", plot = dist_to_Vclass,
   #      path = 
    #       "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/00_Grundlagen/Plots/1.3_Abhaengigkeiten",
     #    height=10, width=16, bg = "white", units="cm")
  
  }

# DRMS, CEP und CEP accuracy
{
  DRMS <- function (Lon, Lat) {
    sdx <- sd(Lon)
    sdy <- sd(Lat)
    drms <- sqrt(sdx^2 + sdy^2)
    return(drms)
  }
  CEP <- function(Lon, Lat) {
    sdx <- sd(Lon)
    sdy <- sd(Lat)
    cep <- 0.62*sdy + 0.56*sdx
    return(cep)
  }
  CEP_accuracy <- function(Lon, Lat) {
    sdx <- sd(Lon)
    sdy <- sd(Lat)
    acc_cep <- sdy/sdx
    return(acc_cep)
  }
}


# Mercator
# Add Mercator (x,y)
{
  mercator_projection <- function(lat, lon) {
    R <- 6371 # Earth's radius in kilometers
    x <- R * lon * pi / 180
    y <- R * log(tan((90 + lat) * pi / 360))
    return(list(x = x, y = y))
  }
  
  cartesian_coords <- mercator_projection(data_IQR$Lat, data_IQR$Lon)
  data_IQR$y <- cartesian_coords$y
  data_IQR$x <- cartesian_coords$x
  
  
  cartesian_coords_ref <- mercator_projection(df_ref$Lat, df_ref$Lon)
  
  # Replace Lat and Lon columns with Cartesian coordinates
  df_ref$y <- cartesian_coords_ref$y
  df_ref$x <- cartesian_coords_ref$x
  
  # Check if coordinates line up
  {
    Controlplot <- plot(df_ref$x, df_ref$y, col = "blue", 
                        xlab = "X", ylab = "Y")
    
    # Add coordinates in degrees
    points(df_ref$Lon, df_ref$Lat, pch = 20)
    points(df_ref$Lat, df_ref$Lon, pch = 20, col = "red")
    
    # Add legend
    legend("topright", legend = c("Cartesian Coordinates", "Degrees Coordinates"),
           col = c("blue", "red"), pch = c(1, 20))
  } # currently does not print red dots, lined up before, does not work anymore... Yuck
  
  
}

accuracy_measures <- data_IQR %>%
  group_by(Trial, Pos) %>%
  summarize(
    DRMS = DRMS(x, y),
    CEP = CEP(x, y),
    CEP_accuracy = CEP_accuracy(x, y)
  )

# seperate Trials, positions
trials <- c("1.1", "1.2", "2.1", "2.2")
positions <- c("50m", "90m", "115m")
position_names <- c("Vorne", "Seite", "Hinten") 

# create plots by running through seperate positions and trials, creates distance plots and 
for (trial in trials) {
  for (pos_index in seq_along(positions)) {
    pos <- positions[pos_index]
    position_name <- position_names[pos_index]
    
    # Filter data for the current trial and position
    filtered_data <- subset(data_IQR, Trial == trial & Pos == pos)
    filtered_accuracy <- subset(accuracy_measures, Trial == trial & Pos == pos)
    filtered_ref <- subset(df_ref, Position == position_name)
    
    # Plot the data
    plot_title <- paste("GPS Data of Trial", trial, "at", pos, "with CEP and DRMS")
    p <- ggplot() +
      geom_point(data = filtered_data, aes(x = x, y = y), color = "red", size = 0.5) +
      geom_point(data = filtered_ref, aes(x = x, y = y), color = "black", size = 2) +
      geom_circle(data = filtered_ref, aes(x0 = x, y0 = y, r = filtered_accuracy$CEP), color = "green", fill = NA, size = 1) +
      geom_circle(data = filtered_ref, aes(x0 = x, y0 = y, r = filtered_accuracy$DRMS), color = "blue", fill = NA, size = 1) +
      labs(x = "x-Koordinate", y = "y-Koordinate") +
      theme_minimal()
    
    print(p)
    # Save the plot
    plot_filename <- paste("Trial", trial, "at", pos, "a.png", sep = "_")
   #ggsave(plot_filename, plot = p, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Genauigkeit/Plots/GPS CEP DMRS", height=10, width=16, bg = "white", units="cm")
  }
}


# complete plot for Trial 1.1
{alldone <- subset(data_IQR, Trial == "1.1")
  alldone_p <- subset(accuracy_measures, Trial == "1.1")
  front <- subset(alldone, Pos == "50m")
  middle <- subset(alldone, Pos == "90m")
  back <- subset(alldone, Pos == "115m")
  
  
  colors <- c('50m' = 'antiquewhite4', '90m' = 'aquamarine4', '115m' = 'bisque4', 'Referenzpunkt' = 'black', 'CEP' = 'green', 'DRMS' = 'blue')
  
  plotted_all <- ggplot(fill = 'colors') +
    geom_point(data = front, aes(x=x, y=y), color = 'antiquewhite4', size = 0.05) +
    geom_point(data = middle, aes(x=x, y=y), color = 'aquamarine4', size = 0.05) +
    geom_point(data = back, aes(x=x, y=y), color = 'burlywood4', size = 0.05) +
    geom_point(data = df_ref, aes(x=x, y=y), color = 'black', size = 1) +
    geom_circle(data = df_ref, aes(x0 = x, y0 = y, r = alldone_p$CEP), color = "green", fill = NA, size = 1) +
    geom_circle(data = df_ref, aes(x0 = x, y0 = y, r = alldone_p$DRMS), color = "blue", fill = NA, size = 1) +
    labs(x = "X",
         y = "Y") +
    scale_color_manual(breaks = c('CEP', 'DRMS'),
                       values = c('CEP' = 'green', 'DRMS' = 'blue')) +
    theme(legend.position = 'bottom')
  print(plotted_all)
  
  #ggsave(filename = 'Abb29isha_CEP_DRMS_Trial11.png', plot = plotted_all, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Genauigkeit/Plots", height=10, width=16, bg = "white", units="cm")
}
# complete plot for trial 1.2
{
  alldone <- subset(data_IQR, Trial == "1.2")
  alldone_p <- subset(accuracy_measures, Trial == "1.2")
  front <- subset(alldone, Pos == "50m")
  middle <- subset(alldone, Pos == "90m")
  back <- subset(alldone, Pos == "115m")
  
  
  colors <- c('50m' = 'antiquewhite4', '90m' = 'aquamarine4', '115m' = 'bisque4', 'Referenzpunkt' = 'black', 'CEP' = 'green', 'DRMS' = 'blue')
  
  plotted_all <- ggplot(fill = 'colors') +
    geom_point(data = front, aes(x=x, y=y), color = 'antiquewhite4', size = 0.05) +
    geom_point(data = middle, aes(x=x, y=y), color = 'aquamarine4', size = 0.05) +
    geom_point(data = back, aes(x=x, y=y), color = 'burlywood4', size = 0.05) +
    geom_point(data = df_ref, aes(x=x, y=y), color = 'black', size = 1) +
    geom_circle(data = df_ref, aes(x0 = x, y0 = y, r = alldone_p$CEP), color = "green", fill = NA, size = 1) +
    geom_circle(data = df_ref, aes(x0 = x, y0 = y, r = alldone_p$DRMS), color = "blue", fill = NA, size = 1) +
    labs(x = "X",
         y = "Y") +
    scale_color_manual(breaks = c('CEP', 'DRMS'),
                       values = c('CEP' = 'green', 'DRMS' = 'blue')) +
    theme(legend.position = 'bottom')
  print(plotted_all)
  
  #ggsave(filename = 'Abb29isha_CEP_DRMS_Trial12.png', plot = plotted_all, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Genauigkeit/Plots", height=10, width=16, bg = "white", units="cm")
}

# complete plot for trial 1.2
{
  alldone <- subset(data_IQR, Trial == "2.1")
  alldone_p <- subset(accuracy_measures, Trial == "2.1")
  front <- subset(alldone, Pos == "50m")
  middle <- subset(alldone, Pos == "90m")
  back <- subset(alldone, Pos == "115m")
  
  
  colors <- c('50m' = 'antiquewhite4', '90m' = 'aquamarine4', '115m' = 'bisque4', 'Referenzpunkt' = 'black', 'CEP' = 'green', 'DRMS' = 'blue')
  
  plotted_all <- ggplot(fill = 'colors') +
    geom_point(data = front, aes(x=x, y=y), color = 'antiquewhite4', size = 0.05) +
    geom_point(data = middle, aes(x=x, y=y), color = 'aquamarine4', size = 0.05) +
    geom_point(data = back, aes(x=x, y=y), color = 'burlywood4', size = 0.05) +
    geom_point(data = df_ref, aes(x=x, y=y), color = 'black', size = 1) +
    geom_circle(data = df_ref, aes(x0 = x, y0 = y, r = alldone_p$CEP), color = "green", fill = NA, size = 1) +
    geom_circle(data = df_ref, aes(x0 = x, y0 = y, r = alldone_p$DRMS), color = "blue", fill = NA, size = 1) +
    labs(x = "X",
         y = "Y") +
    scale_color_manual(breaks = c('CEP', 'DRMS'),
                       values = c('CEP' = 'green', 'DRMS' = 'blue')) +
    theme(legend.position = 'bottom')
  print(plotted_all)
  
  #ggsave(filename = 'Abb29isha_CEP_DRMS_Trial21.png', plot = plotted_all, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Genauigkeit/Plots", height=10, width=16, bg = "white", units="cm")
}

# complete Plot for Trial 2.2
{alldone <- subset(data_IQR, Trial == "2.2")
  alldone_p <- subset(accuracy_measures, Trial == "2.2")
  front <- subset(alldone, Pos == "50m")
  middle <- subset(alldone, Pos == "90m")
  back <- subset(alldone, Pos == "115m")
  
  
  colors <- c('50m' = 'antiquewhite4', '90m' = 'aquamarine4', '115m' = 'bisque4', 'Referenzpunkt' = 'black', 'CEP' = 'green', 'DRMS' = 'blue')
  
  plotted_all <- ggplot(fill = 'colors') +
    geom_point(data = front, aes(x=x, y=y), color = 'antiquewhite4', size = 0.05) +
    geom_point(data = middle, aes(x=x, y=y), color = 'aquamarine4', size = 0.05) +
    geom_point(data = back, aes(x=x, y=y), color = 'burlywood4', size = 0.05) +
    geom_point(data = df_ref, aes(x=x, y=y), color = 'black', size = 1) +
    geom_circle(data = df_ref, aes(x0 = x, y0 = y, r = alldone_p$CEP), color = "green", fill = NA, size = 1) +
    geom_circle(data = df_ref, aes(x0 = x, y0 = y, r = alldone_p$DRMS), color = "blue", fill = NA, size = 1) +
    labs(x = "X",
         y = "Y") +
    scale_color_manual(breaks = c('CEP', 'DRMS'),
                       values = c('CEP' = 'green', 'DRMS' = 'blue')) +
    theme(legend.position = 'bottom')
  print(plotted_all)
  #ggsave(filename = 'Abb29isha_CEP_DRMS_Trial22.png', plot = plotted_all, path ="~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Genauigkeit/Plots", height=10, width=16, bg = "white", units="cm")
}

#### more trys for statistical analysis without result ####
# more statistic and dependand paramaters?
deviceone <- subset(alldone, DeviceNumber == 1)
q <- ggplot(deviceone, aes(x=dist, y=RSSI)) +
  geom_point()
print(q)

# Plot the Temperature Class (1-15 in 2 Degree increments starting at 4 degrees)
observation_counts <- data_IQR %>%
  group_by(TempClass, Trial) %>%
  summarise(obs_count = n())

Trial1done <- subset(data_IQR, Trial == "2.2")
p_temp_Time <-  ggplot(Trial1done, aes(x = as.factor(DeviceNumber), y = Hum, color = Pos)) +
  geom_boxplot() +
  theme_minimal()
print(p_temp_Time)

p <- ggplot(data_IQR, aes(x=dist, y=SNR)) +
  geom_point()
print(p)


ggplot(data_IQR, aes(x=RSSI, y=dist, color = Trial)) +
    geom_smooth()


ggplot(data_IQR, aes(x=Pos, y=RSSI, color=Trial)) +
  geom_boxplot()
         
ggplot(data_IQR, aes(x=as.factor(m_class), y=RSSI)) +
  geom_boxplot()


ggplot(data_calced_copy, aes(x=as.factor(m_class), y=RSSI)) +
  geom_point()

ggplot(resulting, aes(x=as.factor(m_class), y=RSSI)) +
  geom_boxplot()


ggplot(data_IQR, aes(x=as.factor(Trial), y=dist)) +
  geom_boxplot()


# Humidity
p_Hum_n <- ggplot(Trial1done, aes(x = as.factor(HumClass), y = RSSI)) +
  geom_boxplot()
print(p_Hum_n)




# Zeitklasse 5 min über distance
p_dist_t <- ggplot(data_IQR, aes(x=TimeClass5min, y=dist)) +
  geom_smooth()
print(p_dist_t)


#### #Cleaned and meaned ####
# smooth time over a 5 min period
data_smoothend <- data_IQR %>%
  mutate(Rounded_Time = floor_date(Date_Time, "5 minutes")) %>% 
  group_by(File_Dat, DeviceNumber)
resulting <- data_smoothend %>%
  group_by(DeviceNumber, File_Dat, Trial, Pos, Rounded_Time) %>% 
  summarise(
    Lat = mean(Lat),
    Lon = mean(Lon),
    Hum = mean(Hum),
    RSSI = mean(RSSI),
    SNR = mean(SNR),
    V = mean(V),
    .groups = "drop"
    )


#create subsets for each Position
{
  # Vorne
  {sub_Vorne <- resulting %>% 
    filter(Pos == "Vorne")
  df_ref_Vorne <- df_ref %>% 
    filter(Position == "Vorne")
  }
  # Seite
  {sub_Seite <- resulting %>% 
      filter(Pos == "Seite")
    df_ref_Seite <- df_ref %>% 
      filter(Position == "Seite")
  }
  # Hinten
  {sub_Hinten <- resulting %>% 
      filter(Pos == "Hinten")
    df_ref_Hinten <- df_ref %>% 
      filter(Position == "Hinten")
  }
  }


# map and calculate each Subset through
sub_Vorne$dist <- mapply(calculate_distance, sub_Vorne$Lat, sub_Vorne$Lon, 
                         ref_point_Vorne[2], ref_point_Vorne[1])


sub_Seite$dist <- mapply(calculate_distance, sub_Seite$Lat, sub_Seite$Lon, 
                         ref_point_Seite[2], ref_point_Seite[1])

sub_Hinten$dist <- mapply(calculate_distance, sub_Hinten$Lat, sub_Hinten$Lon, 
                          ref_point_Hinten[2], ref_point_Hinten[1])

resulting <- bind_rows(sub_Hinten, sub_Seite, sub_Vorne)

cartesian_coords_sm <- mercator_projection(resulting$Lat, resulting$Lon)
resulting$y <- cartesian_coords_sm$y
resulting$x <- cartesian_coords_sm$x

sub_DevOne <- resulting %>% 
  filter(DeviceNumber == 1)
ggplot(sub_DevOne, aes(x=as.factor(Trial), y=dist)) +
  geom_boxplot()

cartesian_coords_ref <- mercator_projection(df_ref$Lat, df_ref$Lon)

accuracy_measures_smooth <- resulting %>%
  group_by(Trial, Pos) %>%
  summarize(
    DRMS = DRMS(x, y),
    CEP = CEP(x, y),
    CEP_accuracy = CEP_accuracy(x, y)
  )

{
max_dist_sm <- max(resulting$dist)
  resulting <-  resulting %>% 
    mutate(m_class = cut(dist, breaks = c(0, seq(1, max_dist + 1, by = 1)), labels = FALSE))
  }

summary_Gateways <- data_calced %>% 
  group_by(Gateway) %>% 
  summarize(
    N = n()
  )
summary_Gateways_IQR <- data_IQR %>% 
  group_by(Gateway) %>% 
  summarize(
    N = n()
    )

summary_Gateways_dist <-  data_calced %>% 
  group_by(Gateway) %>% 
  summarize(
    N = n(),
    dist_mean = mean(dist)
  )

summary_Gateways_IQR_dist <-  data_IQR %>% 
  group_by(Gateway) %>% 
  summarize(
    N = n(),
    dist_mean = mean(dist)
)



#### Safe dataframes ####
setwd("~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data")

#write_csv(data_calced, file = "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Data_w_dist.csv")
#write_csv(data_IQR, file = "~/Documents/01_Uni/Master_Kiel/00_Master Arbeit/02_Data/01_Data_accuracy.csv")
