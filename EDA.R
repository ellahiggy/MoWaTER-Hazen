# -------------------------------------------------------------------------
# MOWATER 
# Hazen and Sawyer Team
# EDA
# Henry Burch, PJ Williams, Ella Higginbotham, Lauren Walker, Cyril Pillai
# -------------------------------------------------------------------------

## Directory
# setwd("~/Desktop/Data")
# load("HSMaster - Hazen and Sawyer.rda")


library(lubridate)

###### FIX Line 657

# Make variable names general to all trains
General <- HSMaster[, c("Date_Time", "T_f_CO", "TCF_a")]

vars <- c("Date_Time","Sp_Fl","Nt_DP","NCp","Per_R_a","P_f","SP_n","ON_OFF","Index")


# Create separate data frames for each train with variables specified above
# add new column that gives index of observation

Train_1 <- HSMaster[,c("Date_Time", "Sp_Fl_TR1", "Nt_DP_TR1", 
                       "NCp_TR1", "Per_R_a_TR1", "P_f_TR1","SP_n_TR1","ON_TR1")]
Train_1$index <- 1:nrow(Train_1)
colnames(Train_1) <- vars

Train_2 <- HSMaster[,c("Date_Time", "Sp_Fl_TR2", "Nt_DP_TR2", 
                       "NCp_TR2", "Per_R_a_TR2", "P_f_TR2","SP_n_TR2","ON_TR2")]
Train_2$index <- 1:nrow(Train_2)
colnames(Train_2) <- vars

Train_3 <- HSMaster[,c("Date_Time", "Sp_Fl_TR3", "Nt_DP_TR3", 
                       "NCp_TR3", "Per_R_a_TR3", "P_f_TR3","SP_n_TR3","ON_TR3")]
Train_3$index <- 1:nrow(Train_3)
colnames(Train_3) <- vars

Train_4 <- HSMaster[,c("Date_Time", "Sp_Fl_TR4", "Nt_DP_TR4", 
                       "NCp_TR4", "Per_R_a_TR4", "P_f_TR4","SP_n_TR4",'ON_TR4')]
Train_4$index <- 1:nrow(Train_4)
colnames(Train_4) <- vars

Train_5 <- HSMaster[,c("Date_Time", "Sp_Fl_TR5", "Nt_DP_TR5", 
                       "NCp_TR5", "Per_R_a_TR5", "P_f_TR5","SP_n_TR5","ON_TR5")]
Train_5$index <- 1:nrow(Train_5)
colnames(Train_5) <- vars

#----------------------------------------------------------------------

### Standardizing the data to compare variables on the same y-axis

## New Standardized Data set for Train one ---

# List of variables to use
vars <- c("Date_Time","Nt_DP_TR1","Sp_Fl_TR1","NCp_TR1","Per_R_a_TR1","P_f_TR1")

# Initiate data frame from the Train_1 data frame
Train_1_standardized <- Train_1[vars]

# Replace each value in the Train_1 data frame with standardized values 
for (column_num in c(2:6)) {
  Train_1_standardized[,column_num] <- scale(Train_1_standardized[,column_num])
}

# Create data frame which selects the time period of December through March and 
# omit all NA values. We will use this data to plot the time around the 
# potential clean in place/membrane change. Use the data which has been standardized

df <- na.omit(Train_1_standardized[Train_1_standardized$Date_Time >= "2020-12-01 00:00:00" & 
                                     Train_1_standardized$Date_Time <= "2021-03-30 02:00:00",])

plot(df$Date_Time,                                # Net Driving Pressure
     df$Nt_DP_TR1_scaled,
     col = 1,
     ylim = c(-5,5),
     xlab = "Date",
     ylab = "Variables",
     main = "Variables over Time")
points(df$Date_Time,                              # Specific Flux
       df$Sp_Fl_TR1_scaled,
       col = "orange")
points(df$Date_Time,                              # Permeate Conductivity
       df$NCp_TR1_scaled,
       col = 3)
points(df$Date_Time,                              # % Conductivity
       df$Per_R_a_TR1_scaled,
       col = 4)
points(df$Date_Time,                              # Feedwater Pressure
       df$P_f_TR1_scaled,
       col = 6)

legend("topleft",                                 # Add legend to plot
       c("Net driving pressure", "Specific flux", "Permeate conductivity","% Recovery","Feedwater Pressure"),
       lty = 1,
       col = c("black","orange", 3,4,6),
       cex = 0.48,
       horiz = TRUE)


# Create data frame which selects the time period of April through May and 
# omit all NA values. We will use this data to plot the time around the 
# potential clean in place/membrane change. Use the data which has been standardized

df <- na.omit(Train_1_standardized[Train_1_standardized$Date_Time >= "2021-04-01 00:00:00" & 
                                     Train_1_standardized$Date_Time <= "2021-05-30 00:00:00",])

plot(df$Date_Time,                                # Bet Driving Pressure
     df$Nt_DP_TR1_scaled,
     col = 1,
     ylim = c(-5,5),
     xlab = "Date",
     ylab = "Variables",
     main = "Variables over Time")
points(df$Date_Time,                              # Specific Flux
       df$Sp_Fl_TR1_scaled,
       col = "orange")
points(df$Date_Time,                              # Permeate Conductivity
       df$NCp_TR1_scaled,
       col = 3)
points(df$Date_Time,                              # % Recovery
       df$Per_R_a_TR1_scaled,
       col = 4)
points(df$Date_Time,                              # Feed water Pressure 
       df$P_f_TR1_scaled,
       col = 6)

legend("topleft",                                 # Add legend to plot
       c("Net driving pressure", "Specific flux", "Permeate conductivity","% Recovery","Feedwater Pressure"),
       lty = 1,
       col = c("black","orange", 3,4,6),
       cex = 0.48,
       horiz = TRUE)


#--------------------------------------------------------------------------------

### This code creates 15 plots: One plot per train (five trains = five plots) 
# for specific flux over time, normalized differential pressure over time, 
# and permeate conductivity over time. 

## Specific Flux 

# Train 1 
# Comments for Train 1 Specific Flux are very similar for the rest of the plots

# Provides space for 2 rows, 3 columns for 5 plots
par(mfrow = c(2, 3)) 

# Creates a dataframe for Train 1
Train_1 <- HSMaster[, c(1, 37:50, 121:137)]

# Makes NA values into T/F, T/F into integers (0s and 1s), and takes mean of them to put them on plot
na_values_1_spfl <- as.integer(is.na(Train_1$Sp_Fl_TR1))*mean(Train_1$Sp_Fl_TR1,na.rm=TRUE) 

# Finds time of NA values
na_time_1_spfl <- Train_1$Date_Time[is.na(Train_1$Sp_Fl_TR1)]

# Plots specific flux over time, subtracting off the mean of the data 
plot(Train_1$Date_Time, Train_1$Sp_Fl_TR1- median(Train_1$Sp_Fl_TR1, na.rm=TRUE), 
     main = "Train 1 Specific Flux Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Specific Flux", 
     #ylim = c(0.07, 0.13), 
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

# Adds lines for NA values
abline(v = na_time_1_spfl, col = adjustcolor("magenta", alpha = 0.1))


# repeat same code for rest of variables and trains


# Train 2 

Train_2 <- HSMaster[, c(1, 51:64, 138:154)]

na_values_2_spfl <- as.integer(is.na(Train_2$Sp_Fl_TR2))*mean(Train_2$Sp_Fl_TR2,na.rm=TRUE) 

na_time_2_spfl <- Train_2$Date_Time[is.na(Train_2$Sp_Fl_TR2)]

plot(Train_2$Date_Time, Train_2$Sp_Fl_TR2 - median(Train_2$Sp_Fl_TR2, na.rm=TRUE), 
     main = "Train 2 Specific Flux Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Specific Flux", 
     ylim = c(-0.01, 0.01),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19, 
)

abline(v = na_time_2_spfl, col = adjustcolor("magenta", alpha = 0.2))


# Train 3 

Train_3 <- HSMaster[, c(1, 65:78, 155:171)]

na_values_3_spfl <- as.integer(is.na(Train_3$Sp_Fl_TR3))*mean(Train_3$Sp_Fl_TR3,na.rm=TRUE) 

na_time_3_spfl <- Train_3$Date_Time[is.na(Train_3$Sp_Fl_TR3)]

plot(Train_3$Date_Time, Train_3$Sp_Fl_TR3 - median(Train_3$Sp_Fl_TR3, na.rm=TRUE), 
     main = "Train 3 Specific Flux Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Specific Flux", 
     ylim = c(-0.01, 0.006),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_3_spfl, col = adjustcolor("magenta", alpha = 0.1))


# Train 4 

Train_4 <- HSMaster[, c(1, 79:98, 117, 172:188)]

na_values_4_spfl <- as.integer(is.na(Train_4$Sp_Fl_TR4))*mean(Train_4$Sp_Fl_TR4,na.rm=TRUE) 

na_time_4_spfl <- Train_4$Date_Time[is.na(Train_4$Sp_Fl_TR4)]

plot(Train_4$Date_Time, Train_4$Sp_Fl_TR4 - median(Train_4$Sp_Fl_TR4, na.rm=TRUE), 
     main = "Train 4 Specific Flux Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Specific Flux", 
     ylim = c(-0.03, 0.03),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_4_spfl, col = adjustcolor("magenta", alpha = 0.1))


# Train 5 

Train_5 <- HSMaster[, c(1, 99:116, 118, 189:205)]

na_values_5_spfl <- as.integer(is.na(Train_5$Sp_Fl_TR5))*mean(Train_5$Sp_Fl_TR5,na.rm=TRUE) 

na_time_5_spfl <- Train_5$Date_Time[is.na(Train_5$Sp_Fl_TR5)]

plot(Train_5$Date_Time, Train_5$Sp_Fl_TR5 - median(Train_5$Sp_Fl_TR5, na.rm=TRUE), 
     main = "Train 5 Specific Flux Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Specific Flux", 
     ylim = c(-0.016, 0.015),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_5_spfl, col = adjustcolor("magenta", alpha = 0.1))


## Normalized Differential Pressure 

# Train 1 

par(mfrow = c(2, 3)) 

Train_1 <- HSMaster[, c(1, 37:50, 121:137)]

na_values_1_ndp <- as.integer(is.na(Train_1$DP_n_TR1))*mean(Train_1$DP_n_TR1,na.rm=TRUE) 

na_time_1_ndp <- Train_1$Date_Time[is.na(Train_1$DP_n_TR1)]

plot(Train_1$Date_Time, Train_1$DP_n_TR1 - median(Train_1$DP_n_TR1, na.rm=TRUE), 
     main = "Train 1 Normalized Differential Pressure Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Normalized Differential Pressure", 
     ylim = c(-3, 5),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_1_ndp, col = adjustcolor("magenta", alpha = 0.1))


# Train 2 

Train_2 <- HSMaster[, c(1, 51:64, 138:154)]

na_values_2_ndp <- as.integer(is.na(Train_2$DP_n_TR2))*mean(Train_2$DP_n_TR2,na.rm=TRUE) 

na_time_2_ndp <- Train_2$Date_Time[is.na(Train_2$DP_n_TR2)]

plot(Train_2$Date_Time, Train_2$DP_n_TR2 - median(Train_2$DP_n_TR2, na.rm=TRUE), 
     main = "Train 2 Normalized Differential Pressure Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Normalized Differential Pressure", 
     ylim = c(-3, 2),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19, 
)

abline(v = na_time_2_ndp, col = adjustcolor("magenta", alpha = 0.2))


# Train 3 

Train_3 <- HSMaster[, c(1, 65:78, 155:171)]

na_values_3_ndp <- as.integer(is.na(Train_3$DP_n_TR3))*mean(Train_3$DP_n_TR3,na.rm=TRUE) 

na_time_3_ndp <- Train_3$Date_Time[is.na(Train_3$DP_n_TR3)]

plot(Train_3$Date_Time, Train_3$DP_n_TR3 - median(Train_3$DP_n_TR3, na.rm=TRUE), 
     main = "Train 3 Normalized Differential Pressure Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Normalized Differential Pressure", 
     ylim = c(-2.7, 1.5),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_3_ndp, col = adjustcolor("magenta", alpha = 0.2))


# Train 4 

Train_4 <- HSMaster[, c(1, 79:98, 117, 172:188)]

na_values_4_ndp <- as.integer(is.na(Train_4$DP_n_TR4))*mean(Train_4$DP_n_TR4,na.rm=TRUE) 

na_time_4_ndp <- Train_4$Date_Time[is.na(Train_4$DP_n_TR4)]

plot(Train_4$Date_Time, Train_4$DP_n_TR4 - median(Train_4$DP_n_TR4, na.rm=TRUE), 
     main = "Train 4 Normalized Differential Pressure Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Normalized Differential Pressure", 
     ylim = c(-6, 8),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_4_ndp, col = adjustcolor("magenta", alpha = 0.2))


# Train 5

Train_5 <- HSMaster[, c(1, 99:116, 118, 189:205)]

na_values_5_ndp <- as.integer(is.na(Train_5$DP_n_TR5))*mean(Train_5$DP_n_TR5,na.rm=TRUE) 

na_time_5_ndp <- Train_5$Date_Time[is.na(Train_5$DP_n_TR5)]

plot(Train_5$Date_Time, Train_5$DP_n_TR5 - median(Train_5$DP_n_TR5, na.rm=TRUE), 
     main = "Train 5 Normalized Differential Pressure Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Normalized Differential Pressure", 
     ylim = c(-4.5, 3),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_5_ndp, col = adjustcolor("magenta", alpha = 0.2))



## Normalized Permeate Conductivity 
# Train 1 

par(mfrow = c(2, 3)) 

Train_1 <- HSMaster[, c(1, 37:50, 121:137)]

na_values_1_npc <- as.integer(is.na(Train_1$NCp_TR1))*mean(Train_1$NCp_TR1,na.rm=TRUE) 

na_time_1_npc <- Train_1$Date_Time[is.na(Train_1$NCp_TR1)]

plot(Train_1$Date_Time, Train_1$NCp_TR1 - median(Train_1$NCp_TR1, na.rm=TRUE), 
     main = "Train 1 Normalized Permeate Conductivity Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Normalized Permeate Conductivity", 
     ylim = c(-10, 50),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_1_npc, col = adjustcolor("magenta", alpha = 0.1))


# Train 2 

Train_2 <- HSMaster[, c(1, 51:64, 138:154)]

na_values_2_npc <- as.integer(is.na(Train_2$NCp_TR2))*mean(Train_2$NCp_TR2,na.rm=TRUE) 

na_time_2_npc <- Train_2$Date_Time[is.na(Train_2$NCp_TR2)]

plot(Train_2$Date_Time, Train_2$NCp_TR2 - median(Train_2$NCp_TR2, na.rm=TRUE), 
     main = "Train 2 Normalized Permeate Conductivity Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Normalized Permeate Conductivity", 
     ylim = c(-6, 5),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19, 
)

abline(v = na_time_2_npc, col = adjustcolor("magenta", alpha = 0.2))


# Train 3 

Train_3 <- HSMaster[, c(1, 65:78, 155:171)]

na_values_3_npc <- as.integer(is.na(Train_3$NCp_TR3))*mean(Train_3$NCp_TR3,na.rm=TRUE) 

na_time_3_npc <- Train_2$Date_Time[is.na(Train_3$NCp_TR3)]

plot(Train_3$Date_Time, Train_3$NCp_TR3 - median(Train_3$NCp_TR3, na.rm=TRUE), 
     main = "Train 3 Normalized Permeate Conductivity Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Normalized Permeate Conductivity", 
     ylim = c(-2, 2),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_3_npc, col = adjustcolor("magenta", alpha = 0.2))


# Train 4 

Train_4 <- HSMaster[, c(1, 79:98, 117, 172:188)]

na_values_4_npc <- as.integer(is.na(Train_4$NCp_TR4))*mean(Train_4$NCp_TR4,na.rm=TRUE) 

na_time_4_npc <- Train_4$Date_Time[is.na(Train_4$NCp_TR4)]

plot(Train_4$Date_Time, Train_4$NCp_TR4 - median(Train_4$NCp_TR4, na.rm=TRUE), 
     main = "Train 4 Normalized Permeate Conductivity Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Normalized Permeate Conductivity", 
     ylim = c(35, 50),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_4_npc, col = adjustcolor("magenta", alpha = 0.2))


# Train 5 

Train_5 <- HSMaster[, c(1, 99:116, 118, 189:205)]

na_values_5_npc <- as.integer(is.na(Train_5$NCp_TR5))*mean(Train_5$NCp_TR5,na.rm=TRUE) 

na_time_5_npc <- Train_4$Date_Time[is.na(Train_5$NCp_TR5)]

plot(Train_5$Date_Time, Train_5$NCp_TR5 - median(Train_5$NCp_TR5, na.rm=TRUE), 
     main = "Train 5 Normalized Permeate Conductivity Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Normalized Permeate Conductivity", 
     ylim = c(26, 36),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_5_npc, col = adjustcolor("magenta", alpha = 0.2))

#----------------------------------------------------------------------------------

### Looking at rate of change for certain variables in Train 1

vars <- c("Date_Time","Nt_DP_TR1","Sp_Fl_TR1","NCp_TR1","Per_R_a_TR1","P_f_TR1")

# Create shortened data frame with only the desired variables
Train_1_Rate <- Train_1[vars]

## Rate of Change by Hour 

# Replace each value in the Train_1_Rate data frame with difference values  
for (column_num in c(2:6)) {
  Train_1_Rate[,column_num] <- c(diff(Train_1_Rate[,column_num]),NA)
}


# Create data frame which selects the time period of December through March and 
# omit all NA values. We will use this data to plot the time around the 
# potential clean in place/membrane change. Use the data for Rate of Change by hour

df <- na.omit(Train_1_Rate[Train_1_Rate$Date_Time >= "2020-12-01 00:00:00" & 
                             Train_1_Rate$Date_Time <= "2021-03-30 02:00:00",])


plot(df$Date_Time,                                # Net Driving Pressure
     df$Nt_DP_TR1,
     col = 1,
     xlab = "Date",
     ylab = "Variables",
     main = "Variables over Time")

points(df$Date_Time,                              # Specific Flux
       df$Sp_Fl_TR1,
       col = "orange")
points(df$Date_Time,                              # Permeate Conductivity
       df$NCp_TR1,
       col = 3)
points(df$Date_Time,                              # Percent Recovery
       df$Per_R_a_TR1,
       col = 4)
points(df$Date_Time,                              # Feedwater Pressure
       df$P_f_TR1,
       col = 6)

legend("topleft",                                 # Add legend to plot
       c("Net driving pressure", "Specific flux", "Permeate conductivity","% Recovery","Feedwater Pressure"),
       lty = 1,
       col = c("black","orange", 3,4,6),
       cex = 0.48,
       horiz = TRUE)


## Also looking at rate of Change by Date (not hour)

# Create Data frame by Date -----

# List of variables to use
vars <- c("Date_Time","Nt_DP_TR1","Sp_Fl_TR1","NCp_TR1","Per_R_a_TR1","P_f_TR1")

# Create shortened data frame with only the desired variables and remove NA's
HSMaster_short <- HSMaster[vars]
HSMaster_short[is.na(HSMaster_short)] = 0 

# Change the Date_Time column to just the date without the time
HSMaster_short$Date_Time <- date(HSMaster$Date_Time)
# Create a list of unique dates since their a repeats due to multiple hours per day
dates <- unique(date(HSMaster$Date_Time))

# Create new data frame to append to with the 5 selected variables and the Date 
HSMaster_ByDate <- as.data.frame(matrix(nrow = length(dates), ncol = 6))
colnames(HSMaster_ByDate) <- vars
HSMaster_ByDate[,1] = dates 

# Append the average all of values with the same date from HSMaster_short to the
# new empty data frame HSMaster_ByDate
for (d in dates){
  info <- HSMaster_short[HSMaster_short$Date_Time == d,]
  HSMaster_ByDate[HSMaster_ByDate$Date_Time == d,2:6] <- colMeans(info[,2:6])
}

## Rate of change by Date for Train one 
HSMaster_ByDate_Rate <- HSMaster_ByDate

for (column_num in c(2:6)) {
  HSMaster_ByDate_Rate[,column_num] <- c(diff(HSMaster_ByDate_Rate[,column_num]),NA)
}


# Create data frame which selects the time period of December through March and 
# omit all NA values. We will use this data to plot the time around the 
# potential clean in place/membrane change. Use the data for Rate of Change by Date
df <- HSMaster_ByDate_Rate[HSMaster_ByDate_Rate$Date_Time >= "2020-12-01 00:00:00" & 
                             HSMaster_ByDate_Rate$Date_Time <= "2021-03-30 02:00:00",]


plot(df$Date_Time,                                # Net Driving Pressure
     df$Nt_DP_TR1,
     col = 1,
     xlab = "Date",
     ylab = "Variables",
     main = "Variables over Time")

plot(df$Date_Time,                                # Specific Flux
     df$Sp_Fl_TR1,
     col = "orange")
points(df$Date_Time,                              # Permeate Conductivity
       scale(df$NCp_TR1),
       col = 3)
points(df$Date_Time,                              # Percent Recovery
       df$Per_R_a_TR1,
       col = 4)
points(df$Date_Time,                              # Feed water Pressure
       df$P_f_TR1,
       col = 6)

legend("topleft",                                 # Add legend to plot
       c("Net driving pressure", "Specific flux", "Permeate conductivity","% Recovery","Feedwater Pressure"),
       lty = 1,
       col = c("black","orange", 3,4,6),
       cex = 0.48,
       horiz = TRUE)

# ---------------------------------------------------------------------------

### Create a data frame that gives the start and end indices of when the train is on
# Also includes how many times the train is on, how long the train is on,
# and how long the train is off between each time 
# used this to look at trends in between assumed CIPs


# Add Train Number below and remove comment
# data_ONOFF <- Train_#

data_ONOFF$ON_Count <- 0

data_ONOFF[is.na(data_ONOFF)] = 0
data_ONOFF[is.null(data_ONOFF)] = 0

off <- 0
on <- 0

len <- nrow(data_ONOFF)

for (num in 1:len) {
  if (data_ONOFF$ON_OFF[num] == 0|is.na(data_ONOFF$ON_OFF[num])){
    data_ONOFF$ON_Count[num] <- 0
  }
  else {
    if (num == 1){
      on <<- on + 1
    }
    else if (data_ONOFF$ON_OFF[num-1] == 0|is.na(data_ONOFF$ON_OFF[num-1])){
      on <<- on + 1
    }
    data_ONOFF$ON_Count[num] <- on
  }
}

Data_ON_Number <-  unique(data_ONOFF$ON_Count)
Data_ON_Number <- Data_ON_Number[Data_ON_Number != 0]; # without elements that are "b"
Data_ON_Number

Data_ON_Change <- as.data.frame(matrix(nrow = max(Data_ON_Number), ncol = 3))
colnames(Data_ON_Change) <- c("ON_Count","Start_Index","End_Index")

for (num in Data_ON_Number){
  Data_ON_Change$ON_Count[num] <- num
  Data_ON_Change$Start_Index[num] <- min(which(data_ONOFF$ON_Count == num))
  Data_ON_Change$End_Index[num] <- max(which(data_ONOFF$ON_Count == num))
  Data_ON_Change$Time_ON[num] <- max(which(data_ONOFF$ON_Count == num)) - min(which(data_ONOFF$ON_Count == num))
}


Data_ON_Change$Time_of_CIP[1] <- 0
for (num in 2:nrow(Data_ON_Change)){
  Data_ON_Change$Time_of_CIP[num] <- Data_ON_Change$Start_Index[num] - Data_ON_Change$End_Index[num-1]
}


# Add Train Number below and remove comment
# Train#_ON_Change = Data_ON_Change  


# ------------------------------------------------------------------------------------------------------

################ IDK WHAT THESE LAST THINGS MEAN HELP ###########################


Train3_ON_Change <- Train3_ON_Change[-c(5),]

for (num in 1:nrow(Train3_ON_Change)){
  x <- Train3_ON_Change$Time_ON[num] / 500
  data <- Train_3[Train_3$Index[Train3_ON_Change$Start_Index[num]:Train3_ON_Change$End_Index[num]],]
  list <- data$Sp_Fl
  data_indices <- c(Train3_ON_Change$Start_Index[num]:Train3_ON_Change$End_Index[num])
  data_indices <- data_indices - data_indices[1] + 1
  data_indices <- data_indices / x
  if (num == 1){
    plot(data_indices, list, main = "Specific FLux",type = "l",col = num, ylim = c(.075,.09))
  }
  else{
    points(data_indices, list, main = "Specific FLux", type = "l", col = num)
  }
}


# ------------------------------------------------------------------------------


Train3_ON_Change <- Train3_ON_Change[-c(5),]

for (num in 1:nrow(Train3_ON_Change)){
  data <- Train_3[Train_3$Index[Train3_ON_Change$Start_Index[num]:Train3_ON_Change$End_Index[num]],]
  list <- data$Sp_Fl
  y_adjust <- 1 - 
    
  x_scale <- Train3_ON_Change$Time_ON[num] / 500
  data_indices <- c(Train3_ON_Change$Start_Index[num]:Train3_ON_Change$End_Index[num])
  data_indices <- data_indices - data_indices[1] + 1
  data_indices <- data_indices / x_scale
  if (num == 1){
    plot(data_indices, list, main = "Specific FLux",type = "l",col = num, ylim = c(.075,.09))
  }
  else{
    points(data_indices, list, main = "Specific FLux", type = "l", col = num)
  }
}



# -----------------------------------------------------------------------------


# Train 3 Specific Flux

Train3_ON_Change <- Train3_ON_Change[-c(5),]

list_of_change_var <- c()
#list_of_hours

vecTimes <- paste(month(HSMaster$Date_Time[X$starting_locations]), "-" ,month(HSMaster$Date_Time[X$ending_locations]))



Change_Data <- Train3_ON_Change
Train_Data <- Train_3

for (num in 1:nrow(Change_Data)){
  data <- Train_Data[Train_Data$Index[Change_Data$Start_Index[num]:Change_Data$End_Index[num]],]
  list <- data$Sp_Fl
  data_indices <- c(Change_Data$Start_Index[num]:Change_Data$End_Index[num])
  data_indices <- data_indices - data_indices[1] + 1
  #data_indices <- data_indices / Change_Data$Time_ON[num]
  
  if (num == 1){
    if (Change_Data$Time_ON[num] > 100) {
      plot(data_indices, list, main = "Train 3 Specific Flux over Time",type = "l",col = num, ylim = c(.075,.09), xlab = "Standardized Time", ylab = "Specific Flux")
      model <- lm(list ~ data_indices)
      abline(model, col = num)
      list_of_change_var <- append(list_of_change_var,model$coefficients[2]*Change_Data$Time_ON[num])
    }
    
  }
  else{
    if (Change_Data$Time_ON[num] > 100) {
      points(data_indices, list, type = "l", col = num)
      model <- lm(list ~ data_indices)
      abline(model, col = num)
      list_of_change_var <- append(list_of_change_var,model$coefficients[2]*Change_Data$Time_ON[num])
    }
    
  }
}

barplot(-list_of_change_var)



# ------------------------------------------------------------------------------

# Train 3 Normalized Salt Passage

Train3_ON_Change <- Train3_ON_Change[-c(5),]

Change_Data <- Train3_ON_Change
Train_Data <- Train_3

for (num in 1:nrow(Change_Data)){
  data <- Train_Data[Train_Data$Index[Change_Data$Start_Index[num]:Change_Data$End_Index[num]],]
  list <- data$SP_n
  data_indices <- c(Change_Data$Start_Index[num]:Change_Data$End_Index[num])
  data_indices <- data_indices - data_indices[1] + 1
  #data_indices <- data_indices / Change_Data$Time_ON[num]
  
  if (num == 1){
    plot(data_indices, list, main = "Train 3 Normalized Salt Passage over Time",type = "l",col = num, ylim = c(1,1.3), xlab = "Standardized Time", ylab = "Normalized Salt Passage")
  }
  else{
    points(data_indices, list, type = "l", col = num)
  }
}




# ------------------------------------------------------------------------------

# Train 3 Specific Flux Adjusted Y

plot(Train_3$SP_n ~ Train_3$Date_Time)


Train3_ON_Change <- Train3_ON_Change[-c(5),]

Change_Data <- Train3_ON_Change
Train_Data <- Train_3

for (num in 1:nrow(Change_Data)){
  data <- Train_Data[Train_Data$Index[Change_Data$Start_Index[num]:Change_Data$End_Index[num]],]
  list <- data$Sp_Fl
  y_adjust <- .1 - list[1]
  list <- list + y_adjust
  x_scale <- Change_Data$Time_ON[num] / 1
  data_indices <- c(Change_Data$Start_Index[num]:Change_Data$End_Index[num])
  data_indices <- data_indices - data_indices[1] + 1
  data_indices <- data_indices / x_scale
  if (num == 1){
    plot(data_indices, list, main = "Specific FLux",type = "l",col = num, ylim = c(.095,.102))
  }
  else{
    points(data_indices, list, main = "Specific FLux", type = "l", col = num)
  }
}


# --------------

# ------------------------------------------------------------------------------

# Train 3 Net Driving Pressure

Train3_ON_Change <- Train3_ON_Change[-c(5),]

Change_Data <- Train3_ON_Change
Train_Data <- Train_3

for (num in 1:nrow(Change_Data)){
  x <- Change_Data$Time_ON[num] / 500
  data <- Train_Data[Train_Data$Index[Change_Data$Start_Index[num]:Change_Data$End_Index[num]],]
  list <- data$Nt_DP
  data_indices <- c(Change_Data$Start_Index[num]:Change_Data$End_Index[num])
  data_indices <- data_indices - data_indices[1] + 1
  data_indices <- data_indices / x
  if (num == 1){
    plot(data_indices, list, main = "Specific FLux",type = "l",col = num, ylim = c(90,180))
  }
  else{
    points(data_indices, list, main = "Specific FLux", type = "l", col = num)
  }
}


