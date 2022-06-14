## Hazen and Sawyer Team
## Rate of Change Train 1


## Filtered Data set for Train one

vars <- c("Date_Time","Nt_DP_TR1","Sp_Fl_TR1","NCp_TR1","Per_R_a_TR1","P_f_TR1")

# Create shortened data frame with only the desired variables
Train_1_Rate <- HSMaster[vars]

### Rate of Change by Hour -----------------------------------------------------

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

 

### Rate of Change by Date -----------------------------------------------------

# remotes::install_github("business-science/timetk")
# 
# 
# install.packages("timetk")
# install.packages("dplyr")
# library(timetk)
# library(dplyr)
# HSMasterByDay <- HSMaster
# HSMasterByDay <- condense_period(HSMasterByDay, Date_Time, .period = "1 day", .side = c("start","end"))


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





