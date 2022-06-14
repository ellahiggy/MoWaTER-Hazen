## Hazen and Sawyer Team
## Rate of Change Train 1


## Filtered Data set for Train one

vars <- c("Date_Time","Nt_DP_TR1","Sp_Fl_TR1","NCp_TR1","Per_R_a_TR1","P_f_TR1")

Train_1_Rate <- HSMaster[vars]

### Rate of Change by Hour -----------------------------------------------------

# Rate of change of Net Driving pressure - Nt_DP_TR1 ---------------------------

for (column_num in c(2:6)) {
  Train_1_Rate[,column_num] <- c(diff(Train_1_Rate[,column_num]),NA)
}



# -----------------------------------------------------------------------------

# Plot of Specific Flux Rate of change

plot(Train_1_Rate$Sp_Fl_TR1 ~ Train_1_Rate$Date_Time)

# December through March Rate of Change per Hour -------------------------------

df <- na.omit(Train_1_Rate[Train_1_Rate$Date_Time >= "2020-12-01 00:00:00" & 
                             Train_1_Rate$Date_Time <= "2021-03-30 02:00:00",])


plot(df$Date_Time,                              # Draw first time series
     df$Nt_DP_TR1,
     col = 1,
     xlab = "Date",
     ylab = "Variables",
     main = "Variables over Time")

points(df$Date_Time,                              # Draw second time series
       df$Sp_Fl_TR1,
       col = "orange")
points(df$Date_Time,                              # Draw third time series
       df$NCp_TR1,
       col = 3)
points(df$Date_Time,                              # Draw fourth time series
       df$Per_R_a_TR1,
       col = 4)
points(df$Date_Time,                              # Draw fifth time series
       df$P_f_TR1,
       col = 6)
#points(df$Date_Time, 600*as.integer(is.na(df$Sp_Fl_TR1))*mean(df$Sp_Fl_TR1,na.rm=TRUE), col = "orange", type = "h")


legend("topleft",                           # Add legend to plot
       c("Net driving pressure", "Specific flux", "Permeate conductivity","% Recovery","Feedwater Pressure"),
       lty = 1,
       col = c("black","orange", 3,4,6),
       cex = 0.48,
       horiz = TRUE)

par(mfrow=c(2,1))

plot(df$Date_Time,                              # Draw third time series
       df$NCp_TR1,
       col = 3)
 

### Rate of Change by Date ------------------------------------------------------

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

vars <- c("Date_Time","Nt_DP_TR1","Sp_Fl_TR1","NCp_TR1","Per_R_a_TR1","P_f_TR1")

HSMaster_short <- HSMaster[vars]
HSMaster_short[is.na(HSMaster_short)] = 0 

HSMaster_short$Date_Time <- date(HSMaster$Date_Time)
dates <- unique(date(HSMaster$Date_Time))

HSMaster_ByDate <- as.data.frame(matrix(nrow = length(dates), ncol = 6))
colnames(HSMaster_ByDate) <- vars
HSMaster_ByDate[,1] = dates 

for (d in dates){
  info <- HSMaster_short[HSMaster_short$Date_Time == d,]
  HSMaster_ByDate[HSMaster_ByDate$Date_Time == d,2:6] <- colMeans(info[,2:6])
}

## Rate of change by Date for Train one 

HSMaster_ByDate_Rate <- HSMaster_ByDate

for (column_num in c(2:6)) {
  HSMaster_ByDate_Rate[,column_num] <- c(diff(HSMaster_ByDate_Rate[,column_num]),NA)
}



df <- HSMaster_ByDate_Rate[HSMaster_ByDate_Rate$Date_Time >= "2020-12-01 00:00:00" & 
                             HSMaster_ByDate_Rate$Date_Time <= "2021-03-30 02:00:00",]


plot(df$Date_Time,                              # Draw first time series
     df$Nt_DP_TR1,
     col = 1,
     xlab = "Date",
     ylab = "Variables",
     main = "Variables over Time")

plot(df$Date_Time,                              # Draw second time series
       df$Sp_Fl_TR1,
       col = "orange")
points(df$Date_Time,                              # Draw third time series
       scale(df$NCp_TR1),
       col = 3)
points(df$Date_Time,                              # Draw fourth time series
       df$Per_R_a_TR1,
       col = 4)
points(df$Date_Time,                              # Draw fifth time series
       df$P_f_TR1,
       col = 6)
#points(df$Date_Time, 600*as.integer(is.na(df$Sp_Fl_TR1))*mean(df$Sp_Fl_TR1,na.rm=TRUE), col = "orange", type = "h")


legend("topleft",                           # Add legend to plot
       c("Net driving pressure", "Specific flux", "Permeate conductivity","% Recovery","Feedwater Pressure"),
       lty = 1,
       col = c("black","orange", 3,4,6),
       cex = 0.48,
       horiz = TRUE)



