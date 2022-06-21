## Standardized_Variables_Train1


## New Standardized Data set for Train one

# Initiate data frame from the Train_1 data frame
Train_1_standardized <- Train_1

# Replace each value in the Train_1_standardized data frame with standardized values 
for (column_num in c(2:6)) {
  Train_1_standardized[,column_num] <- scale(Train_1_standardized[,column_num])
}


# Create data frame which selects the time period of December through March and 
# omit all NA values. We will use this data to plot the time around the 
# potential clean in place/membrane change. Use the data which has been standardized

df <- na.omit(Train_1_standardized[Train_1_standardized$Date_Time >= "2020-12-01 00:00:00" & 
                                 Train_1_standardized$Date_Time <= "2021-03-30 02:00:00",]) 

par(mfrow = c(1,1))

plot(df$Date_Time,                          # Net Driving Pressure
     df$Nt_DP,
     col = 1,
     ylim = c(-5,5),
     xlab = "Date",
     ylab = "Variables",
     main = "Variables over Time")
points(df$Date_Time,                        # Specific Flux
       df$Sp_Fl,
       col = "orange")
points(df$Date_Time,                        # Permeate Conductivity
       df$NCp,
       col = 3)
points(df$Date_Time,                        # Feed Water Pressure
       df$P_f,
       col = 6)

legend("topleft",                           # Add legend to plot
       c("Net driving pressure", "Specific flux", "Permeate conductivity","Feedwater Pressure"),
       lty = 1,
       col = c("black","orange", 3,6),
       cex = 0.48,
       horiz = TRUE)
