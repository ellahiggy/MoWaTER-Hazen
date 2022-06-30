## Standardized_Variables_Train3


vars <- c("Date_Time","Sp_Fl","DP_n","NCp","ON_OFF")


## New Standardized Data set for each Train


#Data for each variable by Train, as detailed above
Train_3_standardized <- Train_3[,c("Date_Time","Sp_Fl", "NCp","ON_OFF")]

for (column_num in c(2:3)) {
  Train_3_standardized[,column_num] <- scale(Train_3_standardized[,column_num])
}

Train_4_standardized <- Train_4[,c("Date_Time","Sp_Fl", "NCp","ON_OFF")]

for (column_num in c(2:3)) {
  Train_4_standardized[,column_num] <- scale(Train_4_standardized[,column_num])
}

Train_5_standardized <- Train_5[,c("Date_Time","Sp_Fl", "NCp","ON_OFF")]

for (column_num in c(2:3)) {
  Train_5_standardized[,column_num] <- scale(Train_5_standardized[,column_num])
}


# Create data frame which selects the time period of December through March and 
# omit all NA values. We will use this data to plot the time around the 
# potential clean in place/membrane change. Use the data which has been standardized


# Train 3 Zoom All

plot(Train_3_standardized$Sp_Fl ~ Train_3_standardized$Date_Time, type = "l", 
     main = "Train 3 Time Series (Magenta = Train Off)", 
     ylab = "Standardized Y Scale", 
     xlab = "Date",
     ylim = c(-10,10),
     col = "red")
points(Train_3_standardized$NCp ~ Train_3_standardized$Date_Time, col = "blue", type = "l")

train_3_off <- as.integer(!(Train_3_standardized$ON_OFF))
time_train_3_off <- Train_3_standardized$Date_Time[!(Train_3_standardized$ON_OFF)]
abline(v = time_train_3_off, col = "magenta")

legend("topright",
    legend = c("Specific Flux","Normalized Permeate Conductivity"),
       lty = 1,
       col = c("red","blue"),
       cex = 1,
       inset=c(0,0),
       horiz = FALSE)


# Train 3 Zoom in

plot(Train_3_standardized$Sp_Fl ~ Train_3_standardized$Date_Time, type = "l", 
     main = "Train 3 Time Series (Magenta = Train Off)", 
     ylab = "Standardized Y Scale", 
     xlab = "Date",
     ylim = c(-2.5,2.5),
     col = 'red')
points(Train_3_standardized$NCp ~ Train_3_standardized$Date_Time, col = "blue", type = "l")

train_3_off <- as.integer(!(Train_3_standardized$ON_OFF))
time_train_3_off <- Train_3_standardized$Date_Time[!(Train_3_standardized$ON_OFF)]
abline(v = time_train_3_off, col = "magenta")

legend("topright",
       legend = c("Specific Flux","Normalized Permeate Conductivity"),
       lty = 1,
       col = c("red","blue"),
       cex = 1,
       inset=c(0,0),
       horiz = FALSE)

# Train 4 ----------------------------------------------------------------------

plot(Train_4_standardized$Sp_Fl ~ Train_4_standardized$Date_Time, type = "l", 
     main = "Train 4 Time Series (Magenta = Train Off)", 
     ylab = "Standardized Y Scale", 
     xlab = "Date",
     ylim = c(-3,3),
     col = "red")
points(Train_4_standardized$NCp ~ Train_4_standardized$Date_Time, col = "blue", type = "l")

train_4_off <- as.integer(!(Train_4_standardized$ON_OFF))
time_train_4_off <- Train_4_standardized$Date_Time[!(Train_4_standardized$ON_OFF)]
abline(v = time_train_4_off, col = "magenta")

legend("topright",
       legend = c("Specific Flux","Normalized Permeate Conductivity"),
       lty = 1,
       col = c("red","blue"),
       cex = 1,
       inset=c(0,0),
       horiz = FALSE)


# Train 5 ----------------------------------------------------------------------

plot(Train_5_standardized$Sp_Fl ~ Train_5_standardized$Date_Time, type = "l", 
     main = "Train 5 Time Series (Magenta = Train Off)", 
     ylab = "Standardized Y Scale", 
     xlab = "Date",
     ylim = c(-3,3),
     col = "red")
points(Train_5_standardized$NCp ~ Train_5_standardized$Date_Time, col = "blue", type = "l")

train_5_off <- as.integer(!(Train_5_standardized$ON_OFF))
time_train_5_off <- Train_5_standardized$Date_Time[!(Train_5_standardized$ON_OFF)]
abline(v = time_train_5_off, col = "magenta")

legend("topright",
       legend = c("Specific Flux","Normalized Permeate Conductivity"),
       lty = 1,
       col = c("red","blue"),
       cex = 1,
       inset=c(0,0),
       horiz = FALSE)

