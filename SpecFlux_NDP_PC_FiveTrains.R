### Plots of Specific Flux, Normalized Differential Pressure, Permeate Conductivity

# Cyril is super dope


# swd 
load("~/Desktop/DATA SCIENCE FELLOWS/Hazen and Sawyer/dataset/HSMaster - Hazen and Sawyer (1).rda")





## Specific Flux ---------------------------------------------------------------

# Train 1 ------------------------------------------

# get 2 rows, 3 columns to plot graphs in
par(mfrow = c(2, 3)) 

# get just Train 1 data
Train_1 <- HSMaster[, c(1, 37:50, 121:137)]

# pulling NA values for specific flux, then making them an integer
# and finding the mean of those integer NA values (either 0 or 1) to show
# NA blocks of time
na_values_1_spfl <- as.integer(is.na(Train_1$Sp_Fl_TR1))*mean(Train_1$Sp_Fl_TR1,na.rm=TRUE) 

# plot date vs. specific flux
plot(Train_1$Date_Time, Train_1$Sp_Fl_TR1, 
     main = "Train 1 Specific Flux Over Time, with NA values", 
     xlab = "Date", 
     ylab = "Specific Flux", 
     ylim = c(0.07, 0.13), 
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

# plot NA blocks of time
points(Train_1$Date_Time, na_values_1_spfl, 
       pch = 10, 
       type = "h",
       cex = 0.9, 
       col = "magenta")

# make legend
legend("topright", 
       legend = c("Specific Flux", "NA values"), 
       col = c("black", "magenta"), 
       lty = c(NA, 1),
       pch = c(19, NA)
)

# repeat same code for rest of variables and trains

# Train 2 ------------------------------------------

Train_2 <- HSMaster[, c(1, 51:64, 138:154)]

na_values_2_spfl <- as.integer(is.na(Train_2$Sp_Fl_TR2))*mean(Train_2$Sp_Fl_TR2,na.rm=TRUE) 

plot(Train_2$Date_Time, Train_2$Sp_Fl_TR2, 
     main = "Train 2 Specific Flux Over Time, with NA values", 
     xlab = "Date", 
     ylab = "Specific Flux", 
     ylim = c(0.09, 0.12),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19, 
)
points(Train_2$Date_Time, na_values_2_spfl, 
       pch = 10, 
       type = "h",
       cex = 0.9, 
       col = "magenta")
legend("topright", 
       legend = c("Specific Flux", "NA values"), 
       col = c("black", "magenta"), 
       lty = c(NA, 1),
       pch = c(19, NA)
)

# Train 3 ------------------------------------------

Train_3 <- HSMaster[, c(1, 65:78, 155:171)]

na_values_3_spfl <- as.integer(is.na(Train_3$Sp_Fl_TR3))*mean(Train_3$Sp_Fl_TR3,na.rm=TRUE) 

plot(Train_3$Date_Time, Train_3$Sp_Fl_TR3, 
     main = "Train 3 Specific Flux Over Time, with NA values", 
     xlab = "Date", 
     ylab = "Specific Flux", 
     ylim = c(0.07, 0.10),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)
points(Train_3$Date_Time, na_values_3_spfl, 
       pch = 10, 
       type = "h",
       cex = 0.9, 
       col = "magenta")
legend("topright", 
       legend = c("Specific Flux", "NA values"), 
       col = c("black", "magenta"), 
       lty = c(NA, 1),
       pch = c(19, NA)
)


# Train 4 ------------------------------------------

Train_4 <- HSMaster[, c(1, 79:98, 117, 172:188)]

na_values_4_spfl <- as.integer(is.na(Train_4$Sp_Fl_TR4))*mean(Train_4$Sp_Fl_TR4,na.rm=TRUE) 

plot(Train_4$Date_Time, Train_4$Sp_Fl_TR4, 
     main = "Train 4 Specific Flux Over Time, with NA values", 
     xlab = "Date", 
     ylab = "Specific Flux", 
     ylim = c(0.07, 0.13),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)
points(Train_4$Date_Time, na_values_4_spfl, 
       pch = 10, 
       type = "h",
       cex = 0.9, 
       col = "magenta")
legend("topright", 
       legend = c("Specific Flux", "NA values"), 
       col = c("black", "magenta"), 
       lty = c(NA, 1),
       pch = c(19, NA)
)


# Train 5 ------------------------------------------

Train_5 <- HSMaster[, c(1, 99:116, 118, 189:205)]

na_values_5_spfl <- as.integer(is.na(Train_5$Sp_Fl_TR5))*mean(Train_5$Sp_Fl_TR5,na.rm=TRUE) 

plot(Train_5$Date_Time, Train_5$Sp_Fl_TR5, 
     main = "Train 5 Specific Flux Over Time, with NA values", 
     xlab = "Date", 
     ylab = "Specific Flux", 
     ylim = c(0.11, 0.15),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)
points(Train_5$Date_Time, na_values_5_spfl, 
       pch = 10, 
       type = "h",
       cex = 0.9, 
       col = "magenta")
legend("topright", 
       legend = c("Specific Flux", "NA values"), 
       col = c("black", "magenta"), 
       lty = c(NA, 1),
       pch = c(19, NA)
)

#-------------------------------------------------------------------------------











## Normalized Differential Pressure --------------------------------------------

# Train 1 ------------------------------------------

par(mfrow = c(2, 3)) 

Train_1 <- HSMaster[, c(1, 37:50, 121:137)]

na_values_1_ndp <- as.integer(is.na(Train_1$DP_n_TR1))*mean(Train_1$DP_n_TR1,na.rm=TRUE) 

plot(Train_1$Date_Time, Train_1$DP_n_TR1, 
     main = "Train 1 Normalized Differential Pressure Over Time, with NA values", 
     xlab = "Date", 
     ylab = "Normalized Differential Pressure", 
     ylim = c(15, 25),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)
points(Train_1$Date_Time, na_values_1_ndp, 
       pch = 10, 
       type = "h",
       cex = 0.9, 
       col = "blue")
legend("topright", 
       legend = c("Normalized Differential Pressure", "NA values"), 
       col = c("black", "blue"), 
       lty = c(NA, 1),
       pch = c(19, NA)
)

# Train 2 ------------------------------------------

Train_2 <- HSMaster[, c(1, 51:64, 138:154)]

na_values_2_ndp <- as.integer(is.na(Train_2$DP_n_TR2))*mean(Train_2$DP_n_TR2,na.rm=TRUE) 

plot(Train_2$Date_Time, Train_2$DP_n_TR2, 
     main = "Train 2 Normalized Differential Pressure Over Time, with NA values", 
     xlab = "Date", 
     ylab = "Normalized Differential Pressure", 
     ylim = c(26, 32),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19, 
)
points(Train_2$Date_Time, na_values_2_ndp, 
       pch = 10, 
       type = "h",
       cex = 0.9, 
       col = "blue")
legend("topright", 
       legend = c("Normalized Differential Pressure", "NA values"), 
       col = c("black", "blue"), 
       lty = c(NA, 1),
       pch = c(19, NA)
)

# Train 3 ------------------------------------------

Train_3 <- HSMaster[, c(1, 65:78, 155:171)]

na_values_3_ndp <- as.integer(is.na(Train_3$DP_n_TR3))*mean(Train_3$DP_n_TR3,na.rm=TRUE) 

plot(Train_3$Date_Time, Train_3$DP_n_TR3, 
     main = "Train 3 Normalized Differential Pressure Over Time, with NA values", 
     xlab = "Date", 
     ylab = "Normalized Differential Pressure", 
     ylim = c(26, 32),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)
points(Train_3$Date_Time, na_values_3_ndp, 
       pch = 10, 
       type = "h",
       cex = 0.9, 
       col = "blue")
legend("topright", 
       legend = c("Normalized Differential Pressure", "NA values"), 
       col = c("black", "blue"), 
       lty = c(NA, 1),
       pch = c(19, NA)
)


# Train 4 ------------------------------------------

Train_4 <- HSMaster[, c(1, 79:98, 117, 172:188)]

na_values_4_ndp <- as.integer(is.na(Train_4$DP_n_TR4))*mean(Train_4$DP_n_TR4,na.rm=TRUE) 

plot(Train_4$Date_Time, Train_4$DP_n_TR4, 
     main = "Train 4 Normalized Differential Pressure Over Time, with NA values", 
     xlab = "Date", 
     ylab = "Normalized Differential Pressure", 
     ylim = c(20, 35),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)
points(Train_4$Date_Time, na_values_4_ndp, 
       pch = 10, 
       type = "h",
       cex = 0.9, 
       col = "blue")
legend("topright", 
       legend = c("Normalized Differential Pressure", "NA values"), 
       col = c("black", "blue"), 
       lty = c(NA, 1),
       pch = c(19, NA)
)


# Train 5 ------------------------------------------

Train_5 <- HSMaster[, c(1, 99:116, 118, 189:205)]

na_values_5_ndp <- as.integer(is.na(Train_5$DP_n_TR5))*mean(Train_5$DP_n_TR5,na.rm=TRUE) 

plot(Train_5$Date_Time, Train_5$DP_n_TR5, 
     main = "Train 5 Normalized Differential Pressure Over Time, with NA values", 
     xlab = "Date", 
     ylab = "Normalized Differential Pressure", 
     ylim = c(24, 32),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)
points(Train_5$Date_Time, na_values_5_ndp, 
       pch = 10, 
       type = "h",
       cex = 0.9, 
       col = "blue")
legend("topright", 
       legend = c("Normalized Differential Pressure", "NA values"), 
       col = c("black", "blue"), 
       lty = c(NA, 1),
       pch = c(19, NA)
)

#-------------------------------------------------------------------------------











## Normalized Permeate Conductivity --------------------------------------------

# Train 1 ------------------------------------------

par(mfrow = c(2, 3)) 

Train_1 <- HSMaster[, c(1, 37:50, 121:137)]

na_values_1_npc <- as.integer(is.na(Train_1$NCp_TR1))*mean(Train_1$NCp_TR1,na.rm=TRUE) 

plot(Train_1$Date_Time, Train_1$NCp_TR1, 
     main = "Train 1 Normalized Permeate Conductivity Over Time, with NA values", 
     xlab = "Date", 
     ylab = "Normalized Permeate Conductivity", 
     ylim = c(100, 180),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)
points(Train_1$Date_Time, na_values_1_npc, 
       pch = 10, 
       type = "h",
       cex = 0.9, 
       col = "orange")
legend("topright", 
       legend = c("Normalized Permeate Conductivity", "NA values"), 
       col = c("black", "orange"), 
       lty = c(NA, 1),
       pch = c(19, NA)
)

# Train 2 ------------------------------------------

Train_2 <- HSMaster[, c(1, 51:64, 138:154)]

na_values_2_npc <- as.integer(is.na(Train_2$NCp_TR2))*mean(Train_2$NCp_TR2,na.rm=TRUE) 

plot(Train_2$Date_Time, Train_2$NCp_TR2, 
     main = "Train 2 Normalized Permeate Conductivity Over Time, with NA values", 
     xlab = "Date", 
     ylab = "Normalized Permeate Conductivity", 
     ylim = c(20, 35),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19, 
)
points(Train_2$Date_Time, na_values_2_npc, 
       pch = 10, 
       type = "h",
       cex = 0.9, 
       col = "orange")
legend("topright", 
       legend = c("Normalized Permeate Conductivity", "NA values"), 
       col = c("black", "orange"), 
       lty = c(NA, 1),
       pch = c(19, NA)
)

# Train 3 ------------------------------------------

Train_3 <- HSMaster[, c(1, 65:78, 155:171)]

na_values_3_npc <- as.integer(is.na(Train_3$NCp_TR3))*mean(Train_3$NCp_TR3,na.rm=TRUE) 

plot(Train_3$Date_Time, Train_3$NCp_TR3, 
     main = "Train 3 Normalized Permeate Conductivity Over Time, with NA values", 
     xlab = "Date", 
     ylab = "Normalized Permeate Conductivity", 
     ylim = c(22, 26),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)
points(Train_3$Date_Time, na_values_3_npc, 
       pch = 10, 
       type = "h",
       cex = 0.9, 
       col = "orange")
legend("topright", 
       legend = c("Normalized Permeate Conductivity", "NA values"), 
       col = c("black", "orange"), 
       lty = c(NA, 1),
       pch = c(19, NA)
)


# Train 4 ------------------------------------------

Train_4 <- HSMaster[, c(1, 79:98, 117, 172:188)]

na_values_4_npc <- as.integer(is.na(Train_4$NCp_TR4))*mean(Train_4$NCp_TR4,na.rm=TRUE) 

plot(Train_4$Date_Time, Train_4$NCp_TR4, 
     main = "Train 4 Normalized Permeate Conductivity Over Time, with NA values", 
     xlab = "Date", 
     ylab = "Normalized Permeate Conductivity", 
     ylim = c(35, 50),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)
points(Train_4$Date_Time, na_values_4_npc, 
       pch = 10, 
       type = "h",
       cex = 0.9, 
       col = "orange")
legend("topright", 
       legend = c("Normalized Permeate Conductivity", "NA values"), 
       col = c("black", "orange"), 
       lty = c(NA, 1),
       pch = c(19, NA)
)


# Train 5 ------------------------------------------

Train_5 <- HSMaster[, c(1, 99:116, 118, 189:205)]

na_values_5_npc <- as.integer(is.na(Train_5$NCp_TR5))*mean(Train_5$NCp_TR5,na.rm=TRUE) 

plot(Train_5$Date_Time, Train_5$NCp_TR5, 
     main = "Train 5 Normalized Permeate Conductivity Over Time, with NA values", 
     xlab = "Date", 
     ylab = "Normalized Permeate Conductivity", 
     ylim = c(26, 36),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)
points(Train_5$Date_Time, na_values_5_npc, 
       pch = 10, 
       type = "h",
       cex = 0.9, 
       col = "orange")
legend("topright", 
       legend = c("Normalized Permeate Conductivity", "NA values"), 
       col = c("black", "orange"), 
       lty = c(NA, 1),
       pch = c(19, NA)
)
