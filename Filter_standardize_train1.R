## Hazen and Sawyer Team
## Filter_Standardize_Train1


## New Standardized Data set for Train one

vars <- c("Date_Time","Nt_DP_TR1","Sp_Fl_TR1","NCp_TR1","Per_R_a_TR1","P_f_TR1")

Train_1_filtered <- HSMaster[vars]

Train_1_filtered$Nt_DP_TR1_scaled <- scale(Train_1_filtered$Nt_DP_TR1)
Train_1_filtered$Sp_Fl_TR1_scaled <- scale(Train_1_filtered$Sp_Fl_TR1)
Train_1_filtered$NCp_TR1_scaled <- scale(Train_1_filtered$NCp_TR1)
Train_1_filtered$Per_R_a_TR1_scaled <- scale(Train_1_filtered$Per_R_a_TR1)
Train_1_filtered$Nt_DP_TR1_scaled <- scale(Train_1_filtered$Nt_DP_TR1)
Train_1_filtered$P_f_TR1_scaled <- scale(Train_1_filtered$P_f_TR1)


plot(Nt_DP_TR1 ~ Date_Time, data = Train_1_filtered)

plot(Nt_DP_TR1_scaled ~ Date_Time, data = Train_1_filtered)


################################################################################

### December through March


df <- na.omit(Train_1_filtered[Train_1_filtered$Date_Time >= "2020-12-01 00:00:00" & 
                                 Train_1_filtered$Date_Time <= "2021-03-30 02:00:00",])



plot(df$Date_Time,                              # Draw first time series
     df$Nt_DP_TR1_scaled,
     col = 1,
     ylim = c(-5,5),
     xlab = "Date",
     ylab = "Variables",
     main = "Variables over Time")
points(df$Date_Time,                              # Draw senond time series
       df$Sp_Fl_TR1_scaled,
       col = "orange")
points(df$Date_Time,                              # Draw third time series
       df$NCp_TR1_scaled,
       col = 3)
points(df$Date_Time,                              # Draw fourth time series
       df$Per_R_a_TR1_scaled,
       col = 4)
points(df$Date_Time,                              # Draw fifth time series
       df$P_f_TR1_scaled,
       col = 6)
#points(df$Date_Time, 600*as.integer(is.na(df$Sp_Fl_TR1))*mean(df$Sp_Fl_TR1,na.rm=TRUE), col = "orange", type = "h")


legend("topleft",                           # Add legend to plot
       c("Net driving pressure", "Specific flux", "Permeate conductivity","% Recovery","Feedwater Pressure"),
       lty = 1,
       col = c("black","orange", 3,4,6),
       cex = 0.48,
       horiz = TRUE)















