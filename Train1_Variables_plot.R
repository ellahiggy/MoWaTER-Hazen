
df <- na.omit(Train_1_data_on[Train_1_data_on$Date_Time >= "2020-12-01 00:00:00" & 
                                                    Train_1_data_on$Date_Time <= "2021-03-30 02:00:00",])

df2 <- na.omit(Train_1_data_on[Train_1_data_on$Date_Time >= "2021-04-01 00:00:00" & 
                                 Train_1_data_on$Date_Time <= "2021-05-30 00:00:00",])

### December through March

par(mfrow=c(2,1))

plot(df$Date_Time,                              # Draw first time series
     df$Nt_DP_TR1,
     col = 1,
     ylim = c(50,200),
     xlab = "Date",
     ylab = "Variables",
     main = "Variables over Time")
points(df$Date_Time,                              # Draw senond time series
      650*df$Sp_Fl_TR1,
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
points(df$Date_Time, 600*as.integer(is.na(df$Sp_Fl_TR1))*mean(df$Sp_Fl_TR1,na.rm=TRUE), col = "orange", type = "h")


#legend("topleft",                           # Add legend to plot
#       c("Net driving pressure", "Specific flux", "Permeate conductivity","% Recovery","Feedwater Pressure"),
#       lty = 1,
#       col = c("black","orange", 3,4,6),
#       cex = 0.53,
#       pt.cex = 2.5,
#       horiz = TRUE)


############ April May


plot(df2$Date_Time,                              # Draw first time series
     df2$Nt_DP_TR1,
     col = 1,
     ylim = c(50,200),
     xlab = "Date",
     ylab = "Variables",
     main = "Variables over Time")
points(df2$Date_Time,                              # Draw senond time series
       650*df2$Sp_Fl_TR1,
       col = "orange")
points(df2$Date_Time,                              # Draw third time series
       df2$NCp_TR1,
       col = 3)
points(df2$Date_Time,                              # Draw fourth time series
       df2$Per_R_a_TR1,
       col = 4)
points(df2$Date_Time,                              # Draw fifth time series
       df2$P_f_TR1,
       col = 6)
points(df2$Date_Time, 600*as.integer(is.na(df2$Sp_Fl_TR1))*mean(df2$Sp_Fl_TR1,na.rm=TRUE), col = "orange", type = "h")


#legend("topleft",                           # Add legend to plot
#       c("Net driving pressure", "Specific flux", "Permeate conductivity","% Recovery","Feedwater Pressure"),
#       lty = 1,
#       col = c("black","orange", 3,4,6),
#       cex = 0.53,
#       pt.cex = 2.5,
#       horiz = TRUE)



