# -------------------------------------------------------------------------
# MOWATER 
# Hazen and Sawyer Team
# EDA
# Henry Burch, PJ Williams, Ella Higginbotham, Lauren Walker, Cyril Pillai
# -------------------------------------------------------------------------

load("Hazen_Sawyer_Data_Clean.rda")

#load in each dataframe object from the list

Train_1 <- HS_Dataset$Train_1
Train_2 <- HS_Dataset$Train_2
Train_3 <- HS_Dataset$Train_3
Train_4 <- HS_Dataset$Train_4
Train_5 <- HS_Dataset$Train_5

Train1_ON_Change <- HS_Dataset$Train1_ON_Change
Train2_ON_Change <- HS_Dataset$Train2_ON_Change
Train3_ON_Change <- HS_Dataset$Train3_ON_Change
Train4_ON_Change <- HS_Dataset$Train4_ON_Change
Train5_ON_Change <- HS_Dataset$Train5_ON_Change


#----------------------------------------------------------------------

### Standardizing the data to compare variables on the same y-axis

## New Standardized Data set for Train one ---

# List of variables to use
vars <- c("Date_Time","Nt_DP","Sp_Fl","NCp","Per_R_a","P_f")

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
     df$Nt_DP,
     col = 1,
     ylim = c(-4,4),
     xlab = "Date",
     ylab = "Variables",
     main = "Standardized Variables over Time, Train 1 (Dec - Apr)", )
points(df$Date_Time,                              # Specific Flux
       df$Sp_Fl,
       col = "orange")
points(df$Date_Time,                              # Permeate Conductivity
       df$NCp,
       col = 3)
points(df$Date_Time,                              # % Recovery
      df$Per_R_a,
     col = 4)
points(df$Date_Time,                              # Feedwater Pressure
       df$P_f,
       col = 6)

legend("topleft",                                 # Add legend to plot
       c("Net driving pressure", "Specific flux", "Permeate conductivity","% Recovery", "Feedwater Pressure"),
       lty = 1,
       col = c("black","orange", 3, 4, 6),
       cex = 0.48,
       horiz = TRUE)


# Create data frame which selects the time period of April through May and 
# omit all NA values. We will use this data to plot the time around the 
# potential clean in place/membrane change. Use the data which has been standardized

df <- na.omit(Train_1_standardized[Train_1_standardized$Date_Time >= "2021-04-01 00:00:00" & 
                                     Train_1_standardized$Date_Time <= "2021-05-30 00:00:00",])

plot(df$Date_Time,                                # Bet Driving Pressure
     df$Nt_DP,
     col = 1,
     ylim = c(-5,5),
     xlab = "Date",
     ylab = "Variables",
     main = "Standardized Variables over Time, Train 1 (Apr - Jun)")
points(df$Date_Time,                              # Specific Flux
       df$Sp_Fl,
       col = "orange")
points(df$Date_Time,                              # Permeate Conductivity
       df$NCp,
       col = 3)
points(df$Date_Time,                              # % Recovery
       df$Per_R_a,
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


#--------------------------------------------------------------------------------

### This code creates 15 plots: One plot per train (five trains = five plots) 
# for specific flux over time, normalized differential pressure over time, 
# and permeate conductivity over time. 

## Specific Flux 

# Train 1 
# Comments for Train 1 Specific Flux are very similar for the rest of the plots

# Provides space for 2 rows, 3 columns for 5 plots
par(mfrow = c(1, 1)) 

# Makes NA values into T/F, T/F into integers (0s and 1s), and takes mean of them to put them on plot
na_values_1_spfl <- as.integer(is.na(Train_1$Sp_Fl))*mean(Train_1$Sp_Fl,na.rm=TRUE) 

# Finds time of NA values
na_time_1_spfl <- Train_1$Date_Time[is.na(Train_1$Sp_Fl)]

# Plots specific flux over time, subtracting off the mean of the data 
plot(Train_1$Date_Time, Train_1$Sp_Fl- median(Train_1$Sp_Fl, na.rm=TRUE), 
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


na_values_2_spfl <- as.integer(is.na(Train_2$Sp_Fl))*mean(Train_2$Sp_Fl,na.rm=TRUE) 

na_time_2_spfl <- Train_2$Date_Time[is.na(Train_2$Sp_Fl)]

plot(Train_2$Date_Time, Train_2$Sp_Fl - median(Train_2$Sp_Fl, na.rm=TRUE), 
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

na_values_3_spfl <- as.integer(is.na(Train_3$Sp_Fl))*mean(Train_3$Sp_Fl,na.rm=TRUE) 

na_time_3_spfl <- Train_3$Date_Time[is.na(Train_3$Sp_Fl)]

plot(Train_3$Date_Time, Train_3$Sp_Fl - median(Train_3$Sp_Fl, na.rm=TRUE), 
     main = "Train 3 Specific Flux Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Specific Flux", 
     ylim = c(-0.01, 0.006),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_3_spfl, col = adjustcolor("magenta", alpha = 0.1))


# Train 4 

na_values_4_spfl <- as.integer(is.na(Train_4$Sp_Fl))*mean(Train_4$Sp_Fl,na.rm=TRUE) 

na_time_4_spfl <- Train_4$Date_Time[is.na(Train_4$Sp_Fl)]

plot(Train_4$Date_Time, Train_4$Sp_Fl - median(Train_4$Sp_Fl, na.rm=TRUE), 
     main = "Train 4 Specific Flux Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Specific Flux", 
     ylim = c(-0.03, 0.03),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_4_spfl, col = adjustcolor("magenta", alpha = 0.1))


# Train 5 

na_values_5_spfl <- as.integer(is.na(Train_5$Sp_Fl))*mean(Train_5$Sp_Fl,na.rm=TRUE) 

na_time_5_spfl <- Train_5$Date_Time[is.na(Train_5$Sp_Fl)]

plot(Train_5$Date_Time, Train_5$Sp_Fl - median(Train_5$Sp_Fl, na.rm=TRUE), 
     main = "Train 5 Specific Flux Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Specific Flux", 
     ylim = c(-0.016, 0.015),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_5_spfl, col = adjustcolor("magenta", alpha = 0.1))



## Normalized Permeate Conductivity 
# Train 1 

par(mfrow = c(1, 1)) 


na_values_1_npc <- as.integer(is.na(Train_1$NCp))*mean(Train_1$NCp,na.rm=TRUE) 

na_time_1_npc <- Train_1$Date_Time[is.na(Train_1$NCp)]

plot(Train_1$Date_Time, Train_1$NCp - median(Train_1$NCp, na.rm=TRUE), 
     main = "Train 1 Normalized Permeate Conductivity Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Normalized Permeate Conductivity", 
     ylim = c(-10, 50),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_1_npc, col = adjustcolor("magenta", alpha = 0.1))


# Train 2 


na_values_2_npc <- as.integer(is.na(Train_2$NCp))*mean(Train_2$NCp,na.rm=TRUE) 

na_time_2_npc <- Train_2$Date_Time[is.na(Train_2$NCp)]

plot(Train_2$Date_Time, Train_2$NCp - median(Train_2$NCp, na.rm=TRUE), 
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

na_values_3_npc <- as.integer(is.na(Train_3$NCp))*mean(Train_3$NCp,na.rm=TRUE) 

na_time_3_npc <- Train_2$Date_Time[is.na(Train_3$NCp)]

plot(Train_3$Date_Time, Train_3$NCp - median(Train_3$NCp, na.rm=TRUE), 
     main = "Train 3 Normalized Permeate Conductivity Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Normalized Permeate Conductivity", 
     ylim = c(-2, 2),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_3_npc, col = adjustcolor("magenta", alpha = 0.2))


# Train 4 

na_values_4_npc <- as.integer(is.na(Train_4$NCp))*mean(Train_4$NCp,na.rm=TRUE) 

na_time_4_npc <- Train_4$Date_Time[is.na(Train_4$NCp)]


plot(Train_4$Date_Time, Train_4$NCp - median(Train_4$NCp, na.rm=TRUE), 
     main = "Train 4 Normalized Permeate Conductivity Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Normalized Permeate Conductivity",
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_4_npc, col = adjustcolor("magenta", alpha = 0.2))


# Train 5 

na_values_5_npc <- as.integer(is.na(Train_5$NCp))*mean(Train_5$NCp,na.rm=TRUE) 

na_time_5_npc <- Train_4$Date_Time[is.na(Train_5$NCp)]

plot(Train_5$Date_Time, Train_5$NCp - median(Train_5$NCp, na.rm=TRUE), 
     main = "Train 5 Normalized Permeate Conductivity Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Normalized Permeate Conductivity", 
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_5_npc, col = adjustcolor("magenta", alpha = 0.2))

#----------------------------------------------------------------------------------


### Create plot to show data from every time train was on
##  ^^ Train 3 Specific Flux

# Create list of colors for our plot
library(viridis)
colors <- viridis(12)

# Initialize data sets for the below loop, these can be changed to work with any
# train 1-5

Change_Data <- Train3_ON_Change
Train_Data <- Train_3

# For loop which plots Specific Flux for each time the the train was on over top
# each other with the same starting point
for (num in 1:nrow(Change_Data)){
  data <- Train_Data[Train_Data$Index[Change_Data$Start_Index[num]:Change_Data$End_Index[num]],]
  list <- data$Sp_Fl
  data_indices <- c(Change_Data$Start_Index[num]:Change_Data$End_Index[num])
  data_indices <- data_indices - data_indices[1] + 1
  # The below statement standardizes each function over 1 hour
  #data_indices <- data_indices / Change_Data$Time_ON[num]
  if (num == 1){
    plot(data_indices, list, main = "Train 3 Specific Flux Over Time",type = "l",col = colors[num], ylim = c(.075,.09), xlab = "Time (hours)", ylab = "Specific Flux")
    model <- lm(list ~ data_indices)
    abline(model, col = colors[num])
    # }
  }
  else{
    points(data_indices, list, type = "l", col = colors[num])
    model <- lm(list ~ data_indices)
    abline(model, col = colors[num])
  }
}

# Add legend displaying colors in order of oldest to newest
legend("topright", inset = .01, title = "Cycle Number (12 = most recent)",legend= c(1:12)
       ,fill = colors, horiz=TRUE,cex = 0.45)


#----------------------------------------------------------------------------------------

#Standardized Variables Plots for Specific Flux and Permeate Conductivity


## Standardized_Variables_Train3
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


# Train 3

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




