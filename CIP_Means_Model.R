
General <- HSMaster[, c("Date_Time", "T_f_CO", "TCF_a")]

vars <- c("Date_Time","Sp_Fl","Nt_DP","P_f","Index")


#Data for each variable by Train, as detailed above
Train_1 <- HSMaster[,c("Date_Time", "Sp_Fl_TR1", "Nt_DP_TR1", 
                       "P_f_TR1")]
Train_1$index <- 1:nrow(Train_1)
colnames(Train_1) <- vars

Train_2 <- HSMaster[,c("Date_Time", "Sp_Fl_TR2", "Nt_DP_TR2", 
                       "P_f_TR2")]
Train_2$index <- 1:nrow(Train_2)
colnames(Train_2) <- vars

Train_3 <- HSMaster[,c("Date_Time", "Sp_Fl_TR3", "Nt_DP_TR3", 
                       "P_f_TR3")]
Train_3$index <- 1:nrow(Train_3)
colnames(Train_3) <- vars

Train_4 <- HSMaster[,c("Date_Time", "Sp_Fl_TR4", "Nt_DP_TR4", 
                       "P_f_TR4")]
Train_4$index <- 1:nrow(Train_4)
colnames(Train_4) <- vars

Train_5 <- HSMaster[,c("Date_Time", "Sp_Fl_TR5", "Nt_DP_TR5", 
                       "P_f_TR5")]
Train_5$index <- 1:nrow(Train_5)
colnames(Train_5) <- vars

# ------------------------------------------------------------------------------

Train_3_Oct_thru_Nov <- Train_3[Train_3$Date_Time >= "2020-10-15 00:00:00" & 
                                  Train_3$Date_Time <= "2020-11-08 00:00:00",]

Train_3_Nov_thru_Jan <- Train_3[Train_3$Date_Time >= "2020-11-17 00:00:00" & 
                                  Train_3$Date_Time <= "2021-01-19 00:00:00",]

Train_3_Jan_thru_Apr <- Train_3[Train_3$Date_Time >= "2021-01-21 00:00:00" & 
                                  Train_3$Date_Time <= "2021-04-29 02:00:00",]


Train_5_Apr <- Train_5[Train_5$Date_Time >= "2021-04-05 00:00:00" & 
                         Train_5$Date_Time <= "2021-04-27 02:00:00",]

# ------------------------------------------------------------------------------

Train4_ON_Change <- subset(Train4_ON_Change,Time_ON > 1)

#-------------------------------------------------------------------------------
# Create new data frame to append to with the Change of 4 selected variables 
Changes_Between_CIP_DF <- as.data.frame(matrix(ncol= 3))
colnames(Changes_Between_CIP_DF) <- c("Sp_Fl","Nt_DP","P_f")

# Create new data frame to append to with the Percent Change of 4 selected variables 
Changes_Percent_Between_CIP_DF <- as.data.frame(matrix(ncol= 5))
colnames(Changes_Percent_Between_CIP_DF) <- c("Sp_Fl","Nt_DP","P_f","Start_Index","End_Index")

df_index <- 1

# ------------------------------------------------------------------------------

for (value in 2:nrow(Train4_ON_Change)){
  First_Index <- Train4_ON_Change$Start_Index[value]
  Last_Index <- Train4_ON_Change$End_Index[value]
  Train_Data_ON <- Train_4[Train_4$Index >= First_Index & 
                             Train_4$Index <= Last_Index,]
  List_of_Changes <- c()
  List_of_Percent_Changes <- c()
  
  for (num in 2:4){
    #plot(Train_Data_ON[,num] ~ Train_Data_ON$Index)
    model <- lm(Train_Data_ON[,num] ~ Train_Data_ON$Index)
    abline(model, col = 'red')
    slope <- as.numeric(model$coefficients[2])
    intercept <- as.numeric(model$coefficients[1])
    #time <- as.integer(difftime("2021-04-29 02:00:00","2021-01-21 00:00:00",units = c("hours")))
    change <- slope * nrow(Train_Data_ON)
    List_of_Changes <- append(List_of_Changes,change)
    Start_Val <- (slope * Train_Data_ON$Index[1] + intercept)
    Percent_Change <- change / Start_Val
    List_of_Percent_Changes <- append(List_of_Percent_Changes,Percent_Change)
  }
  Changes_Between_CIP_DF[df_index,] <- List_of_Changes 
  Start_End <- c(First_Index,Last_Index)
  Row <- append(List_of_Percent_Changes,Start_End)
  Changes_Percent_Between_CIP_DF[df_index,] <- Row
  df_index <<- df_index + 1
}

#---------------------------------------------------------------------------------

Percent_Means_List <- as.numeric(colMeans(Changes_Percent_Between_CIP_DF))
Changes_Percent_Between_CIP_DF$CIP <- 0

for (row in 1:nrow(Changes_Percent_Between_CIP_DF)){
  sum = 0
  for(col in 1:3){
    if(Percent_Means_List[col] < 0){
      if (Changes_Percent_Between_CIP_DF[row,col] < Percent_Means_List[col]){
        sum = sum + 1
      }
    }else{
      if (Changes_Percent_Between_CIP_DF[row,col] > Percent_Means_List[col]){
        sum = sum + 1
      }
    }
  }
  if (sum >= 2){
    Changes_Percent_Between_CIP_DF$CIP[row] <- 1
  }
}


sum(Changes_Percent_Between_CIP_DF$CIP)

# -------------------------------------------------------------------------------

# Set Data Here
Train_3_Jan_thru_Apr <- Train_3[Train_3$Date_Time >= "2021-01-21 00:00:00" & 
                                  Train_3$Date_Time <= "2021-03-29 02:00:00",]


Percent_Means_List_CIP <- as.numeric(colMeans(Changes_Percent_Between_CIP_DF[Changes_Percent_Between_CIP_DF$CIP == 1,]))

data <- Train_3_Jan_thru_Apr

# Calculate Percent Changes of the Variables

List_of_Percent_Changes <- c()
for (num in 2:4){
  model <- lm(data[,num] ~ data$Index)
  abline(model, col = 'red')
  slope <- as.numeric(model$coefficients[2])
  intercept <- as.numeric(model$coefficients[1])
  change <- slope * nrow(data)
  Start_Val <- (slope * data$Index[1] + intercept)
  Percent_Change <- change / Start_Val
  Percent_Change <- change / Start_Val
  List_of_Percent_Changes <- append(List_of_Percent_Changes,Percent_Change)
}

# Set Variables for the percent change
x_1 <- List_of_Percent_Changes[1]
x_2 <- List_of_Percent_Changes[2]
x_3 <- List_of_Percent_Changes[3]


# Calculate the total threshold variable
CIP_Variable <- ( 1/Percent_Means_List_CIP[1] * (x_1) 
                  + 1/Percent_Means_List_CIP[2]*(x_2) 
                  + 1/Percent_Means_List_CIP[3]*(x_3))


