
# General <- HSMaster[, c("Date_Time", "T_f_CO", "TCF_a")]
# 
# vars <- c("Date_Time","Sp_Fl","Nt_DP","P_f","DP_n","NCp","Index")
# 
# 
# #Data for each variable by Train, as detailed above
# Train_1 <- HSMaster[,c("Date_Time", "Sp_Fl_TR1", "Nt_DP_TR1","DP_n_TR1","NCp_TR1", 
#                        "P_f_TR1")]
# Train_1$index <- 1:nrow(Train_1)
# colnames(Train_1) <- vars
# 
# Train_2 <- HSMaster[,c("Date_Time", "Sp_Fl_TR2", "Nt_DP_TR2", "DP_n_TR2","NCp_TR2",
#                        "P_f_TR2")]
# Train_2$index <- 1:nrow(Train_2)
# colnames(Train_2) <- vars
# 
# Train_3 <- HSMaster[,c("Date_Time", "Sp_Fl_TR3", "Nt_DP_TR3", "DP_n_TR3","NCp_TR3",
#                        "P_f_TR3")]
# Train_3$index <- 1:nrow(Train_3)
# colnames(Train_3) <- vars
# 
# Train_4 <- HSMaster[,c("Date_Time", "Sp_Fl_TR4", "Nt_DP_TR4", "DP_n_TR4","NCp_TR4",
#                        "P_f_TR4")]
# Train_4$index <- 1:nrow(Train_4)
# colnames(Train_4) <- vars
# 
# Train_5 <- HSMaster[,c("Date_Time", "Sp_Fl_TR5", "Nt_DP_TR5", "DP_n_TR5","NCp_TR5",
#                        "P_f_TR5")]
# Train_5$index <- 1:nrow(Train_5)
# colnames(Train_5) <- vars

# ------------------------------------------------------------------------------

# Train_3_Oct_thru_Nov <- Train_3[Train_3$Date_Time >= "2020-10-15 00:00:00" & 
#                                   Train_3$Date_Time <= "2020-11-08 00:00:00",]
# 
# Train_3_Nov_thru_Jan <- Train_3[Train_3$Date_Time >= "2020-11-17 00:00:00" & 
#                                   Train_3$Date_Time <= "2021-01-19 00:00:00",]
# 
# Train_3_Jan_thru_Apr <- Train_3[Train_3$Date_Time >= "2021-01-21 00:00:00" & 
#                                   Train_3$Date_Time <= "2021-04-29 02:00:00",]
# 
# 
# Train_5_Apr <- Train_5[Train_5$Date_Time >= "2021-04-05 00:00:00" & 
#                          Train_5$Date_Time <= "2021-04-27 02:00:00",]

#-------------------------------------------------------------------------------
# Create new data frame to append to with the Change of 4 selected variables 
Changes_Between_CIP_DF <- as.data.frame(matrix(ncol= 3))
colnames(Changes_Between_CIP_DF) <- c("Sp_Fl","Nt_DP","P_f")

# Create new data frame to append to with the Percent Change of 4 selected variables 
Changes_Percent_Between_CIP_DF <- as.data.frame(matrix(ncol= 8))
colnames(Changes_Percent_Between_CIP_DF) <- c("Sp_Fl","Nt_DP","P_f","DP_n","NCp","Start_Index","End_Index","Train_Num")

df_index <- 1

Changes_Percent_Between_CIP_DF_Test <- as.data.frame(matrix(ncol= 8))
colnames(Changes_Percent_Between_CIP_DF_Test) <- c("Sp_Fl","Nt_DP","P_f","DP_n","NCp","Start_Index","End_Index","Train_Num")


# ------------------------------------------------------------------------------

### Train 3 --------------

for (value in 2:nrow(Train3_ON_Change)){ #Change Train Number
  First_Index <- Train3_ON_Change$Start_Index[value] #Change Train Number
  Last_Index <- Train3_ON_Change$End_Index[value] #Change Train Number
  Train_Data_ON <- Train_3[Train_3$Index >= First_Index & #Change Train Number
                             Train_3$Index <= Last_Index,] #Change Train Number
  List_of_Changes <- c()
  List_of_Percent_Changes <- c()
  
  for (num in 2:6){
    #plot(Train_Data_ON[,num] ~ Train_Data_ON$Index)
    model <- lm(Train_Data_ON[,num] ~ Train_Data_ON$Index)
    #abline(model, col = 'red')
    slope <- as.numeric(model$coefficients[2])
    intercept <- as.numeric(model$coefficients[1])
    #time <- as.integer(difftime("2021-04-29 02:00:00","2021-01-21 00:00:00",units = c("hours")))
    change <- slope * nrow(Train_Data_ON)
    List_of_Changes <- append(List_of_Changes,change)
    Start_Val <- (slope * Train_Data_ON$Index[1] + intercept)
    Percent_Change <- change / Start_Val
    List_of_Percent_Changes <- append(List_of_Percent_Changes,Percent_Change)
  }
  Start_End <- c(First_Index,Last_Index,3) #Change Train Number
  Row <- append(List_of_Percent_Changes,Start_End)
  Changes_Percent_Between_CIP_DF[df_index,] <- Row
  df_index <<- df_index + 1
}

### Train 4 --------------

for (value in 2:nrow(Train4_ON_Change)){ #Change Train Number
  First_Index <- Train4_ON_Change$Start_Index[value] #Change Train Number
  Last_Index <- Train4_ON_Change$End_Index[value] #Change Train Number
  Train_Data_ON <- Train_4[Train_4$Index >= First_Index & #Change Train Number
                             Train_4$Index <= Last_Index,] #Change Train Number
  List_of_Changes <- c()
  List_of_Percent_Changes <- c()
  
  for (num in 2:6){
    #plot(Train_Data_ON[,num] ~ Train_Data_ON$Index)
    model <- lm(Train_Data_ON[,num] ~ Train_Data_ON$Index)
    #abline(model, col = 'red')
    slope <- as.numeric(model$coefficients[2])
    intercept <- as.numeric(model$coefficients[1])
    #time <- as.integer(difftime("2021-04-29 02:00:00","2021-01-21 00:00:00",units = c("hours")))
    change <- slope * nrow(Train_Data_ON)
    List_of_Changes <- append(List_of_Changes,change)
    Start_Val <- (slope * Train_Data_ON$Index[1] + intercept)
    Percent_Change <- change / Start_Val
    List_of_Percent_Changes <- append(List_of_Percent_Changes,Percent_Change)
  }
  Start_End <- c(First_Index,Last_Index,4) #Change Train Number
  Row <- append(List_of_Percent_Changes,Start_End)
  Changes_Percent_Between_CIP_DF[df_index,] <- Row
  df_index <<- df_index + 1
}

### Train 5 --------------

for (value in 2:nrow(Train5_ON_Change)){ #Change Train Number
  First_Index <- Train5_ON_Change$Start_Index[value] #Change Train Number
  Last_Index <- Train5_ON_Change$End_Index[value] #Change Train Number
  Train_Data_ON <- Train_5[Train_5$Index >= First_Index & #Change Train Number
                             Train_5$Index <= Last_Index,] #Change Train Number
  List_of_Changes <- c()
  List_of_Percent_Changes <- c()
  
  for (num in 2:6){
    #plot(Train_Data_ON[,num] ~ Train_Data_ON$Index)
    model <- lm(Train_Data_ON[,num] ~ Train_Data_ON$Index)
    #abline(model, col = 'red')
    slope <- as.numeric(model$coefficients[2])
    intercept <- as.numeric(model$coefficients[1])
    #time <- as.integer(difftime("2021-04-29 02:00:00","2021-01-21 00:00:00",units = c("hours")))
    change <- slope * nrow(Train_Data_ON)
    List_of_Changes <- append(List_of_Changes,change)
    Start_Val <- (slope * Train_Data_ON$Index[1] + intercept)
    Percent_Change <- change / Start_Val
    List_of_Percent_Changes <- append(List_of_Percent_Changes,Percent_Change)
  }
  Start_End <- c(First_Index,Last_Index,5) #Change Train Number
  Row <- append(List_of_Percent_Changes,Start_End)
  Changes_Percent_Between_CIP_DF[df_index,] <- Row
  df_index <<- df_index + 1
}

#---------------------------------------------------------------------------------

Percent_Means_List <- as.numeric(colMeans(Changes_Percent_Between_CIP_DF))
Changes_Percent_Between_CIP_DF$CIP <- 0

for (row in 1:nrow(Changes_Percent_Between_CIP_DF)){
  sum = 0
  for(col in 1:5){
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
  if (sum >= 4){
    Changes_Percent_Between_CIP_DF$CIP[row] <- 1
  }
}


sum(Changes_Percent_Between_CIP_DF$CIP)

# -------------------------------------------------------------------------------

### Manual Model ------------

# Set Data Here
Train_3_Jan_thru_Apr <- Train_3[Train_3$Date_Time >= "2021-01-21 00:00:00" & 
                                  Train_3$Date_Time <= "2021-04-29 02:00:00",]


Percent_Means_List_CIP <- as.numeric(colMeans(Changes_Percent_Between_CIP_DF[Changes_Percent_Between_CIP_DF$CIP == 1,]))

data <- Train_3_Jan_thru_Apr

# Calculate Percent Changes of the Variables

List_of_Percent_Changes <- c()
for (num in 2:4){
  model <- lm(data[,num] ~ data$Index)
  #abline(model, col = 'red')
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



Percent_Means_List
# Calculate the total threshold variable
CIP_Variable <- ( 1/Percent_Means_List_CIP[1] * (x_1) 
                  + 1/Percent_Means_List_CIP[2]*(x_2) 
                  + 1/Percent_Means_List_CIP[3]*(x_3))

#-----------------------------------------------------------------------

### linear model --------------

# 
# 
# 
# 


# 
# 
# model <- lm(training_data$CIP ~
#               training_data$Sp_Fl +
#               training_data$Nt_DP + 
#               training_data$P_f   +
#               training_data$DP_n  +
#               training_data$NCp)
# 
# 
# model$coefficients[2]
# model$coefficients[3]
# model$coefficients[4]
# model$coefficients[5]
# model$coefficients[6]
# 
# for(i in 1:nrow(testing_data)){
#   output <- model$coefficients[2] * (testing_data$Sp_Fl[i]) +
#     model$coefficients[3] * (testing_data$Nt_DP[i]) +
#     model$coefficients[4] * (testing_data$P_f[i])  +
#     model$coefficients[5] * (testing_data$DP_n[i])  +
#     model$coefficients[6] * (testing_data$NCp[i])
#     testing_data$CIP_Predict[i] <- round(output,2)
# }
# 
# lm_test_output <- predict(model, 
#                            testing_data[,1:5], type = "response")


# ------------------------------------------------------------------------------

### Log Model



summary(Log_model)



# for(i in 1:nrow(testing_data))
# {
#   num <- exp(Log_model$coefficients[0] + 
#       Log_model$coefficients[1] * testing_data$Sp_Fl[i] + 
#       Log_model$coefficients[2] * testing_data$Nt_DP[i] +
#       Log_model$coefficients[3] * testing_data$P_f[i] +
#       Log_model$coefficients[4] * testing_data$DP_n[i] +
#       Log_model$coefficients[5] * testing_data$NCp[i])
#   
#   testing_data$CIP_Predict[i] <- round(num/(1+num), 2)
# }


library(caTools)
set.seed(0)
sample <- sample.split(Changes_Percent_Between_CIP_DF$CIP, SplitRatio = 0.7)
training_data <- subset(Changes_Percent_Between_CIP_DF, sample == TRUE)
testing_data <- subset(Changes_Percent_Between_CIP_DF, sample == FALSE)

Log_model <- glm(CIP ~ 
                   Sp_Fl +
                   Nt_DP +
                   P_f   +
                   DP_n  +
                   NCp, 
                 data = training_data,
                 family=binomial)

predict_reg <- predict.glm(Log_model,
                           newdata = testing_data[,1:5], type = "response")
print(predict_reg)

testing_data$CIP_Predict <- round(predict_reg, 2)





for(i in 1:nrow(testing_data))
{
  
  
  
  
}

    
sum(Changes_Percent_Between_CIP_DF$CIP)

(0.74 + 0.55 + 1 + 0.91)/4




#--------------------------------------------------------------------------------
# Specific Flux drops 10%
# Net Driving Pressure increases 4%
# Feedwater Pressure increases 3.5%
# 
# ^^ = CIP

summary(model)
# 1/-10.24168
# 
# (-0.09764023 * 100)% -->   
# 
# 
# -10.24*x1 + 3.86*x2 + 3.52*x3 = Response
# 
# x2 = 0, x3 = 0
# 
# -10.24*x1 = Response
# 
# 1/-10.24168
# 1/3.86
# 1/3



#--------------------------------------------------------------------------------
# Changes_Percent_Between_CIP_DF_Test <- Changes_Percent_Between_CIP_DF
# Changes_Percent_Between_CIP_DF_Test$CIP_Predict <- 0
# myvars <- c("Sp_Fl", "Nt_DP", "P_f")
# Changes_Percent_Between_CIP_DF_Test_Vars <- Changes_Percent_Between_CIP_DF_Test[myvars]
# 
# ?subset()

# for (i in 1:nrow(Changes_Percent_Between_CIP_DF_Test_Vars))
# {
#   Changes_Percent_Between_CIP_DF_Test$CIP_Predict[i] <- predict(Log_model, 
#                                                                 new_data = Changes_Percent_Between_CIP_DF_Test_Vars, type = "response")
# }



