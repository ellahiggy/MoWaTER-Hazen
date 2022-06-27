# -------------------------------------------------------------------------
# MOWATER 
# Hazen and Sawyer Team
# Data Cleanup
# Henry Burch, PJ Williams, Ella Higginbotham, Lauren Walker, Cyril Pillai
# 
# Narrowed Variables List, x = train number
# Time Log - Date_Time
# Specific Flux - Sp_Fl_TRx
# Net Driving pressure - Nt_DP_TRx 
# Normalized permeate conductivity - NCp_TRx
# % Recovery - Per_R_a_TR1
# Feedwater Pressure - P_f_TRx
# Percent Salt Passage - SP_a_TRx
# Online Status - ON_TRx
#  -------------------------------------------------------------------------


# Please change the path to one that includes the following files:
# "Sample_Membrane_Treatment_Plant_Data" spreadsheet
# HSMaster - Hazen and Sawyer.rda"
setwd("/Users/cyrilpillai/Desktop/Data_Processing")
load("HSMaster - Hazen and Sawyer.rda")

vars <- c("Date_Time","Sp_Fl","Nt_DP","NCp","Per_R_a","P_f","SP_n","ON_OFF","Index")


#Data for each variable by Train, as detailed above
Train_1 <- HSMaster[,c("Date_Time", "Sp_Fl_TR1", "Nt_DP_TR1", 
                       "NCp_TR1", "Per_R_a_TR1", "P_f_TR1","SP_n_TR1","ON_TR1")]
Train_1$index <- 1:nrow(Train_1)
colnames(Train_1) <- vars

Train_2 <- HSMaster[,c("Date_Time", "Sp_Fl_TR2", "Nt_DP_TR2", 
                       "NCp_TR2", "Per_R_a_TR2", "P_f_TR2","SP_n_TR2","ON_TR2")]
Train_2$index <- 1:nrow(Train_2)
colnames(Train_2) <- vars

Train_3 <- HSMaster[,c("Date_Time", "Sp_Fl_TR3", "Nt_DP_TR3", 
                       "NCp_TR3", "Per_R_a_TR3", "P_f_TR3","SP_n_TR3","ON_TR3")]
Train_3$index <- 1:nrow(Train_3)
colnames(Train_3) <- vars

Train_4 <- HSMaster[,c("Date_Time", "Sp_Fl_TR4", "Nt_DP_TR4", 
                       "NCp_TR4", "Per_R_a_TR4", "P_f_TR4","SP_n_TR4",'ON_TR4')]
Train_4$index <- 1:nrow(Train_4)
colnames(Train_4) <- vars

Train_5 <- HSMaster[,c("Date_Time", "Sp_Fl_TR5", "Nt_DP_TR5", 
                       "NCp_TR5", "Per_R_a_TR5", "P_f_TR5","SP_n_TR5","ON_TR5")]
Train_5$index <- 1:nrow(Train_5)
colnames(Train_5) <- vars


# sum(is.null(Train_1)) # 0
# sum(is.null(Train_2)) # 0
# sum(is.null(Train_3)) # 0
# sum(is.null(Train_4)) # 0
# sum(is.null(Train_5)) # 0
# No null values in the dataset, all done!


#Function to get time periods when a train is on and off
train_ONOFF <- function(number)
{
  
  Trains <- list(Train_1, Train_2, Train_3, Train_4, Train_5)
  
  data_ONOFF <- Trains[[number]]
  
  data_ONOFF$ON_Count <- 0
  #data_ONOFF[is.na(data_ONOFF)] = 0
  #data_ONOFF[is.null(data_ONOFF)] = 0

  
  len <- nrow(data_ONOFF)
  
  off <- 0
  on <- 0
  
  
  for (num in 1:len) {
    
    if (data_ONOFF$ON_OFF[num] == 0|is.na(data_ONOFF$ON_OFF[num])){
      data_ONOFF$ON_Count[num] <- 0
    }else {
      if (num == 1){
        on <- on + 1
      }else if (data_ONOFF$ON_OFF[num-1] == 0|is.na(data_ONOFF$ON_OFF[num-1])){
        on <- on + 1
      }
      data_ONOFF$ON_Count[num] <- on
    }
  }
  
  Data_ON_Number <-  unique(data_ONOFF$ON_Count)
  
  Data_ON_Number <- Data_ON_Number[Data_ON_Number != 0]; # without elements that are "b"
  
  max_ON <- max(Data_ON_Number)
  
  Data_ON_Change <- as.data.frame(matrix(nrow = max_ON, ncol = 3))
  colnames(Data_ON_Change) <- c("ON_Count","Start_Index","End_Index")

  for (num in Data_ON_Number){
    Data_ON_Change$ON_Count[num] <- num
    Data_ON_Change$Start_Index[num] <- min(which(data_ONOFF$ON_Count == num))
    Data_ON_Change$End_Index[num] <- max(which(data_ONOFF$ON_Count == num))
    Data_ON_Change$Time_ON[num] <- max(which(data_ONOFF$ON_Count == num)) - min(which(data_ONOFF$ON_Count == num))
  }

  Data_ON_Change$Time_of_CIP[1] <- 0
  for (num in 2:nrow(Data_ON_Change)){
    Data_ON_Change$Time_of_CIP[num] <- Data_ON_Change$Start_Index[num] - Data_ON_Change$End_Index[num-1]
  }

  Data_ON_Change <- subset(Data_ON_Change, Time_ON > 1)
  return(Data_ON_Change)
}


#data frames with intervals in which each train is on
Train1_ON_Change <- train_ONOFF(1)
Train2_ON_Change <- train_ONOFF(2)
Train3_ON_Change <- train_ONOFF(3)
Train4_ON_Change <- train_ONOFF(4)
Train5_ON_Change <- train_ONOFF(5)


save(Train_1, Train_2, Train_3, Train_4, Train_5, 
     Train1_ON_Change, Train2_ON_Change, Train3_ON_Change,
     Train4_ON_Change, Train5_ON_Change,
     file = "Hazen_Sawyer_Data_Clean.rda")

