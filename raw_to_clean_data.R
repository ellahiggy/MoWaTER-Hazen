# -------------------------------------------------------------------------
# MOWATER 
# Hazen and Sawyer Team
# Data Cleanup
# Henry Burch, PJ Williams, Ella Higginbotham, Lauren Walker, Cyril Pillai
# 
# Narrowed Variables List, x = train number
# Time Log - Date_Time
# Percent Salt Passage - SP_a_TRx
# Specific Flux - Sp_Fl_TRx
# Net Driving pressure - Nt_DP_TRx 
# Net Driving Pressure at Actual Conditions (Train) - NetDp_a_TRx
# Normalized permeate conductivity - NCp_TRx
# Permeate Conductivity - C_p_TRx
# T Concentrate Conductivity - C_c_TRx
# % Recovery - Per_R_a_TR1
# Recovery at Actual Conditions - R_a_TRx
# Feedwater Pressure - P_f_TRx
# Online Status - ON_TRx
# Normalized DIfferential Pressure - DP_n_TRx
# Feedwater Temperature - T_f_CO
# Temperature Correction Factor at Actual Conditions - TCF_a
#  -------------------------------------------------------------------------


# Please change the path to one that includes the following files:
# "Sample_Membrane_Treatment_Plant_Data" spreadsheet
# HSMaster - Hazen and Sawyer.rda"
setwd("/Users/cyrilpillai/Desktop/Data_Processing")
load("HSMaster - Hazen and Sawyer.rda")

#Data general to all trains
General <- HSMaster[, c("Date_Time", "T_f_CO", "TCF_a")]

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


#Function to get time periods when each train is on and off


train_ONOFF <- function(Train_X)
{
  if(!is.data.frame(Train_X)) stop("Train_X must be a df")
  
  data_ONOFF <- Train_X
  
  data_ONOFF$ON_Count <- 0
  #data_ONOFF[is.na(data_ONOFF)] = 0
  #data_ONOFF[is.null(data_ONOFF)] = 0
  off <- 0
  on <- 0
  
  len <- nrow(data_ONOFF)
  
  for (num in 1:len) {
    if (data_ONOFF$ON_OFF[num] == 0|is.na(data_ONOFF$ON_OFF[num])){
      data_ONOFF$ON_Count[num] <- 0
    }else {
      if (num == 1){
        on <<- on + 1
      }else if (data_ONOFF$ON_OFF[num-1] == 0|is.na(data_ONOFF$ON_OFF[num-1])){
        on <<- on + 1
      }
      data_ONOFF$ON_Count[num] <- on
    }
  }
  
  Data_ON_Number <-  unique(data_ONOFF$ON_Count)
  Data_ON_Number <- Data_ON_Number[Data_ON_Number != 0]; # without elements that are "b"
  
  Data_ON_Change <- as.data.frame(matrix(nrow = max(Data_ON_Number), ncol = 3))
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


Train5_ON_Change <- train_ONOFF(Train_5)












# Variables for EDA Plots

# Variables for five train plots of specific flux plots with values for train = OFF
na_values_1_spfl <- as.integer(is.na(Train_1$Sp_Fl_TR1))*mean(Train_1$Sp_Fl_TR1,na.rm=TRUE) 
na_time_1_spfl <- Train_1$Date_Time[is.na(Train_1$Sp_Fl_TR1)]

na_values_2_spfl <- as.integer(is.na(Train_2$Sp_Fl_TR2))*mean(Train_2$Sp_Fl_TR2,na.rm=TRUE) 
na_time_2_spfl <- Train_2$Date_Time[is.na(Train_2$Sp_Fl_TR2)]

na_values_3_spfl <- as.integer(is.na(Train_3$Sp_Fl_TR3))*mean(Train_3$Sp_Fl_TR3,na.rm=TRUE) 
na_time_3_spfl <- Train_3$Date_Time[is.na(Train_3$Sp_Fl_TR3)]

na_values_4_spfl <- as.integer(is.na(Train_4$Sp_Fl_TR4))*mean(Train_4$Sp_Fl_TR4,na.rm=TRUE) 
na_time_4_spfl <- Train_4$Date_Time[is.na(Train_4$Sp_Fl_TR4)]

na_values_5_spfl <- as.integer(is.na(Train_5$Sp_Fl_TR5))*mean(Train_5$Sp_Fl_TR5,na.rm=TRUE) 
na_time_5_spfl <- Train_5$Date_Time[is.na(Train_5$Sp_Fl_TR5)]


