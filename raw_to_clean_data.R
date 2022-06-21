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


#Data for each variable by Train, as detailed above
Train_1 <-
             HSMaster[,c("Date_Time", "SP_n_TR1", "Sp_Fl_TR1", "Nt_DP_TR1", 
                         "NetDp_a_TR1", "NCp_TR1", "C_p_TR1", "C_c_TR1", 
                         "Per_R_a_TR1", "R_a_TR1", "P_f_TR1", "ON_TR1", 
                         "DP_n_TR1")]

Train_2 <-
            HSMaster[,c("Date_Time", "SP_n_TR2", "Sp_Fl_TR2", "Nt_DP_TR2", 
                        "NetDp_a_TR2", "NCp_TR2", "C_p_TR2", "C_c_TR2", 
                        "Per_R_a_TR2", "R_a_TR2", "P_f_TR2", "ON_TR2", 
                        "DP_n_TR2")]
Train_3 <-
            HSMaster[,c("Date_Time", "SP_n_TR3", "Sp_Fl_TR3", "Nt_DP_TR3", 
                        "NetDp_a_TR3", "NCp_TR3", "C_p_TR3", "C_c_TR3", 
                        "Per_R_a_TR3", "R_a_TR3", "P_f_TR3", "ON_TR3", 
                        "DP_n_TR3")]
Train_4 <-
            HSMaster[,c("Date_Time", "SP_n_TR4", "Sp_Fl_TR4", "Nt_DP_TR4", 
                        "NetDp_a_TR4", "NCp_TR4", "C_p_TR4", "C_c_TR4", 
                        "Per_R_a_TR4", "R_a_TR4", "P_f_TR4", "ON_TR4", 
                        "DP_n_TR4")]
Train_5 <-
            HSMaster[,c("Date_Time", "SP_n_TR5", "Sp_Fl_TR5", "Nt_DP_TR5", 
                        "NetDp_a_TR5", "NCp_TR5", "C_p_TR5", "C_c_TR5", 
                        "Per_R_a_TR5", "R_a_TR5", "P_f_TR5", "ON_TR5", 
                        "DP_n_TR5")]

# sum(is.null(Train_1)) # 0
# sum(is.null(Train_2)) # 0
# sum(is.null(Train_3)) # 0
# sum(is.null(Train_4)) # 0
# sum(is.null(Train_5)) # 0
# No null values in the dataset, all done!


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




