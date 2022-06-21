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


#Please change the path to one that includes the following files:
# "Sample_Membrane_Treatment_Plant_Data" spreadsheet
# HSMaster - Hazen and Sawyer.rda"
setwd("/Users/cyrilpillai/Desktop/Data_Processing")
load("HSMaster - Hazen and Sawyer.rda")

#Data general to all trains
General <- HSMaster[, c("Date_Time", "T_f_CO", "TCF_a")]

vars <- c("Date_Time","Sp_Fl","Nt_DP","NCp","Per_R_a","P_f","SP_n","ON_OFF","Index")
#column name standardization


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

#clean df with all variables we created 
#list of dataframes
#save as .rda object


