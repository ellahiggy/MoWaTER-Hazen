
#Data general to all trains
General <- HSMaster[, c("Date_Time", "T_f_CO", "TCF_a")]

vars <- c("Date_Time","Sp_Fl","Nt_DP","NCp","Per_R_a","P_f","ON_OFF")


#Data for each variable by Train, as detailed above
Train_1 <- HSMaster[,c("Date_Time", "Sp_Fl_TR1", "Nt_DP_TR1", 
                       "NCp_TR1", "Per_R_a_TR1", "P_f_TR1","ON_TR1")]
colnames(Train_1) <- vars

Train_2 <- HSMaster[,c("Date_Time", "Sp_Fl_TR2", "Nt_DP_TR2", 
                       "NCp_TR2", "Per_R_a_TR2", "P_f_TR2","ON_TR2")]
colnames(Train_2) <- vars

Train_3 <- HSMaster[,c("Date_Time", "Sp_Fl_TR3", "Nt_DP_TR3", 
                       "NCp_TR3", "Per_R_a_TR3", "P_f_TR3","ON_TR3")]
colnames(Train_3) <- vars

Train_4 <- HSMaster[,c("Date_Time", "Sp_Fl_TR4", "Nt_DP_TR4", 
                       "NCp_TR4", "Per_R_a_TR4", "P_f_TR4",'ON_TR4')]
colnames(Train_4) <- vars

Train_5 <- HSMaster[,c("Date_Time", "Sp_Fl_TR5", "Nt_DP_TR5", 
                       "NCp_TR5", "Per_R_a_TR5", "P_f_TR5","ON_TR5")]
colnames(Train_5) <- vars

## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------


# Add Train Number below and remove comment

#data_ONOFF <- Train_#

data_ONOFF$ON_Count <- 0

#data_ONOFF[is.na(data_ONOFF)] = 0
#data_ONOFF[is.null(data_ONOFF)] = 0

off <- 0
on <- 0

len <- nrow(data_ONOFF)

for (num in 1:len) {
  if (data_ONOFF$ON_OFF[num] == 0|is.na(data_ONOFF$ON_OFF[num])){
    data_ONOFF$ON_Count[num] <- 0
  }
  else {
    if (num == 1){
      on <<- on + 1
    }
    else if (data_ONOFF$ON_OFF[num-1] == 0|is.na(data_ONOFF$ON_OFF[num-1])){
      on <<- on + 1
    }
    data_ONOFF$ON_Count[num] <- on
  }
}

Data_ON_Number <-  unique(data_ONOFF$ON_Count)
Data_ON_Number <- Data_ON_Number[Data_ON_Number != 0]; # without elements that are "b"
Data_ON_Number

Data_ON_Change <- as.data.frame(matrix(nrow = max(Data_ON_Number), ncol = 3))
colnames(Data_ON_Change) <- c("ON_Count","Start_Index","End_Index")

for (num in Data_ON_Number){
  Data_ON_Change$ON_Count[num] <- num
  Data_ON_Change$Start_Index[num] <- min(which(data_ONOFF$ON_Count == num))
  Data_ON_Change$End_Index[num] <- max(which(data_ONOFF$ON_Count == num))
}

# Add Train Number below and remove comment

# Train#_ON_Change = Data_ON_Change 

# ------------------------------------------------------------------------------

