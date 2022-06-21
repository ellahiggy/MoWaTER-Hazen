<<<<<<< HEAD
General <- HSMaster[, c("Date_Time", "T_f_CO", "TCF_a")]

vars <- c("Date_Time","Sp_Fl","Nt_DP","NCp","Per_R_a","P_f","ON_OFF","Index")


#Data for each variable by Train, as detailed above
Train_1 <- HSMaster[,c("Date_Time", "Sp_Fl_TR1", "Nt_DP_TR1", 
                       "NCp_TR1", "Per_R_a_TR1", "P_f_TR1","ON_TR1")]
Train_1$index <- 1:nrow(Train_1)
colnames(Train_1) <- vars

Train_2 <- HSMaster[,c("Date_Time", "Sp_Fl_TR2", "Nt_DP_TR2", 
                       "NCp_TR2", "Per_R_a_TR2", "P_f_TR2","ON_TR2")]
Train_2$index <- 1:nrow(Train_2)
colnames(Train_2) <- vars

Train_3 <- HSMaster[,c("Date_Time", "Sp_Fl_TR3", "Nt_DP_TR3", 
                       "NCp_TR3", "Per_R_a_TR3", "P_f_TR3","ON_TR3")]
Train_3$index <- 1:nrow(Train_3)
colnames(Train_3) <- vars

Train_4 <- HSMaster[,c("Date_Time", "Sp_Fl_TR4", "Nt_DP_TR4", 
                       "NCp_TR4", "Per_R_a_TR4", "P_f_TR4",'ON_TR4')]
Train_4$index <- 1:nrow(Train_4)
colnames(Train_4) <- vars

Train_5 <- HSMaster[,c("Date_Time", "Sp_Fl_TR5", "Nt_DP_TR5", 
                       "NCp_TR5", "Per_R_a_TR5", "P_f_TR5","ON_TR5")]
Train_5$index <- 1:nrow(Train_5)
colnames(Train_5) <- vars

## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------


# Add Train Number below and remove comment

# data_ONOFF <- Train_#

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
  Data_ON_Change$Time_ON[num] <- max(which(data_ONOFF$ON_Count == num)) - min(which(data_ONOFF$ON_Count == num))
}


# Add Train Number below and remove comment

Data_ON_Change$Time_of_CIP[1] <- 0
for (num in 2:nrow(Data_ON_Change)){
  Data_ON_Change$Time_of_CIP[num] <- Data_ON_Change$Start_Index[num] - Data_ON_Change$End_Index[num-1]
}


# Add Train Number below and remove comment

# Train#_ON_Change = Data_ON_Change  

# ------------------------------------------------------------------------------


Train3_ON_Change <- Train3_ON_Change[-c(5),]

for (num in 1:nrow(Train3_ON_Change)){
  x <- Train3_ON_Change$Time_ON[num] / 500
  data <- Train_3[Train_3$Index[Train3_ON_Change$Start_Index[num]:Train3_ON_Change$End_Index[num]],]
  list <- data$Sp_Fl
  data_indices <- c(Train3_ON_Change$Start_Index[num]:Train3_ON_Change$End_Index[num])
  data_indices <- data_indices - data_indices[1] + 1
  data_indices <- data_indices / x
  if (num == 1){
    plot(data_indices, list, main = "Specific FLux",type = "l",col = num, ylim = c(.075,.09))
  }
  else{
    points(data_indices, list, main = "Specific FLux", type = "l", col = num)
  }
}


# ------------------------------------------------------------------------------


Train3_ON_Change <- Train3_ON_Change[-c(5),]

for (num in 1:nrow(Train3_ON_Change)){
  data <- Train_3[Train_3$Index[Train3_ON_Change$Start_Index[num]:Train3_ON_Change$End_Index[num]],]
  list <- data$Sp_Fl
  y_adjust <- 1 - 
    
    x_scale <- Train3_ON_Change$Time_ON[num] / 500
  data_indices <- c(Train3_ON_Change$Start_Index[num]:Train3_ON_Change$End_Index[num])
  data_indices <- data_indices - data_indices[1] + 1
  data_indices <- data_indices / x_scale
  if (num == 1){
    plot(data_indices, list, main = "Specific FLux",type = "l",col = num, ylim = c(.075,.09))
  }
  else{
    points(data_indices, list, main = "Specific FLux", type = "l", col = num)
  }
}
=======

## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------

 
# ------------------------------------------------------------------------------
 
# Train 3 Specific Flux
 
Train3_ON_Change <- Train3_ON_Change[-c(5),]

list_of_change_var <- c()
list_of_hours

vecTimes <- paste(month(HSMaster$Date_Time[X$starting_locations]), "-" ,month(HSMaster$Date_Time[X$ending_locations]))



Change_Data <- Train3_ON_Change
Train_Data <- Train_3

for (num in 1:nrow(Change_Data)){
  data <- Train_Data[Train_Data$Index[Change_Data$Start_Index[num]:Change_Data$End_Index[num]],]
  list <- data$Sp_Fl
  data_indices <- c(Change_Data$Start_Index[num]:Change_Data$End_Index[num])
  data_indices <- data_indices - data_indices[1] + 1
  #data_indices <- data_indices / Change_Data$Time_ON[num]
  
  if (num == 1){
    if (Change_Data$Time_ON[num] > 100) {
       plot(data_indices, list, main = "Train 3 Specific Flux over Time",type = "l",col = num, ylim = c(.075,.09), xlab = "Standardized Time", ylab = "Specific Flux")
    model <- lm(list ~ data_indices)
    abline(model, col = num)
    list_of_change_var <- append(list_of_change_var,model$coefficients[2]*Change_Data$Time_ON[num])
    }
   
  }
  else{
    if (Change_Data$Time_ON[num] > 100) {
       points(data_indices, list, type = "l", col = num)
    model <- lm(list ~ data_indices)
    abline(model, col = num)
    list_of_change_var <- append(list_of_change_var,model$coefficients[2]*Change_Data$Time_ON[num])
    }
   
  }
}

barplot(-list_of_change_var)
 


# ------------------------------------------------------------------------------

# Train 3 Normalized Salt Passage
 
Train3_ON_Change <- Train3_ON_Change[-c(5),]

Change_Data <- Train3_ON_Change
Train_Data <- Train_3

for (num in 1:nrow(Change_Data)){
  data <- Train_Data[Train_Data$Index[Change_Data$Start_Index[num]:Change_Data$End_Index[num]],]
  list <- data$SP_n
  data_indices <- c(Change_Data$Start_Index[num]:Change_Data$End_Index[num])
  data_indices <- data_indices - data_indices[1] + 1
  #data_indices <- data_indices / Change_Data$Time_ON[num]
  
  if (num == 1){
    plot(data_indices, list, main = "Train 3 Normalized Salt Passage over Time",type = "l",col = num, ylim = c(1,1.3), xlab = "Standardized Time", ylab = "Normalized Salt Passage")
  }
  else{
    points(data_indices, list, type = "l", col = num)
  }
}




# ------------------------------------------------------------------------------

# Train 3 Specific Flux Adjusted Y

plot(Train_3$SP_n ~ Train_3$Date_Time)


Train3_ON_Change <- Train3_ON_Change[-c(5),]

Change_Data <- Train3_ON_Change
Train_Data <- Train_3

for (num in 1:nrow(Change_Data)){
  data <- Train_Data[Train_Data$Index[Change_Data$Start_Index[num]:Change_Data$End_Index[num]],]
  list <- data$Sp_Fl
  y_adjust <- .1 - list[1]
  list <- list + y_adjust
  x_scale <- Change_Data$Time_ON[num] / 1
  data_indices <- c(Change_Data$Start_Index[num]:Change_Data$End_Index[num])
  data_indices <- data_indices - data_indices[1] + 1
  data_indices <- data_indices / x_scale
  if (num == 1){
    plot(data_indices, list, main = "Specific FLux",type = "l",col = num, ylim = c(.095,.102))
  }
  else{
    points(data_indices, list, main = "Specific FLux", type = "l", col = num)
  }
}


# --------------

# ------------------------------------------------------------------------------

# Train 3 Net Driving Pressure

Train3_ON_Change <- Train3_ON_Change[-c(5),]

Change_Data <- Train3_ON_Change
Train_Data <- Train_3

for (num in 1:nrow(Change_Data)){
  x <- Change_Data$Time_ON[num] / 500
  data <- Train_Data[Train_Data$Index[Change_Data$Start_Index[num]:Change_Data$End_Index[num]],]
  list <- data$Nt_DP
  data_indices <- c(Change_Data$Start_Index[num]:Change_Data$End_Index[num])
  data_indices <- data_indices - data_indices[1] + 1
  data_indices <- data_indices / x
  if (num == 1){
    plot(data_indices, list, main = "Specific FLux",type = "l",col = num, ylim = c(90,180))
  }
  else{
    points(data_indices, list, main = "Specific FLux", type = "l", col = num)
  }
}




>>>>>>> a1e9fc5924709a707c9655dcd1025467ee90e77b
