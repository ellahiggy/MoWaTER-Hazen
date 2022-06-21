
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

## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------


# Add Train Number below and remove comment

data_ONOFF <- Train_3

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

Train3_ON_Change = Data_ON_Change  

 
 # ------------------------------------------------------------------------------
 
### Train 3 Specific Flux

# Create list of colors for our plot
library(viridis)

colors <- viridis(12)

# Remove the fifth row from the data becasue of an error
Train3_ON_Change <- Train3_ON_Change[-c(5),]

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
    if (Change_Data$Time_ON[num] > 100) {
    plot(data_indices, list, main = "Train 3 Specific Flux Over Time",type = "l",col = colors[num], ylim = c(.075,.09), xlab = "Standardized Time (hours)", ylab = "Specific Flux")
    model <- lm(list ~ data_indices)
    abline(model, col = colors[num])
    }
   
  }
  else{
    if (Change_Data$Time_ON[num] > 100) {
    points(data_indices, list, type = "l", col = colors[num])
    model <- lm(list ~ data_indices)
    abline(model, col = colors[num])
    }
   
  }
}

# Add legend displaying colors in order of oldest to newest
legend("topright", inset = .01, title = "Cycle Number (12 = most recent)",legend= c(1:12)
       ,fill = colors, horiz=TRUE,cex = 0.45)
 
