# Have to run code in raw_to_clean_data file first

# Change Between CIP for Specific Flux

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

#-------------------------------------------------------------------------------
# If statements to add columns of dates for when the train is On & Off
i <- 1

# Replace "*_*" with Train number dataframe

# Date when train was turned on
if(i <= length(Train*_*_ON_Change$Start_Index)) {
  Train_On_Date <- data_ONOFF$Date_Time[Train*_*_ON_Change$Start_Index]
  Train*_*_ON_Change$Train_On_Date <- Train_On_Date
}
# Date when train was turned off
if(i <= length(Train*_*_ON_Change$End_Index)) {
  Train_Off_Date <- data_ONOFF$Date_Time[Train*_*_ON_Change$End_Index]
  Train*_*_ON_Change$Train_Off_Date <- Train_Off_Date
}
#-------------------------------------------------------------------------------

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
    #if (Change_Data$Time_ON[num] > 100) {
    plot(data_indices, list, main = "Train 3 Specific Flux Over Time",type = "l",col = colors[num], ylim = c(.075,.09), xlab = "Time (hours)", ylab = "Specific Flux")
    model <- lm(list ~ data_indices)
    abline(model, col = colors[num])
   # }

  }
  else{
   # if (Change_Data$Time_ON[num] > 100) {
    points(data_indices, list, type = "l", col = colors[num])
    model <- lm(list ~ data_indices)
    abline(model, col = colors[num])
   # }
   
  }
}

# Add legend displaying colors in order of oldest to newest
legend("topright", inset = .01, title = "Cycle Number (12 = most recent)",legend= c(1:12)
       ,fill = colors, horiz=TRUE,cex = 0.45)
 
