
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------

 
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
 
