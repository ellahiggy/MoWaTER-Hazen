\name{MoWaTER_Hazen} % replace with your project name
\alias{MoWaTER_Hazen} % replace with your project name

\docType{data} % don't change

\title{MoWaTER_Hazen_and_Sawyer} % replace with your project full name

\description{
This data came from Hazen and Sawyer, an environmental engineering firm who focuses on providing their clients with clean drinking water. This specific data came from one of Hazen and Sawyer's confidential clients and contained over 200 variables with more than 8000 observations with information on measurements that the client used to determine when a clean in place (CIP) process should occur for the filtration system. However, the 200 variables are separated between the five trains contained in filtration system; therefore, there are approximately 30 repeated variables for each train. We used a linear regression model with our response variable being the change in percent between CIPs for each predictor variable. The predictor variables were specific flux, normalized permeate conductivity, net driving pressure, feedwater pressure, and normalized differential pressure. Additionally, we used multiple cleaned data frames that contained data specific for each of the five trains.
}


\usage{
\code{load("Hazen_Sawyer_Data_Clean.rda")} #- loads into working environment
}

\format{
The data is a list of 10 dataframes:

\strong{(1) Dataframes 1-5 --> (8376 rows and 9 columns)}

Data per Train (1-5) of the following 9 variables for the 8376 records in the study. 

\describe{
\item{\code{Date_Time}}{A measure of the date and its corresponding time. (Year-Month-Day; Hour-Min-Sec)}
\item{\code{Sp_Fl}}{Specific Flux}
\item{\code{Nt_DP}}{Net Driving Pressure - A measurement of the pressure difference between influent and effluent water.}
\item{\code{NCp}}{Normalized Permeate Conductivity - A measurement of the conductivity of the water that leaves the filtration system. The measurement is normalized based on a calculation done by Hazen and Sawyer's client.}
\item{\code{Per_R_a}}{Percent Recovery - A measurement of the percent of water recovered after being filtered through the train systems}
\item{\code{P_f}}{Feedwater Pressure - A measurement of the amount of water that initially enters the system.}
\item{\code{SP_n}}{Normalized Salt Passage - A measurement of the amount of salt that passes through the filtration system.}
\item{\code{ON_OFF}}{Train On/Off status - A binary measurement to determine whether the trains are on (represented by 1) or off (represented by 0).}
\item{\code{Index}}{Observations indexed by hour that assigns a number to each row. Helps with separating CIPs based on index number.} 



\strong{(2) Dataframes 6-10 --> (12-22 rows, 5 columns)}

Data per Train (1-5) of the periods of time the train is on, consisting of a varying number of observations of the following 5 variables. 


\describe{
\item{\code{ON_COUNT}}{Running count of the times which the train is on}
\item{\code{Start_Index}}{The index immediately when the train turns on after a period of inactivity}
\item{\code{End_Index}}{The index immediately when the train turns off after a period of activity}
\item{\code{Time_ON}}{The durations the train was consecutively on for}
\item{\code{Time_of_CIP}}{The durations the train was conescutively off for}
}

}


\source{
\cr
\strong{Source}

Data is provided by Hazen and Sawyer
Contacts: 

Javad Roostaei - JRoostaei@hazenandsawyer.com 

Ben Stanford - BStanford@hazenandsawyer.com
% Links to relevant information
}

\examples{
\cr 
\strong{Examples}
\cr

\code{load("Hazen_Sawyer_Data_Clean.rda")}


# ------------------------------------------------------------------------------
# Create a plot with the Specific Flux values over each period Train 3 is on. 

data_ONOFF <- Train_3
data_ONOFF$ON_Count <- 0
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

# If statements to add columns of dates for when the train is On & Off
i <- 1
# Replace "*_*" with Train number dataframe
# Date when train was turned on
if(i <- length(Train3_ON_Change$Start_Index)) {
  Train_On_Date <- data_ONOFF$Date_Time[Train3_ON_Change$Start_Index]
  Train3_ON_Change$Train_On_Date <- Train_On_Date
}
# Date when train was turned off
if(i <= length(Train3_ON_Change$End_Index)) {
  Train_Off_Date <- data_ONOFF$Date_Time[Train3_ON_Change$End_Index]
  Train3_ON_Change$Train_Off_Date <- Train_Off_Date
}

### Train 3 Specific Flux
# Create list of colors for our plot
library(viridis)
colors <- viridis(12)
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
    plot(data_indices, list, main = "Train 3 Specific Flux Over Time",type = "l",
    col = colors[num], ylim = c(.075,.09), xlab = "Time (hours)", ylab = "Specific Flux")
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

#-------------------------------------------------------------------------------
\cr

}

%\keyword{datasets}
























