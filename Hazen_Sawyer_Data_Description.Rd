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
\item{\code{Sp_Fl}}{Specific Flux - A measurement used of the rate at which water flows through a reverse osmosis membrane and is directly proportional to temperature and pressure.}
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
\item{\code{ON_COUNT}}{Running count of the times which the train is on. (in hours)}
\item{\code{Start_Index}}{The index immediately when the train turns on after a period of inactivity or presumed CIP. (in hours)}
\item{\code{End_Index}}{The index immediately when the train turns off after a period of activity. (in hours)}
\item{\code{Time_ON}}{The durations the train was consecutively on for. (in hours).}
\item{\code{Time_of_CIP}}{The duration of a presumed CIP. (in hours)}
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
# Create a plot with the Specific Flux of Train 3 in black and mark 
# the areas where the train is off with vertical magenta lines. 

Train_3_Off_Dates <- subset(Train_3$Date_Time, Train_3$ON_OFF == 0 | is.na(Train_3$ON_OFF))
plot(Train_3$Date_Time, Train_3$Sp_Fl, col = "black", main  = "Train 3 Specific Flux over Time")
abline(v = Train_3_Off_Dates, col = "magenta") 


#-------------------------------------------------------------------------------
\cr

}

%\keyword{datasets}
























