\name{MoWaTER_Hazen} % replace with your project name
\alias{MoWaTER_Hazen} % replace with your project name

\docType{data} % don't change

\title{MoWaTER_Hazen_and_Sawyer} % replace with your project full name

\description{
% this paragraph should contain full sentences that would answer these questions for someone who has never seen this data before
This data came from Hazen and Sawyer, an environmental engineering firm who focuses on providing their clients with clean drinking water. This specific data came from one of Hazen and Sawyer's confidential clients and contained over 200 variables with more than 8000 observations with information on measurements that the client used to determine when a clean in place (CIP) process should occur for the filtration system. However, the 200 variables are separated between the five trains contained in filtration system; therefore, there are approximately 30 repeated variables for each train. ****We used a linear regression model with our response variable being the change in percent between CIPs for each predictor variable. The predictor variables were specific flux, normalized permeate conductivity, net driving pressure, feedwater pressure, and normalized differential pressure. Additionally, we used multiple cleaned data frames that contained data specific for each of the five trains and when they were on and off.****
}


\usage{
load("Hazen_Sawyer_Data_Clean.rda`")
data(MoWaTER_Hazen) #- loads into working environment
MoWaTER_Hazen
}

\format{
A collection of 10 dataframes: one for each train (totaling 5), one for each time each train is on (also totaling 5).
For each data object in the list, fill in the following:

\strong{(1)} \code{Train_1 to Train_5} is a dataframe with 8,376 rows and 9 columns, with the variables below: 

\describe{
\item{\code{Date_Time}}{A measure of the date and its corresponding time. (Year-Month-Day; Hour-Min-Sec)}
\item{\code{Sp_Fl}}{description (with units)}
\item{\code{Nt_DP}}{A measurement of the pressure difference between influent and effluent water. (with units)}
\item{\code{NCp}}{A measurement of the conductivity of the water that leaves the filtration system. The measurement is normalized based on a calculation done by Hazen and Sawyer's client. (with units)}
\item{\code{Per_R_a}}{A measurement of the percent of water recovered after being filtered through the train systems. (Percent)}
\item{\code{P_f}}{A measurement of the amount of water that leaves the system after being filtered. (with units)}
\item{\code{SP_n}}{A measurement of the amount of salt that passes through the filtration system. (with units)}
\item{\code{ON_OFF}}{A binary measurement to determine whether the trains are on (represented by 1) or off (represented by 0).}
\item{\code{Index}}{Assigns a number to each row that helps with separating CIPs based on index number.}
}

\strong{(1)} \code{Train1_ON_Change} is a data frame with 18 rows and 5 columns, the variables of which are as follows:

\describe{
\item{\code{ON_Count}}{A numerical count of when the train was on after a CIP occurred.}
\item{\code{Start_Index}}{Row number of when the train is turned on.}
\item{\code{End_Index}}{Row number of when the train is turned off.}
\item{\code{Time_ON}}{The amount of hours that the train was turned on for. (hours)}
\item{\code{Time_of_CIP}}{Length of how long the  (with units)}
}

}

\source{
% fill this in
Data is provided by Hazen and Sawyer
Contacts: 
Javad Roostaei - JRoostaei@hazenandsawyer.com
Ben Stanford - BStanford@hazenandsawyer.com
Links to relevant information
}

\examples{
data(project_name)

#- put interesting exploratory plots here




#- trains 1-5, specific flux w/ NA values
par(mfrow = c(2, 3)) 

plot(Train_1$Date_Time, Train_1$Sp_Fl_TR1- median(Train_1$Sp_Fl_TR1, na.rm=TRUE), 
     main = "Train 1 Specific Flux Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Specific Flux", 
     #ylim = c(0.07, 0.13), 
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_1_spfl, col = adjustcolor("magenta", alpha = 0.1))



plot(Train_2$Date_Time, Train_2$Sp_Fl_TR2 - median(Train_2$Sp_Fl_TR2, na.rm=TRUE), 
     main = "Train 2 Specific Flux Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Specific Flux", 
     ylim = c(-0.01, 0.01),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19) 

abline(v = na_time_2_spfl, col = adjustcolor("magenta", alpha = 0.2))



plot(Train_3$Date_Time, Train_3$Sp_Fl_TR3 - median(Train_3$Sp_Fl_TR3, na.rm=TRUE), 
     main = "Train 3 Specific Flux Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Specific Flux", 
     ylim = c(-0.01, 0.006),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_3_spfl, col = adjustcolor("magenta", alpha = 0.1))



plot(Train_4$Date_Time, Train_4$Sp_Fl_TR4 - median(Train_4$Sp_Fl_TR4, na.rm=TRUE), 
     main = "Train 4 Specific Flux Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Specific Flux", 
     ylim = c(-0.03, 0.03),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_4_spfl, col = adjustcolor("magenta", alpha = 0.1))



plot(Train_5$Date_Time, Train_5$Sp_Fl_TR5 - median(Train_5$Sp_Fl_TR5, na.rm=TRUE), 
     main = "Train 5 Specific Flux Over Time
     (NA values in magenta)", 
     xlab = "Date", 
     ylab = "Specific Flux", 
     ylim = c(-0.016, 0.015),
     col = rgb(0, 0, 0, 0.5), 
     pch = 19)

abline(v = na_time_5_spfl, col = adjustcolor("magenta", alpha = 0.1))


#-------------------------------------------------------------------------------


#- trains 3, 4, 5 spec flux, net driving press, perm cond, feedwater press w/ train=OFF
Train_3$ON_TR3[is.na(Train_3$ON_TR3)] = 0
Train_4$ON_TR4[is.na(Train_4$ON_TR4)] = 0
Train_5$ON_TR5[is.na(Train_5$ON_TR5)] = 0

par(mfrow = c(2, 2))

plot(Train_3$Date_Time, Train_3$Sp_Fl_TR3, 
     xlab = "Date", 
     ylab = "Specific Flux", 
     main = "Train 3 Specific Flux Over Time, 
     Train = OFF in magenta",
     col = "black")

points(Train_3$Date)

train_3_off <- as.integer(!(Train_3$ON_TR3))

time_train_3_off <- Train_3$Date_Time[!(Train_3$ON_TR3)]

abline(v = time_train_3_off, col = "magenta")


plot(Train_3$Date_Time, Train_3$Nt_DP_TR3, 
     xlab = "Date", 
     ylab = "Net Driving Pressure", 
     main = "Train 3 Net Driving Pressure Over Time, 
     Train = OFF in magenta",
     ylim = c(145, 175),
     col = "green")

train_3_off <- as.integer(!(Train_3$ON_TR3))

time_train_3_off <- Train_3$Date_Time[!(Train_3$ON_TR3)]

abline(v = time_train_3_off, col = "magenta")


plot(Train_3$Date_Time, Train_3$C_p_TR3, 
     xlab = "Date", 
     ylab = "Permeate Conductivity", 
     main = "Train 3 Permeate Conductivity Over Time, 
     Train = OFF in magenta",
     ylim = c(25, 40),
     col = "red")

train_3_off <- as.integer(!(Train_3$ON_TR3))

time_train_3_off <- Train_3$Date_Time[!(Train_3$ON_TR3)]

abline(v = time_train_3_off, col = "magenta")



plot(Train_3$Date_Time, Train_3$F_p_TR3, 
     xlab = "Date", 
     ylab = "Feedwater Pressure", 
     main = "Train 3 Feedwater Pressure Over Time, 
     Train = OFF in magenta", 
     ylim = c(1380, 1400),
     col = "orange")

train_3_off <- as.integer(!(Train_3$ON_TR3))

time_train_3_off <- Train_3$Date_Time[!(Train_3$ON_TR3)]

abline(v = time_train_3_off, col = "magenta")





par(mfrow = c(2, 2))

plot(Train_4$Date_Time, Train_4$Sp_Fl_TR4, 
     xlab = "Date", 
     ylab = "Specific Flux", 
     main = "Train 4 Specific Flux Over Time, 
     Train = OFF in blue",
     ylim = c(0.08, 0.14),
     col = "black")

train_4_off <- as.integer(!(Train_4$ON_TR4))

time_train_4_off <- Train_4$Date_Time[!(Train_4$ON_TR4)]

abline(v = time_train_4_off, col = "blue")



plot(Train_4$Date_Time, Train_4$Nt_DP_TR4, 
     xlab = "Date", 
     ylab = "Net Driving Pressure", 
     main = "Train 4 Net Driving Pressure Over Time, 
     Train = OFF in blue",
     ylim = c(80, 165),
     col = "green")

train_4_off <- as.integer(!(Train_4$ON_TR4))

time_train_4_off <- Train_4$Date_Time[!(Train_4$ON_TR4)]

abline(v = time_train_4_off, col = "blue")



plot(Train_4$Date_Time, Train_4$C_p_TR4,  
     xlab = "Date", 
     ylab = "Permeate Conductivity", 
     main = "Train 4 Permeate Conductivity Over Time, 
     Train = OFF in blue",
     ylim = c(45, 75),
     col = "red")

train_4_off <- as.integer(!(Train_4$ON_TR4))

time_train_4_off <- Train_4$Date_Time[!(Train_4$ON_TR4)]

abline(v = time_train_4_off, col = "blue")



plot(Train_4$Date_Time, Train_4$F_p_TR4,  
     xlab = "Date", 
     ylab = "Feedwater Pressure", 
     main = "Train 4 Feedwater Pressure Over Time, 
     Train = OFF in blue",
     ylim = c(1700, 2400),
     col = "orange")

train_4_off <- as.integer(!(Train_4$ON_TR4))

time_train_4_off <- Train_4$Date_Time[!(Train_4$ON_TR4)]

abline(v = time_train_4_off, col = "blue")





par(mfrow = c(2, 2))

plot(Train_5$Date_Time, Train_5$Sp_Fl_TR5, 
     xlab = "Date", 
     ylab = "Specific Flux", 
     main = "Train 5 Specific Flux Over Time, 
     Train = OFF in purple",
     ylim = c(0.110, 0.150),
     col = "black")

train_5_off <- as.integer(!(Train_5$ON_TR5))

time_train_5_off <- Train_5$Date_Time[!(Train_5$ON_TR5)]

abline(v = time_train_5_off, col = "purple")



plot(Train_5$Date_Time, Train_5$Nt_DP_TR5,  
     xlab = "Date", 
     ylab = "Net Driving Pressure", 
     main = "Train 5 Net Driving Pressure Over Time, 
     Train = OFF in purple",
     ylim = c(80, 120),
     col = "green")

train_5_off <- as.integer(!(Train_4$ON_TR4))

time_train_5_off <- Train_5$Date_Time[!(Train_5$ON_TR5)]

abline(v = time_train_5_off, col = "purple")



plot(Train_5$Date_Time, Train_5$C_p_TR5,  
     xlab = "Date", 
     ylab = "Permeate Conductivity", 
     main = "Train 5 Permeate Conductivity Over Time, 
     Train = OFF in purple",
     ylim = c(30, 50),
     col = "red")

train_5_off <- as.integer(!(Train_5$ON_TR5))

time_train_5_off <- Train_5$Date_Time[!(Train_5$ON_TR5)]

abline(v = time_train_5_off, col = "purple")



plot(Train_5$Date_Time, Train_5$F_p_TR5,  
     xlab = "Date", 
     ylab = "Feedwater Pressure", 
     main = "Train 5 Feedwater Pressure Over Time, 
     Train = OFF in purple",
     ylim = c(1700, 2500),
     col = "orange")

train_5_off <- as.integer(!(Train_5$ON_TR5))

time_train_5_off <- Train_5$Date_Time[!(Train_5$ON_TR5)]

abline(v = time_train_5_off, col = "purple")

#-------------------------------------------------------------------------------


}

\keyword{datasets}
