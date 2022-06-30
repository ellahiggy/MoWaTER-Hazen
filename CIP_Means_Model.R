# -------------------------------------------------------------------------
# MOWATER 
# Hazen and Sawyer Team
# Modeling
# Henry Burch, PJ Williams, Ella Higginbotham, Lauren Walker, Cyril Pillai
# 
# This file contains the steps we took to split our modeling dataset
# and the code used to create our the logistic regression model.
#  -------------------------------------------------------------------------


load("Hazen_Sawyer_Data_Clean.rda")

#load in each dataframe object from the list

Train_1 <- HS_Dataset$Train_1
Train_2 <- HS_Dataset$Train_2
Train_3 <- HS_Dataset$Train_3
Train_4 <- HS_Dataset$Train_4
Train_5 <- HS_Dataset$Train_5

Train1_ON_Change <- HS_Dataset$Train1_ON_Change
Train2_ON_Change <- HS_Dataset$Train2_ON_Change
Train3_ON_Change <- HS_Dataset$Train3_ON_Change
Train4_ON_Change <- HS_Dataset$Train4_ON_Change
Train5_ON_Change <- HS_Dataset$Train5_ON_Change


# Create new data frame to append to with the Percent Change of 5 selected variables 
Changes_Percent_Between_CIP_DF <- as.data.frame(matrix(ncol= 8))
colnames(Changes_Percent_Between_CIP_DF) <- c("Sp_Fl","Nt_DP", "Per_R_a","NCp","P_f", "Start_Index","End_Index","Train_Num")

df_index <- 1

#add each Train's percent changes for the above variables to the dataframe

### Train 3  -------------------------------------------------------------------

for (value in 2:nrow(Train3_ON_Change)){ #Change Train Number
  First_Index <- Train3_ON_Change$Start_Index[value] #Change Train Number
  Last_Index <- Train3_ON_Change$End_Index[value] #Change Train Number
  Train_Data_ON <- Train_3[Train_3$Index >= First_Index & #Change Train Number
                             Train_3$Index <= Last_Index,] #Change Train Number
  List_of_Percent_Changes <- c()
  
  for (num in 2:6){
    #plot(Train_Data_ON[,num] ~ Train_Data_ON$Index)
    model <- lm(Train_Data_ON[,num] ~ Train_Data_ON$Index)
    #abline(model, col = 'red')
    slope <- as.numeric(model$coefficients[2])
    intercept <- as.numeric(model$coefficients[1])
    #time <- as.integer(difftime("2021-04-29 02:00:00","2021-01-21 00:00:00",units = c("hours")))
    change <- slope * nrow(Train_Data_ON)
    Start_Val <- (slope * Train_Data_ON$Index[1] + intercept)
    Percent_Change <- change / Start_Val
    List_of_Percent_Changes <- append(List_of_Percent_Changes,Percent_Change)
  }
  Start_End <- c(First_Index,Last_Index,3) #Change Train Number
  Row <- append(List_of_Percent_Changes,Start_End)
  Changes_Percent_Between_CIP_DF[df_index,] <- Row
  df_index <<- df_index + 1
}

### Train 4 ----------------------------------------------------------------------

for (value in 2:nrow(Train4_ON_Change)){ #Change Train Number
  First_Index <- Train4_ON_Change$Start_Index[value] #Change Train Number
  Last_Index <- Train4_ON_Change$End_Index[value] #Change Train Number
  Train_Data_ON <- Train_4[Train_4$Index >= First_Index & #Change Train Number
                             Train_4$Index <= Last_Index,] #Change Train Number
  List_of_Percent_Changes <- c()
  
  for (num in 2:6){
    #plot(Train_Data_ON[,num] ~ Train_Data_ON$Index)
    model <- lm(Train_Data_ON[,num] ~ Train_Data_ON$Index)
    #abline(model, col = 'red')
    slope <- as.numeric(model$coefficients[2])
    intercept <- as.numeric(model$coefficients[1])
    #time <- as.integer(difftime("2021-04-29 02:00:00","2021-01-21 00:00:00",units = c("hours")))
    change <- slope * nrow(Train_Data_ON)
    Start_Val <- (slope * Train_Data_ON$Index[1] + intercept)
    Percent_Change <- change / Start_Val
    List_of_Percent_Changes <- append(List_of_Percent_Changes,Percent_Change)
  }
  Start_End <- c(First_Index,Last_Index,4) #Change Train Number
  Row <- append(List_of_Percent_Changes,Start_End)
  Changes_Percent_Between_CIP_DF[df_index,] <- Row
  df_index <<- df_index + 1
}

### Train 5 --------------

for (value in 2:nrow(Train5_ON_Change)){ #Change Train Number
  First_Index <- Train5_ON_Change$Start_Index[value] #Change Train Number
  Last_Index <- Train5_ON_Change$End_Index[value] #Change Train Number
  Train_Data_ON <- Train_5[Train_5$Index >= First_Index & #Change Train Number
                             Train_5$Index <= Last_Index,] #Change Train Number
  List_of_Percent_Changes <- c()
  
  for (num in 2:6){
    #plot(Train_Data_ON[,num] ~ Train_Data_ON$Index)
    model <- lm(Train_Data_ON[,num] ~ Train_Data_ON$Index)
    #abline(model, col = 'red')
    slope <- as.numeric(model$coefficients[2])
    intercept <- as.numeric(model$coefficients[1])
    #time <- as.integer(difftime("2021-04-29 02:00:00","2021-01-21 00:00:00",units = c("hours")))
    change <- slope * nrow(Train_Data_ON)
    Start_Val <- (slope * Train_Data_ON$Index[1] + intercept)
    Percent_Change <- change / Start_Val
    List_of_Percent_Changes <- append(List_of_Percent_Changes,Percent_Change)
  }
  Start_End <- c(First_Index,Last_Index,5) #Change Train Number
  Row <- append(List_of_Percent_Changes,Start_End)
  Changes_Percent_Between_CIP_DF[df_index,] <- Row
  df_index <<- df_index + 1
}

#---------------------------------------------------------------------------------

#create the CIP column (0,1) based on how the % change value for the variables 
#in each time period compares to the mean for all time periods in the train

#If 4 or more % changes exceed the mean change, then 
#a CIP happened immediately after that period (marked with a 1)

Percent_Means_List <- as.numeric(colMeans(Changes_Percent_Between_CIP_DF))
Changes_Percent_Between_CIP_DF$CIP <- 0

for (row in 1:nrow(Changes_Percent_Between_CIP_DF)){
  sum = 0
  for(col in 1:5){
    if(Percent_Means_List[col] < 0){
      if (Changes_Percent_Between_CIP_DF[row,col] < Percent_Means_List[col]){
        sum = sum + 1
      }
    }else{
      if (Changes_Percent_Between_CIP_DF[row,col] > Percent_Means_List[col]){
        sum = sum + 1
      }
    }
  }
  if (sum >= 4){
    Changes_Percent_Between_CIP_DF$CIP[row] <- 1
  }
}


sum(Changes_Percent_Between_CIP_DF$CIP)


### Log Model -----------------------------------------------------------------


#split data into training and testing set (70/30)
library(caTools)
set.seed(0)
sample <- sample.split(Changes_Percent_Between_CIP_DF$CIP, SplitRatio = 0.7)
training_data <- subset(Changes_Percent_Between_CIP_DF, sample == TRUE)
testing_data <- subset(Changes_Percent_Between_CIP_DF, sample == FALSE)


#create the logistic model
Log_model <- glm(CIP ~ 
                   Sp_Fl    +
                   Nt_DP    +
                   Per_R_a  +
                   NCp      +
                   P_f, 
                 data = training_data,
                 family=binomial)

predict_reg <- predict.glm(Log_model,
                           newdata = testing_data[,1:5], type = "response")
testing_data$CIP_Predict <- round(predict_reg, 2)

#model prediction values are the last column in testing_data, or can be seen
#in predict_reg


#-------------------------------------------------------------------------------
