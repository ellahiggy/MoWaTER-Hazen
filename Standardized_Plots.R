##########
# Standardized Train Data and Plots

Train_1_standardized <- Train_1

for (col in 2:ncol(Train_1)){
  Train_1_standardized[,col] <- scale(Train_1[,col])
}

Train_2_standardized <- Train_2

for (col in 2:ncol(Train_2)){
  Train_2_standardized[,col] <- scale(Train_2[,col])
}

Train_3_standardized <- Train_3

for (col in 2:ncol(Train_3)){
  Train_3_standardized[,col] <- scale(Train_3[,col])
}

Train_4_standardized <- Train_4

for (col in 2:ncol(Train_4)){
  Train_4_standardized[,col] <- scale(Train_4[,col])
}

Train_5_standardized <- Train_5

for (col in 2:ncol(Train_5)){
  Train_5_standardized[,col] <- scale(Train_5[,col])
}

plot(Train_3_standardized$Sp_Fl ~ Train_3_standardized$Date_Time)
points(Train_3_standardized$Nt_DP ~ Train_3_standardized$Date_Time, col = "blue")
points(Train_3_standardized$P_f ~ Train_3_standardized$Date_Time, col = "green")

train_3_off <- as.integer(!(Train_3$ON_TR3))
time_train_3_off <- Train_3$Date_Time[!(Train_3$ON_TR3)]
abline(v = time_train_3_off, col = "magenta")

