### Train Split


## Train 1 ---------------------------------------------------------------------

Train_1_names <- raw_data_names[raw_data_names$Service=="Train 1"|(raw_data_names$Service=="RO Train 1"),]

Train_1_vars <- Train_1_names$Variable

append(Train_1_vars,"Date_Time")

Train_1_data <- raw_data[, Train_1_vars]
Train_1_data_clean <- na.omit(Train_1_data)


## Train 2 ---------------------------------------------------------------------

Train_2_names <- raw_data_names[raw_data_names$Service=="Train 2"|(raw_data_names$Service=="RO Train 2"),]

Train_2_vars <- Train_2_names$Variable

append(Train_2_vars,"Date_Time")

Train_2_data <- raw_data[, Train_2_vars]
Train_2_data_clean <- na.omit(Train_2_data)

## Train 3 ---------------------------------------------------------------------

Train_3_names <- raw_data_names[raw_data_names$Service=="Train 3"|(raw_data_names$Service=="RO Train 3"),]

Train_3_vars <- Train_3_names$Variable

append(Train_3_vars,"Date_Time")

Train_3_data <- raw_data[, Train_3_vars]
Train_3_data_clean <- na.omit(Train_3_data)


## Train 4 ---------------------------------------------------------------------

Train_4_names <- raw_data_names[raw_data_names$Service=="Train 4"|(raw_data_names$Service=="RO Train 4"),]

Train_4_vars <- Train_4_names$Variable

append(Train_4_vars,"Date_Time")

Train_4_data <- raw_data[, Train_4_vars]
Train_4_data_clean <- na.omit(Train_4_data)


## Train 5 ---------------------------------------------------------------------

Train_5_names <- raw_data_names[raw_data_names$Service=="Train 5"|(raw_data_names$Service=="RO Train 5"),]

Train_5_vars <- Train_5_names$Variable

append(Train_5_vars,"Date_Time")

Train_5_data <- raw_data[, Train_5_vars]
Train_5_data_clean <- na.omit(Train_5_data)



