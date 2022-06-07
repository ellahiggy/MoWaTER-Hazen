install.packages("timetk")
install.packages("dplyr")

library(timetk)
library(dplyr)

HSMasterByDay <- HSMaster

HSMasterByDay <- condense_period(HSMasterByDay, Date_Time, .period = "1 day", .side = c("start","end"))