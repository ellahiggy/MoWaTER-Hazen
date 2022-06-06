
setwd("/Users/cyrilpillai/Desktop/Data_Processing")
getwd()
load("HSOnlyCompleteEntries - Hazen and Sawyer.rda")
load("HSMaster - Hazen and Sawyer.rda")

HSCompleteEntriesCopy <- HSCompleteEntries

numZeroes <- colSums(HSCompleteEntries == 0)
colNames <- names(HSCompleteEntriesCopy)
ncol(HSCompleteEntriesCopy)

drops <- vector()
for(i in 1:ncol(HSCompleteEntriesCopy))
  if(numZeroes[i] / nrow(HSCompleteEntriesCopy) >= 0.8)
    drops <- c(drops, colNames[i])
drops <- c(drops, colNames[2])
print(drops)

 
HSCompleteEntriesCopy <- HSCompleteEntriesCopy[ , !(names(HSCompleteEntriesCopy) %in% drops)]

correlations <- cor(HSCompleteEntriesCopy[,unlist(lapply(HSCompleteEntriesCopy, is.numeric))])
correlations <- as.data.frame(correlations)

# corrVarNames <- names(correlations)
# 
# highCorrelationsList <- vector()
# size <- length(correlations) # square!!!
# r <- 1
# c <- 1
# for(r in 1:size)
#   for(c in 1:size)
#     if(!(is.na(correlations[r,c])))
#       if(correlations[r,c] > 0.9 && correlations[r,c] < 1)
#         highCorrelationsList <- c(highCorrelationsList, c(corrVarNames[r], corrVarNames[c], correlations[r,c]))
# 
# x <- 1
# while(x < length(highCorrelationsList))
# {
#   paste(highCorrelationsList[x], highCorrelationsList[x+1], highCorrelationsList[x+2])
#   print(paste(highCorrelationsList[x], highCorrelationsList[x+1], highCorrelationsList[x+2]))
#   x <- x + 3
# }




        



      





