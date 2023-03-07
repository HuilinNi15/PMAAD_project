library(readr)
library(dplyr)

bb <- read_csv("~/GIA/Apunts 4sem_1/PMAAD/Treball/Austin Bicycle Crashes 2010-2017.csv")
attach(bb)

#bb[bb == c("No Data", "N/A", "", "UNKNOWN", "NO DATA", "Unknown", 'Unknown', 'No Data', 'NO DATA', 'UNKNOWN', 'N/A', '')] <- NA

#sum(is.na(bb))

#name <- colnames(bb)

#bb <- bb[,!colnames(bb) %in% c("Crash ID", "Adjusted Average Daily Traffic Amount", "Average Daily Traffic Year", "Case ID", "City", "County", "Crash Possible Injury Count", "Crash Unknown Injury Count", "Population Group", "Person Type", "Street Number", "Street Name", "Roadway relation")]

#sum(is.na(bb))

df <- mutate_all(bb, funs(replace(., .=='No Data', NA)))
df <- mutate_all(df, funs(replace(., .=='N/A', NA)))
df <- mutate_all(df, funs(replace(., .==' ', NA)))
df <- mutate_all(df, funs(replace(., .=='Unknown', NA)))
df <- mutate_all(df, funs(replace(., .=='NO DATA', NA)))
df <- mutate_all(df, funs(replace(., .=='UNKNOWN', NA)))

sum(is.na(df))

df <- df[,!colnames(bb) %in% c("Crash ID", "Adjusted Average Daily Traffic Amount", "Average Daily Traffic Year", "Case ID", "City", "County", "Crash Possible Injury Count", "Crash Unknown Injury Count", "Population Group", "Person Type", "Street Number", "Street Name", "Roadway relation")]

sum(is.na(df))

