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
#df <- df[,!colnames(bb) %in% c("Crash ID", "Adjusted Average Daily Traffic Amount", "Average Daily Traffic Year", "Case ID", "City", "County", "Crash Possible Injury Count", "Crash Unknown Injury Count", "Population Group", "Person Type", "Street Number", "Street Name", "Roadway relation", "Crash Total Injury Count")]


sum(is.na(df))


df$`Right of Way Usual Width` <- as.numeric(df$`Right of Way Usual Width`)
df$`Speed Limit` <- as.numeric(df$`Speed Limit`)
df$`Number of Lanes` <- as.numeric(df$`Number of Lanes`)
df$`Outside Shoulder Width on Divided Highway` <- as.numeric(df$`Outside Shoulder Width on Divided Highway`)
df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)
df$`Crash Incapacitating Injury Count` <- as.numeric(df$`Crash Incapacitating Injury Count`)
df$`Average Daily Traffic Amount` <- as.numeric(df$`Average Daily Traffic Amount`)



library(naniar)
mcar_test(df)

#If the p-value > 0.05, missing data is missing completely at random.

library(missForest)
MCARNNTest(df)



num_cols <- unlist(lapply(df, is.numeric))
data_num <- df[ , num_cols]
#numèriques
cormat <- round(cor(data_num, use = "pairwise.complete.obs"), 2)
#mixed data
library(GGally)
# S'ha de dir el número de variables que vols, com a màxim són 15
fggpairs(df, columns = 1:4, title = "Pairwise Correlations")
