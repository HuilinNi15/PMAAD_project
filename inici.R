library(readr)
bb <- read_csv("~/GIA/Apunts 4sem_1/PMAAD/Treball/Austin Bicycle Crashes 2010-2017.csv")
attach(bb)

bb[bb == c("No Data", "N/A", "", "UNKNOWN", "NO DATA", "Unknown", 'Unknown', 'No Data', 'NO DATA', 'UNKNOWN', 'N/A', '')] <- NA

name <- colnames(bb)

bb <- bb[,!colnames(bb) %in% c("Crash ID", "Adjusted Average Daily Traffic Amount", "Average Daily Traffic Year", "Case ID", "City", "County", "Crash Possible Injury Count", "Crash Unknown Injury Count", "Population Group", "Person Type", "Street Number", "Street Name", "Roadway relation")]
