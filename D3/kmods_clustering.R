
# KMODS CLUSTERING WITH MIXED DATA

# install.packages("klaR")
library(klaR) 
library(readr)

df <- read_csv("~/GIA/4sem/PMAAD/Scripts/DadesMimmi.csv")
attach(df)
df2 <- subset(df, select = -c(Crash_ID, Crash_Year, Crash_Time, Crash_Month, Crash_Date, Hour_of_Day, Charge))
# S'ha d'executar sense canviar el format de les dades (no tornar-les a factor)

cl <- klaR::kmodes(df2, 5) # 5 clusters

print(cl)

datos$cluster <- cl$cluster
