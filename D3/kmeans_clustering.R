
# K-MEANS CLUSTERING WITH NUMERICAL VARIABLES

library(cluster)
library(ggplot2)
library(factoextra)
library(dplyr)
library(readr)

df <- read_csv("~/GIA/4sem/PMAAD/Scripts/DadesMimmi.csv")
attach(df)
df2 <- subset(df, select = -c(Crash_ID, Crash_Year, Crash_Time, Crash_Month, Crash_Date, Hour_of_Day, Charge))

classes <- sapply(df2, class)
num <- names(classes)[which(classes == "numeric")]
dfnum <- subset(df2, select = num)
datos_norm <- data.frame(lapply(dfnum, scales::rescale))

# KMEANS
km_clusters <- kmeans(x = dfnum, centers = 5, nstart = 50)
fviz_cluster(object = km_clusters, data = dfnum, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")

