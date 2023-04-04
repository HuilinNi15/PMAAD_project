
# DBSCAN CLUSTERING WITH NUMERICAL VARIABLES

library(cluster)
library(fpc)
library(pracma)
library(ggplot2)
library(factoextra)
library(dbscan)

df <- 'D:/PMAAD_project/cleaned_db.csv'
data <- read.csv(df)
# View(data)
nrow(data)
ncol(data)
colnames(data)


set.seed(11042023)


# Making a subset of numeric data
num_cols <- unlist(lapply(data, is.numeric))
num_cols

data_num <- data[ , num_cols] # Subset numeric columns of data
ncol(data_num)
# View(data_num)

sapply(data_num, class)

# Removing Crash_ID column

data_num <- data_num[,-1]
sapply(data_num, class)

# We apply the DBSCAN to classify the data
dbscan_res <- dbscan::dbscan(data_num, eps = 0.15, minPts = 5)

# Plotting the obtained DBSCAN
fviz_cluster(object = dbscan_res, data = data_num, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")
