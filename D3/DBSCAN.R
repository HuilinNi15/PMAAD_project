
# DBSCAN CLUSTERING WITH NUMERICAL VARIABLES

library(cluster)
library(fpc)
library(pracma)
library(ggplot2)
library(factoextra)
library(dbscan)
library(dplyr)


df <- "./Datasets/cleaned_db.csv"
data <- read.csv(df)

nrow(data)
ncol(data)
colnames(data)


set.seed(11042023)


# Making a subset of numeric data
num_cols <- unlist(lapply(data, is.numeric), use.names = FALSE)
num_cols

data_num <- data[, num_cols] # Subset numeric columns of data
ncol(data_num)

sapply(data_num, class)

# Removing Crash_ID column

data_num <- data_num[, -1]
sapply(data_num, class)

# We apply the DBSCAN to classify the data
dbscan_res <- dbscan::dbscan(data_num, eps = 0.15, minPts = 5)

# Plotting the obtained DBSCAN
fviz_cluster(object = dbscan_res, data = data_num, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")


library(FactoMineR)

# create two sample data frames with numerical variables
df1 <- data.frame(
  x1 = c(1, 2, 3, 4, 5),
  x2 = c(6, 7, 8, 9, 10),
  x3 = c(11, 12, 13, 14, 15)
)

df2 <- data.frame(
  y1 = c(2, 4, 6, 8, 10),
  y2 = c(1, 3, 5, 7, 9),
  y3 = c(10, 9, 8, 7, 6)
)

# create a list of the data frames
datalist <- list(df1, df2)

# perform MFA
mfa <- MFA(datalist, graph = FALSE, group = c(3, 3))

# print the results
print(mfa)