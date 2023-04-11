<<<<<<< HEAD

# DBSCAN CLUSTERING WITH NUMERICAL VARIABLES

library(cluster)
library(fpc)
library(pracma)
library(ggplot2)
library(factoextra)
library(dbscan)
library(dplyr)

df <- "./Datasets/cleaned_db.RDS"
data <- readRDS(df)

nrow(data)
ncol(data)
colnames(data)

set.seed(11042023)

# Making a subset of numeric data
num_cols <- unlist(lapply(data, is.numeric), use.names = FALSE)
num_cols

data_num <- data[, num_cols] # Subset numeric columns of data
ncol(data_num)

# Removing Crash_ID column
data_num <- data_num[, -1]
sapply(data_num, class)

# Normalize the numerical data (ALWAYS)
norm_data <- data.frame(lapply(data_num, scales::rescale))
data[, num_cols] <- norm_data
# View(data)

df <- subset(data, select = -c(Crash_ID, Crash_Year, Crash_Time, Crash_Month, 
                               Crash_Date, Hour_of_Day, Charge))

gower_dist <- daisy(df, metric = "gower")
gower_dist <- gower_dist^2

# We apply the DBSCAN to classify the data
dbscan_res <- dbscan::dbscan(gower_dist, eps = 0.15, minPts = 5)

# Plotting the obtained DBSCAN
fviz_cluster(object = dbscan_res, data = norm_data, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")


# Calculating eps and minPts --------------------------------------------------------------
# minPts
percentatge <- 0.0025 

min_pts <- round(nrow(df) * percentatge) 

min_pts <- ifelse(min_pts <= 1, 2, min_pts)
min_pts <- ifelse(min_pts >= 10, 10, min_pts)

min_pts # 10

# eps
neigh_dist <- dbscan::kNNdist(gower_dist, k = min_pts)
Y <- neigh_dist[order(neigh_dist)]
X <- c(0:(length(Y) - 1))

pendientes <- c()
for (i in 1:length(X)) {
	pendientes[i] <- (Y[i + 1] - Y[i])/(X[i+1] - X[i])
  }

m <- which.max(pendientes)
first <- gdata::first(which(pendientes >= m))
epsilon <- Y[first]

{kNNdistplot(norm_data, k = 5, minPts = min_pts)
  abline(h = 0.235, lty = 2, col = "red")}

### with the elbow plot, we can see that the epsilon is 0.235 (upper bound)
epsilon <- 0.2

# -----------------------------------------------------------------------------
res <- dbscan::dbscan(gower_dist, eps = epsilon, minPts = min_pts) 

df$cluster <- res$cluster

clean_data <- dplyr::filter(df, cluster != 0)

outliers <- dplyr::filter(df, cluster == 0) 

fviz_cluster(object = res, data = norm_data, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")

hullplot(norm_data, res$cluster, main = paste0("Convex cluster Hulls, eps = ", epsilon))
=======

# DBSCAN CLUSTERING WITH NUMERICAL VARIABLES

library(cluster)
library(fpc)
library(pracma)
library(ggplot2)
library(factoextra)
library(dbscan)
library(dplyr)

df <- "./Datasets/cleaned_db.RDS"
data <- readRDS(df)

nrow(data)
ncol(data)
colnames(data)

set.seed(11042023)

# Making a subset of numeric data
num_cols <- unlist(lapply(data, is.numeric), use.names = FALSE)
num_cols

data_num <- data[, num_cols] # Subset numeric columns of data
ncol(data_num)

# Removing Crash_ID column
data_num <- data_num[, -1]
sapply(data_num, class)

# Normalize the numerical data (ALWAYS)
norm_data <- data.frame(lapply(data_num, scales::rescale))
data[, num_cols] <- norm_data
# View(data)

df <- subset(data, select = -c(Crash_ID, Crash_Year, Crash_Time, Crash_Month, 
                               Crash_Date, Hour_of_Day, Charge))

gower_dist <- daisy(df, metric = "gower")
gower_dist <- gower_dist^2

# We apply the DBSCAN to classify the data
dbscan_res <- dbscan::dbscan(gower_dist, eps = 0.15, minPts = 5)

# Plotting the obtained DBSCAN
fviz_cluster(object = dbscan_res, data = norm_data, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")


# Calculating eps and minPts --------------------------------------------------------------
# minPts
percentatge <- 0.0025 

min_pts <- round(nrow(df) * percentatge) 

min_pts <- ifelse(min_pts <= 1, 2, min_pts)
min_pts <- ifelse(min_pts >= 10, 10, min_pts)

min_pts # 10

# eps
neigh_dist <- dbscan::kNNdist(gower_dist, k = min_pts)
Y <- neigh_dist[order(neigh_dist)]
X <- c(0:(length(Y) - 1))

pendientes <- c()
for (i in 1:length(X)) {
	pendientes[i] <- (Y[i + 1] - Y[i])/(X[i+1] - X[i])
  }

m <- which.max(pendientes)
first <- gdata::first(which(pendientes >= m))
epsilon <- Y[first]

{kNNdistplot(norm_data, k = 5, minPts = min_pts)
  abline(h = 0.235, lty = 2, col = "red")}

### with the elbow plot, we can see that the epsilon is 0.235 (upper bound)
epsilon <- 0.235

# -----------------------------------------------------------------------------
res <- dbscan::dbscan(gower_dist, eps = epsilon, minPts = min_pts) 

df$cluster <- res$cluster

clean_data <- dplyr::filter(df, cluster != 0)

outliers <- dplyr::filter(df, cluster == 0) 

fviz_cluster(object = res, data = norm_data, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")

hullplot(norm_data, res$cluster, main = paste0("Convex cluster Hulls, eps = ", epsilon))
