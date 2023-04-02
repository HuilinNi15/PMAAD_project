
# OPTICS CLUSTERING WITH NUMERICAL VARIABLES

# install.packages("cluster")
# install.packages("fpc")
# install.packages("pracma")
# install.packages("ggplot2")
# install.packages("factoextra")
# install.packages("dbscan")
# install.packages("dplyr")
# install.packages("cli")
# install.packages("doParallel")
# install.packages("foreach")
# install.packages("readr")

library(cluster)
library(fpc)
library(pracma)
library(ggplot2)
library(factoextra)
library(dbscan)
library(dplyr)
library(cli)
library(doParallel)
library(foreach)
library(readr)

df <- read_csv("~/GIA/4sem/PMAAD/Scripts/DadesMimmi.csv")
attach(df)
df2 <- subset(df, select = -c(Crash_ID, Crash_Year, Crash_Time, Crash_Month, Crash_Date, Hour_of_Day, Charge))

classes <- sapply(df2, class)
num <- names(classes)[which(classes == "numeric")]
datos <- subset(df2, select = num)
datos_norm <- data.frame(lapply(datos, scales::rescale))


optics <- dbscan::optics(datos, eps = 0.5, minPts = 5)
plot(optics, reachability = TRUE)

eps_values <- seq(0.1, 1.0, by = 0.1)
minPts_values <- seq(5, 20, by = 5)
grid <- expand.grid(eps = eps_values, minPts = minPts_values)
cores <- detectCores()
registerDoParallel(cores = cores)

run_optics <- function(data, eps, minPts) {
  optics <- dbscan::optics(data, eps = eps, minPts = minPts)
  res <- dbscan::extractDBSCAN(optics, eps_cl = eps)
  sil <- cluster::silhouette(res$cluster, dist(data))
  return(ifelse(is.na(sil), sil, mean(sil[, 3])))
}

results <- foreach(i = 1:nrow(grid), .combine = rbind) %dopar% {
  eps <- grid$eps[i]
  minPts <- grid$minPts[i]
  score <- run_optics(datos[, -3], eps, minPts)
  c(eps, minPts, score)
}

results <- results[, c(1:3)]

best_params <- grid[which.max(results[, 3]), ] #eps = 0.1, minPts = 5

optics <- dbscan::optics(datos, eps = best_params$eps, minPts = best_params$minPts)

eps_values <- seq(0.1, 1, by = 0.1)
optics_results <- lapply(eps_values, function(e) optics(datos[, -3], eps = e, minPts = 5))

clusters <- lapply(optics_results, function(x) extractDBSCAN(x, eps = x$eps))

silhouette_avg <- sapply(clusters, function(x) mean(cluster::silhouette(x$cluster, dist(datos[, -3]))))

plot(eps_values, silhouette_avg, type = "b", pch = 20, main = "Silhouette Plot")

opt_eps <- eps_values[which.max(silhouette_avg)]
abline(v = opt_eps, lty = 2, col = "red")
