
# DBSCAN CLUSTERING WITH NUMERICAL VARIABLES

library(cluster)
library(fpc)
library(pracma)
library(ggplot2)
library(factoextra)
library(dbscan)

df <- 'D:/PMAAD_project/cleaned_db.csv'
datos <- read.csv(df)
View(datos)
nrow(datos)
ncol(datos)
colnames(datos)

