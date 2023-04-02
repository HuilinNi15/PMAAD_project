
# CURE CLUSTERING WITH NUMERICAL VARIABLES

library(dplyr)
df <- read_csv("~/GIA/4sem/PMAAD/Scripts/DadesMimmi.csv")
attach(df)
df2 <- subset(df, select = -c(Crash_ID, Crash_Year, Crash_Time, Crash_Month, Crash_Date, Hour_of_Day, Charge))

# Paso 1: Definir el número de clústeres k y el factor de reducción r
k <- 4
r <- 0.2
metodo <- "euclidean"
data <- df2
n <- ceiling(0.3*nrow(data))

classes <- sapply(data, class)
num <- names(classes)[which(classes == "numeric")]
data <- subset(data, select = num)

# Sample de la muestra
ids <- sample(1:nrow(data), replace = FALSE, size = n)
dataNoMuestra <- data[-ids, ]
data <- data[ids, ]
dataMuestra <- data


data <- data.frame(lapply(data, scales::rescale))
hclust <- hclust(dist(data, method = metodo), method = "ward.D2")
plot(hclust)

subsets <- cutree(hclust, k)

dataMuestra$cluster <- subsets


# Paso 3: Seleccionar r elementos representativos de cada subconjunto utilizando k-means
centroides <- matrix(0, k, ncol(data))
representativos <- list()
noRepresentativos <- list()
indicesRepresentativos <- list()

for (i in 1:k) {
  subset <- dfnum[subsets == i,]
  subset$id <- rownames(subset)
  kmeans_result <- kmeans(subset[, -ncol(subset)], 1)
  centroides[i,] <- kmeans_result$centers
  
  matriz <- rbind(centroides[i, ], subset[, -ncol(subset)])
  distancias <- dist(x = matriz, method = metodo)
  distanciaFinal <- as.matrix(distancias)[1, -1]
  names(distanciaFinal) <- subset$id
  pesosOrdenados <- sort(distanciaFinal, decreasing = FALSE)
  indices <- as.numeric(names(pesosOrdenados)[1:(r*nrow(subset))])
  indicesRepresentativos[[i]] <- as.numeric(names(pesosOrdenados)[1:(r*nrow(subset))])
  representativos[[i]] <- subset[which(rownames(subset) %in% indices), ]
  noRepresentativos[[i]] <- subset[which(!rownames(subset) %in% indices), ]
}

noRepresentativos <- dplyr::bind_rows(noRepresentativos)

# Paso 4: Unir los elementos representativos seleccionados con los centroides
merged_representatives <- list()
for (i in 1:k) {
  bbdd <- rbind(centroides[i, ], representativos[[i]][, -ncol(representativos[[i]])])
  bbdd$cluster <- i
  merged_representatives[[i]] <- bbdd
}

centroidesRepresentativos <- dplyr::bind_rows(merged_representatives)

# Paso 5: Calculamos los nuevos centroides por cada una de las listas
NuevosCentroides <- centroidesRepresentativos %>%
  group_by(cluster) %>%
  summarise_all(mean) %>% data.frame()

# Paso 6: Asignar cada observación al clúster cuyo centroide esté más cercano
## Calculamos la distancia para cada cluster i detectamos cual es el más proximo a ellos 
clusterPertenece <- c()
for (i in 1:nrow(dataNoMuestra)) {
  ## 1. Agregamos el valor no representivo a los centroides nuevos
  bbdd <- dataNoMuestra[i, ]
  bbdd$cluster <- 0
  agregado <- rbind(bbdd, NuevosCentroides)
  
  ## 2. Calcula la distancia correspondiente
  distanciaCorr <- as.matrix(dist(agregado[, -1], method = metodo))[1, -1]
  quienEsMenor <- as.numeric(which.min(distanciaCorr))
  clusterPertenece <- c(clusterPertenece, agregado[quienEsMenor+1, "cluster"])   
}

dataNoMuestra$cluster <- clusterPertenece

dataMuestra
dataNoMuestra

table(dataMuestra$cluster)
table(dataNoMuestra$cluster)


