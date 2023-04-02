# ==============================================================================
# [IDEAI] -  Advanced clustering algorithm
# 
# Author(s):    Dante Conti and Sergi Ramírez, IDEAI  (c)
# Date:         10th March 2023
# Description: 
#             Este script trata los algoritmos de clasificación por densidad 
#             DBSCAN y OPTICS
# ==============================================================================
# Cargamos las librerias necesarias
library(cluster)
library(fpc)
library(pracma)
library(ggplot2)
library(factoextra)
library(dbscan)

# =============================================================================
### Generamos una semilla para poder ejecutar los datos
set.seed(04102022)

# ==============================================================================
### Creamos la base de datos que vamos a utilizar para detectar los grupos
data("multishapes")
datos <- multishapes[, 1:2]

### Printamos la imagen que hemos obtenido de los datos a clasificar
ggplot2::ggplot(datos, aes(x = x, y = y)) + 
	ggplot2::geom_point(color='#3333FF')
	
# ==============================================================================
# KMEANS: 
### Gráficamos los datos a través de un k-means para visualizar como quedarian los 
### grupos cuando utilizamos unos algoritmos de agrupación a partir de la inercia
km_clusters <- kmeans(x = datos, centers = 5, nstart = 50)
fviz_cluster(object = km_clusters, data = datos, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")

### Como podemos ver, Kmeans ha hecho una muy mala clusterización, puesto que:
###  - No ha conseguido clusterizar según las formas complejas del modelo.
###  - No ha tenido en cuenta que existen outliers, incluyéndolos en los distintos clusters

# ==============================================================================
# DBSCAN: 

### DBSCAN parte de dos parámetros que son: 
### - eps: distancia máxima a la que debe haber otra observación para ser considerar 
###        que cumple con el criterio de "estar cerca"
### - minPts: parámetro que controla la densidad mínima requerida para que un punto 
###           sea considerado un núcleo y se incluya en un grupo/clúster.

### Para un punto p, si existen al menos minPts puntos dentro del radio eps alrededor de p, 
### entonces p se considera un núcleo (core point) y se incluye en el mismo grupo/clúster 
### que los demás puntos dentro del radio eps. 
### Si no hay suficientes puntos dentro del radio eps, p se considera un punto frontera (border point) 
### y se incluye en el mismo grupo/clúster que su punto núcleo más cercano. 
### Si no hay ningún punto dentro del radio eps, p se considera un punto de ruido (noise point) 
### y no se incluye en ningún grupo/clúster.

### Aplicamos el algoritmo de dbscan para classificar los datos
dbscan_res <- dbscan::dbscan(datos, eps = 0.15, minPts = 5)

### Graficamos el dbscan obtenido 
fviz_cluster(object = dbscan_res, data = datos, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")

### Para escoger los valores de eps i minPts, necesitaremos optimizar el proceso. Para ello, 
### realizaremos la siguiente técnica de optimización. 

# ---------------------------------------------------------------------------------------------------
# Cálculo de min_pts

### El parámetro min_pts establece el número de puntos mínimo que, dado un radio eps, tiene
### que haber para que se considere que dichos puntos forman un clúster.
### Un valor bajo de min_pts asegurará que más puntos son agrupados, pero se corre el riesgo de 
### agrupar outliers. Por el contrario, un valor muy alto de min_pts puede descartar valores que 
### no son anómalos.

### En la literatura hablan de usar un valor entre 3 y 5 ya que funcionan bastante bien en la mayoría de los casos. min Pts igual 2 cuando tenemos una distribución normal y otra nube de outliers

### Para calcularlo de manera empírica, diremos que el mínimo de puntos sea igual al 0.2% - 0.25% del total de los datos teniendo en cuenta que: 

### - El minimo será de 2 para datos que sean muy pequeños
### - El máximo será de 10 para datos con mucha información

#### Cálculo de min_pts
porcentaje <- 0.0025 

# Cálculo de min_pts. 
min_pts <- round(nrow(datos) * porcentaje) 

# Realizamos los cortes de 2 y 10 que se mencionan anteriormente
min_pts <- ifelse(min_pts <= 1, 2, min_pts)
min_pts <- ifelse(min_pts >= 10, 10, min_pts)

# ---------------------------------------------------------------------------------------------------
# Normalización de los datos (SIEMPRE HAY QUE HACERLO)
### Cuando trabajamos con distáncias es óptimo normalizar los datos para que ningúno tenga un peso que no le corresponde
datos_norm <- data.frame(lapply(datos, scales::rescale))

### Como podemos ver, ahora tendremos los valores entre el intervalo [0, 1]

# -----------------------------------------------------------------------------
# Calculo de la Epsilon (eps)
### Realizamos el cálculo de las distancias mas cercanas en una matriz de puntos
#### distanciasVecinas <- dbscan::kNNdist(datos, k = min_pts)
 
### Ordenamos los puntos de menos a mayor y lo guardamos en un vector.
### Cuando realicemos el gráfico elbow, será nuestro eje de las Y
#### Y <- distanciasVecinas[order(distanciasVecinas)]

### Calculamos el índice del eje de la X
#### X <- c(0:(length(Y) - 1))

### A continuación calculamos las pendientes
#### pendientes <- c()
#### for (i in 1:length(X)) {
####	pendientes[i] <- (Y[i + 1] - Y[i])/(X[i+1] - X[i])
#### }

#### m <- which.max(pendientes)
#### primer <- gdata::first(which(pendientes >= m))
#### epsilon <- Y[primer]

### Gráficamos los epsilons ordenados
{kNNdistplot(datos, k = 5, minPts = min_pts)
abline(h = 0.15, lty = 2, col = "red")}

### Mirando el gráfico elbow vemos que el epsilon es 0.15
epsilon <- 0.15

# -----------------------------------------------------------------------------
### Volvemos a ejecutar el DBSCAN con los parámetros óptimos
res <- dbscan(datos, eps = epsilon, minPts = min_pts) 

### Añado la columna clúster a mis datos.
datos$cluster <- res$cluster

### Guardo datos limpios.
datos_limpios <- dplyr::filter(datos, cluster != 0)

### Guardo outliers.
outliers <- dplyr::filter(datos, cluster == 0) 

### Graficamos el dbscan obtenido 
fviz_cluster(object = res, data = datos, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")

### Otra manera de visualizar los clusters obtenidos
hullplot(datos, res$cluster, main = paste0("Convex cluster Hulls, eps = ", epsilon))

# =============================================================================
# OPTICS

### Ejecutamos el algoritmo OPTICS con un radio de vecindad de 0.5 y un número mínimo de puntos de 5
optics <- dbscan::optics(datos, eps = 0.5, minPts = 5)
#### optics <- optics::optics(data, eps = 0.5, minPts = 5)

# -----------------------------------------------------------------------------
### Creamos un gráfico que muestra la distancia alcanzable de cada punto
plot(optics, reachability = TRUE)

# -----------------------------------------------------------------------------
### Optimizamos la búsqueda de parámetros para epsilon y minPts en Optics
library(doParallel)
library(foreach)

### Definimos los valores que se van a probar para eps y minPts
eps_values <- seq(0.1, 1.0, by = 0.1)
minPts_values <- seq(5, 20, by = 5)

### Crear una cuadrícula de búsqueda de los valores de eps y minPts
grid <- expand.grid(eps = eps_values, minPts = minPts_values)

### Establecemos el número de núcleos que se van a usar para realizar la optimización en paralelo
cores <- detectCores()
registerDoParallel(cores = cores)

### Creamos una función para ejecutar OPTICS con una combinación de parámetros y calcular el coeficiente de silueta. 
run_optics <- function(data, eps, minPts) {
  optics <- dbscan::optics(data, eps = eps, minPts = minPts)
  res <- dbscan::extractDBSCAN(optics, eps_cl = eps)
  sil <- cluster::silhouette(res$cluster, dist(data))
  return(ifelse(is.na(sil), sil, mean(sil[, 3])))
}
### Con esta función nos permitirá luego paralelizar le proceso

### Ejecutar la cuadrícula de búsqueda en paralelo para la función dada
results <- foreach(i = 1:nrow(grid), .combine = rbind) %dopar% {
  eps <- grid$eps[i]
  minPts <- grid$minPts[i]
  score <- run_optics(datos[, -3], eps, minPts)
  c(eps, minPts, score)
}

results <- results[, c(1:3)]

### Seleccionamos la combinación de parámetros que produjo el mejor resultado
best_params <- grid[which.max(results[, 3]), ]

### Creamos el modelo con los mejores parámetros
optics <- dbscan::optics(datos, eps = best_params$eps, minPts = best_params$minPts)

# -----------------------------------------------------------------------------
### Metodo de la silueta

#### Ejecutar OPTICS para diferentes valores de eps
eps_values <- seq(0.1, 1, by = 0.1)
optics_results <- lapply(eps_values, function(e) optics(datos[, -3], eps = e, minPts = 5))

#### Obtener los agrupamientos para cada valor de eps
clusters <- lapply(optics_results, function(x) extractDBSCAN(x, eps = x$eps))

#### Calcular la medida de silhouette promedio para cada valor de eps
silhouette_avg <- sapply(clusters, function(x) mean(cluster::silhouette(x$cluster, dist(datos[, -3]))))

# Graficar la medida de silhouette promedio en función de eps
plot(eps_values, silhouette_avg, type = "b", pch = 20, main = "Silhouette Plot")

# Agregar una línea vertical en el valor óptimo de eps
opt_eps <- eps_values[which.max(silhouette_avg)]
abline(v = opt_eps, lty = 2, col = "red")


# -----------------------------------------------------------------------------------
### extract a DBSCAN clustering by cutting the reachability plot at eps_cl
res <- dbscan::extractDBSCAN(optics, eps_cl = opt_eps)

### black is noise
plot(res)  

### Visualizamos el gráfico con los grupos creados
dbscan::hullplot(datos, res)

# ==============================================================================

