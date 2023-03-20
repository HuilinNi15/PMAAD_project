install.packages("StatMatch")
install.packages('readxl')



library(readxl)
library(cluster)
library(StatMatch)
file.choose()


path <- "C:\\Users\\user\\Downloads\\Austin Bicycle Crashes 2010-2017.xlsx"
data <- read_excel(path)
#uncompleteVar <- function(vector){any(is.na(vector))}
  # millor utilitzar complete.cases()

# ja existeix una funció en R que et calcula la moda !!!
#Mode <- function(x) 
#{
  #x<-as.factor(x)
  #maxV<-which.max(table(x))
  #return(levels(x)[maxV])
#}

MiMMi <- function(data, priork = -1) {
  
  #Identificar les files que no tenen missings
  complete_rows <- complete.cases(data)
  
  if (sum(complete_rows) == nrow(data)) { # si no hi ha missings, retorna la bd
    print("Non missing values found")
    out <- list(imputedData = data, imputation = NULL)
  } else {
    
    # com que ens hem quedat amb les columnes completes ja no ens cal fer això
    #K<-dim(data)[2] numero de columnes 
    #colsNoMiss<-setdiff(c(1:K),as.vector(colsMiss))
    
    # Eliminar files amb missings
    data_complete <- data[complete_rows, ] # creem nou db sense missings
    
    # Cluster amb les dades completes
      # calculem la matriu de distància i la de dissimilaritat
      # calculem la distància entre variables (gower -> dades mixtes)
    dissimMatrix <- daisy(data_complete, metric = "gower", stand = TRUE)
    #obtenim matriu de distàncies 
    distMatrix <- dissimMatrix^2
    
    # cluster jerarquic aglomeratiu
    hcdata <- hclust(distMatrix, method = "ward.D2")
    # visualitzem dendograma 
    plot(hcdata)}
    
    # nk <- 2 # valor predeterminat de clusters 
    
    # aquí bàsicament el que estem dient és que si hem 
    # posat un valor predeterminat a nk, utilitzarem aquest nk 
    # si no li hem posat, el podrem triar, per tant borrem nk <- 2 
    # ja que si podem triar millor 
    
    if (priork == -1) {
      print("WARNING: See the dendrogramm and ZOOM if required")
      print("and enter a high number of clusters")
      nk <- readline("(must be a positive integer). k: ")
      nk <- as.integer(nk)
    } else {
      nk <- priork
    }
    
    # partim pel lloc que hem decidit 
    partition <- cutree(hcdata, nk)
    
    
  
    #CompleteData<-data
    #nomes cal per tenir tra?a de com s'ha fet la substituci?
    #newCol<-K+1
    #CompleteData[,newCol]<-partition
    #names(CompleteData)[newCol]<-"ClassAux"
    
    #setOfClasses<-as.numeric(levels(as.factor(partition)))
    #imputationTable<-data.frame(row.names=setOfClasses)
    #p<-1 
    
    # HO PODEM SIMPLIFICAR !!!
    # Agreguem una columna a les dades completes que ens dirà de 
    #quina classe es
    data_complete$ClassAux <- partition
    
    # fem un conjunt de les classes que es troben a la partició
    setOfClasses <- unique(partition) 
    # creem una taula que té tantes files com classes
    imputationTable <- data.frame(row.names = setOfClasses)
    # llavors la farem servir per calcular la moda i la mitjana de cada classe 
    
    # imputació de missings
    for (k in 1:ncol(data)) {
      if (sum(is.na(data[, k])) > 0) {
        if (is.numeric(data[, k])) {
          imputingValues <- tapply(data_complete[, k], partition, mean, na.rm = TRUE)
          # guardem els valors mitjans imputats en cada grup
        } else {
          # guardem les modes de cada classe 
          imputingValues <- tapply(data_complete[, k], partition, mode, na.rm = TRUE)
        }
        # per tots aquells nas imputa mitjana, moda 
        data[is.na(data[, k]), k] <- imputingValues[as.character(partition[is.na(data[, k])])]
        imputationTable[, colnames(data)[k]] <- imputingValues
      }
    }
    
    # assignem noms a la taula d'imputació 
    colnames(imputationTable) <- colnames(data)[colSums(is.na(data)) > 0]
    
    # creem taula amb valors ja imputats i taula d'imputació
    out <- list(imputedData = data, imputation = imputationTable)
    return(out)
}
  
  
  
