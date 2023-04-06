
# PCA 
# https://www.datacamp.com/tutorial/pca-analysis-r

install.packages("corrr")
library(corrr)

install.packages("ggcorrplot")
library(ggcorrplot)
library(corrplot)

install.packages("FactoMineR")
library(FactoMineR)
library(factoextra)

# Preparing the data ---------------------------------------------------------------------

df <- './cleaned_db.csv'
data <- read.csv(df)
str(data)

# Checking if there are any NA's
colSums(is.na(data))

# Normalizing the data
num_cols <- unlist(lapply(data, is.numeric))
num_cols

numerical_data <- data[ , num_cols]
numerical_data <- numerical_data[, -1] # Removing the first column "Crash_ID"
head(numerical_data)

data_normalized <- scale(numerical_data)
head(data_normalized)

# Correlation matrix
corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)
corrplot(corr_matrix, type="upper", order="hclust", tl.col="black", tl.srt=45)


# PCA ------------------------------------------------------------------------------------
data.pca <- princomp(corr_matrix)
summary(data.pca)


# the Cumulative Proportion has to be the 80%, so we take 6 principal components (82%) 
data.pca$loading[, 1:6]

# Scree Plot -----------------------------------------------------------------------------

# to visualize the importance of each Principal Component
# it can also be used to determine the number of principal components to retain

fviz_eig(data.pca, addlabels = TRUE)
# it shows the eigenvalues in a downward curve
# 20.1 + 15.8 + 14.1 + 12.4 + 10.7 + 9.6 = 82.7
# so we keep the first 6 principal components 


# How each variable is represented by the different dimensions. 
var <- get_pca_var(data.pca)
var
corrplot(var$cos2[,1:6], is.corr=FALSE)


# Biplot of the attributes ---------------------------------------------------------------

gradient <- c("#00AFBB", "#E7B800", "#FC4E07")

#  Graph of the variables
fviz_pca_var(data.pca, col.var = "contrib", gradient.cols = gradient, axes = 1:2) # dims 1 & 2
fviz_pca_var(data.pca, col.var = "contrib", gradient.cols = gradient, axes = 3:4) # dims 3 & 4
fviz_pca_var(data.pca, col.var = "contrib", gradient.cols = gradient, axes = 5:6) # dims 5 & 6

# Contribution of each variable
fviz_cos2(data.pca, choice = "var", axes = 1:2) # dims 1 & 2
fviz_cos2(data.pca, choice = "var", axes = 3:4) # dims 3 & 4
fviz_cos2(data.pca, choice = "var", axes = 5:6) # dims 5 & 6

# Biplot combined with cos2
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("blue", "green", "red"),
             repel = TRUE)