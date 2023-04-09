
# PCA 
# https://www.datacamp.com/tutorial/pca-analysis-r

install.packages("corrr")
library(corrr)

install.packages("ggcorrplot")
library(ggcorrplot)
library(corrplot)
library(ggrepel)

install.packages("FactoMineR")
install.packages("devtools")
library(FactoMineR)
library(factoextra)
library("devtools")

library("httpgd")

# Preparing the data ---------------------------------------------------------------------

df <- './Datasets/cleaned_db.csv'
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
corrplot(corr_matrix, type="upper", order="hclust", tl.col="black", tl.srt=45, tl.cex=2, cl.cex=2)


# PCA ------------------------------------------------------------------------------------
data.pca <- princomp(corr_matrix)
summary(data.pca)


# the Cumulative Proportion has to be the 80%, so we take 6 principal components (82%) 
data.pca$loading[, 1:6]

# Scree Plot -----------------------------------------------------------------------------

# to visualize the importance of each Principal Component
# it can also be used to determine the number of principal components to retain

inertia <- cumsum(data.pca$sdev^2/sum(data.pca$sdev^2))

# Create data frame for plotting
inertias <- data.frame(Dimension = 1:length(inertia), Inertia = inertia)

# Plot cumulative inertia
ggplot(inertias, aes(x = Dimension, y = Inertia, fill = Dimension)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", Inertia * 100)), hjust = +0.5, vjust=-0.4, size = 10) +
  scale_fill_gradient(low = "#F2C9CC", high = "#8B0000") +
  labs(x = "Number of Dimensions", y = "Cumulative Inertia (%)") +
  ggtitle("Cumulative innertia of the PCA") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25),
        legend.position = "none")


fviz_eig(data.pca, addlabels = TRUE, hjust = -0.3)
# it shows the eigenvalues in a downward curve
# 20.1 + 15.8 + 14.1 + 12.4 + 10.7 + 9.6 = 82.7
# so we keep the first 6 principal components 


# How each variable is represented by the different dimensions. 
var <- get_pca_var(data.pca)
var
corrplot(var$cos2[,1:6], is.corr=FALSE, tl.cex=2, cl.cex=1.5)


# Biplot of the attributes ---------------------------------------------------------------

gradient <- c("#00AFBB", "#E7B800", "#FC4E07")

#  Graph of the variables
fviz_pca_var(data.pca, col.var = "contrib", gradient.cols = gradient, axes = 1:2, 
            repel = TRUE, label = "none") +
    geom_text_repel(aes(label = colnames(data_normalized)), size = 8) +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25))

fviz_pca_var(data.pca, col.var = "contrib", gradient.cols = gradient, axes = 3:4, 
            repel = TRUE, label = "none") +
    geom_text_repel(aes(label = colnames(data_normalized)), size = 8) +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25))

fviz_pca_var(data.pca, col.var = "contrib", gradient.cols = gradient, axes = 5:6, 
            repel = TRUE, label = "none") +
    geom_text_repel(aes(label = colnames(data_normalized)), size = 8) +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25))

fviz_pca_var(data.pca, col.var = "contrib", gradient.cols = gradient, axes = c(1, 3), 
            repel = TRUE, label = "none") +
    geom_text_repel(aes(label = colnames(data_normalized)), size = 8) +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25))

fviz_pca_var(data.pca, col.var = "contrib", gradient.cols = gradient, axes = c(2, 3), 
            repel = TRUE, label = "none") +
    geom_text_repel(aes(label = colnames(data_normalized)), size = 8) +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 25))

fviz_pca_var(data.pca, col.var = "contrib", gradient.cols = gradient, axes = c(1, 4)) # dims 1 & 4
fviz_pca_var(data.pca, col.var = "contrib", gradient.cols = gradient, axes = c(1, 5)) # dims 1 & 5
fviz_pca_var(data.pca, col.var = "contrib", gradient.cols = gradient, axes = c(1, 6)) # dims 1 & 6

# Contribution of each variable
fviz_cos2(data.pca, choice = "var", axes = 1:2) # dims 1 & 2
fviz_cos2(data.pca, choice = "var", axes = 3:4) # dims 3 & 4
fviz_cos2(data.pca, choice = "var", axes = 5:6) # dims 5 & 6

# Biplot combined with cos2
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("blue", "green", "red"),
             repel = TRUE)