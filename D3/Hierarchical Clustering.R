
# HIERARCHICAL CLUSTERING WITH MIXED DATA AND WITH ONLY NUMERICAL VARIABLES

library(readr)
library(dplyr)
library(cluster)
library(factoextra)

install.packages("factoextra")

df <- read_csv("~/GIA/4sem/PMAAD/Scripts/DadesMimmi.csv")
attach(df)
df2 <- subset(df, select = -c(Crash_ID, Crash_Year, Crash_Time, Crash_Month, Crash_Date, Hour_of_Day, Charge))


df2 <- subset(df2, select = -c(Person_Injury_Severity))

agggr <- sapply(df2, class)
# varsCat <- names(agggr)[which(agggr == "character")]
# varslog <- names(agggr)[which(agggr == "logical")]

#for (varCat in varsCat) {
#  df2[, varCat] <- as.factor(df2[, varCat])
#}
#for (varCat in varslog) {
#  df2[, varCat] <- as.factor(df2[, varCat])
#}
# HO HEU DE FER AMB EL FITXER FACTOR, S'HA DE MIRAR ALGUNA MANERA DE QUE ES FACI TOT JUNT

dist_mat <- daisy(df2, metric = "gower", stand=TRUE)
dist_mat <- dist_mat^2


hclust_avg <- hclust(dist_mat, method = 'ward.D2')
plot(hclust_avg)
abline(h = 2, col = 'red')
rect.hclust(hclust_avg , k = 6, border = 2:6)

cut_avg <- cutree(hclust_avg, k = 6)

suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 2)
plot(avg_col_dend)

suppressPackageStartupMessages(library(dplyr))
seeds_df_cl <- mutate(df2, cluster = cut_avg)
count(seeds_df_cl,cluster)


suppressPackageStartupMessages(library(ggplot2))
ggplot(seeds_df_cl, aes(x=, y = perimeter, color = factor(cluster))) + geom_point()

table(seeds_df_cl$cluster,rownames(df2))


# ONLY WITH NUMERICAL VARIABLES
classes <- sapply(df2, class)
num <- names(classes)[which(classes == "numeric")]
dfnum <- subset(df, select = num)

hc.cut <- hcut(dfnum, k = 6, hc_method = "ward.D2")
# Visualize dendrogram
fviz_cluster(object = hc.cut, data = df2, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")

