library(factoextra)
library(cluster)
library(NbClust)
library(clustertend)

setwd("C:/Users/casey/Desktop/Birds")

rm(list=ls())

df <- read.csv("traits_final.csv", row.names = 1)

# Standardize the data sets
df <- traits.scaled <- scale(df)

# Compute Hopkins statistic for dataset
res <- get_clust_tendency(df, n = nrow(df)-1, graph = FALSE)
res$hopkins_stat

# Number of clusters
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

fviz_nbclust(df, kmeans, method = "silhouette") +
  geom_vline(xintercept = 4, linetype = 2)

# Plot data set
fviz_pca_ind(prcomp(df), title = "PCA - Birds of Belfield",
                palette = "npg",
                geom = "point", ggtheme = theme_minimal(),
                legend = "bottom") +
  coord_cartesian(xlim=c(-8, 16), ylim=c(-8, 8))

fviz_pca_biplot(prcomp(df), title = "PCA - Birds of Belfield", 
             palette = "npg", labelsize = 2,
             geom = "point", ggtheme = theme_minimal(),
             legend = "bottom") +
  coord_cartesian(xlim=c(-8, 16), ylim=c(-8, 8))

set.seed(123)
# K-means on dataset
km.res1 <- kmeans(df, 4)
fviz_cluster(list(data = df, cluster = km.res1$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "npg", ggtheme = theme_minimal()) +
  coord_cartesian(xlim=c(-8, 16), ylim=c(-8, 8))

cluster <- as.data.frame(km.res1$cluster)
write.csv(cluster, "clusters.csv")
