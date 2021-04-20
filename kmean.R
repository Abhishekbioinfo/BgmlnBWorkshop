library(tidyverse)
library(factoextra)
data(mtcars)
mtcars = as.data.frame(mtcars %>% select_if(is.numeric))
fviz_nbclust(mtcars, kmeans, method ="silhouette", k.max = 14)+ggtitle("Optimal Number of Clusters for Mtcars")
km.res <- eclust(mtcars, "kmeans", k = 2, nstart = 25, graph = FALSE)
fviz_cluster(km.res, data = mtcars,geom = c("text","point"), ggtheme = theme_minimal(), main = "Partitioning Clustering Plot",ellipse= FALSE)
fviz_silhouette(km.res, print.summary = FALSE) + scale_fill_brewer(palette = "Dark2") + scale_color_brewer(palette = "Dark2") + theme_minimal()+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# Silhouette information
silinfo <- km.res$silinfo
names(silinfo)
# Silhouette widths of each observation
head(silinfo$widths[, 1:3], 10)
# Average silhouette width of each cluster
silinfo$clus.avg.widths
# The total average (mean of all individual silhouette widths)
silinfo$avg.width
# The size of each clusters
km.res$size
