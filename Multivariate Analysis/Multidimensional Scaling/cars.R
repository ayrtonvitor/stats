library(ggpubr)
library(ggrepel)
library(NbClust)
library(readxl)
library(smacof)
library(tidyverse)
library(vegan)

#transform data
carros <- read_excel("./Downloads/carros.xls")
View(carros)
summary(carros)
head(carros)
d_carros <- dist(scale(carros[, -c(1, 2)]))
d_carros_np <- dist(carros[, -c(1, 2)])

#check if 2 dimensions is good enough for mds
pk <- function(eig) {
  x <- sum(abs(eig))
  return(cumsum(eig) / x)
}
print(pk(cmdscale(d_carros, k = 3, eig = TRUE)$eig)[1:4])

#perform mds
mds <- cmdscale(d_carros, k = 2)
mds[,1:2]

coord_carros <- mds[,1:2] %>% as_tibble()
ggplot(coord_carros, aes(x = V1, y = V2)) +
  geom_point() +
  geom_label_repel(aes(label = carros$Nome)) +
  labs(x = "Dimensão 1", y = "Dimensão 2")

#chose clusters and plot it with data points
NbClust(data = coord_carros, min.nc = 2, max.nc = 10,
        index = 'all', method = 'ward.D')
coord_carros$clusters <- kmeans(coord_carros, centers = 2,
                                nstart = 100)$cluster %>% as.factor()

ggscatter(coord_carros, x = "V1", y = "V2",
          label = carros$Nome,
          color = "clusters",
          ellipse = TRUE,
          ggtheme = theme_gray())

#principal components for biplot
pc <- prcomp(scale(carros[, -c(1, 2)]))
pc
summary(pc)

pca <- princomp(scale(carros[, -c(1, 2)]))
biplot(pca)

#procrustes analysis of mds vs biplot
proc <- procrustes(mds[,1:2], pca$scores[, 1:2])
proc
summary(proc)

plot(proc)
residuals(proc)

protest(mds[,1:2], pca$scores[,1:2])
