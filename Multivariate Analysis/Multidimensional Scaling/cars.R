library(tidyverse)
library(readxl)
library(ggrepel)
library(ggpubr)
library(NbClust)
library(smacof)

carros <- read_excel("./Downloads/carros.xls")
View(carros)
summary(carros)

d_carros <- dist(carros[, -c(1, 2)])

for (i in 1:4) {
  print(mds(d_carros, ndim = i)$stress)
}

MDS <- mds(d_carros, ndim = 2)
MDS$conf

coord_carros <- MDS$conf %>% as_tibble()
ggplot(coord_carros, aes(x = D1, y = D2)) +
  geom_point() +
  geom_label_repel(aes(label = carros$Nome)) +
  labs(x = "Dimensão 1", y = "Dimensão 2")

NbClust(data = coord_carros, min.nc = 2, max.nc = 10,
        index = 'all', method = 'ward.D')
coord_carros$clusters <- kmeans(coord_carros, centers = 2,
                                nstart = 100)$cluster %>% as.factor()

ggscatter(coord_carros, x = "D1", y = "D2",
          label = carros$Nome,
          color = "clusters",
          ellipse = TRUE,
          ggtheme = theme_gray())


