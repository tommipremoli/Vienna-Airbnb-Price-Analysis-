#Robust Clustering

library(tclust)
library(ggplot2)

vienna_listings <- read.csv("C:/Users/Honest/Downloads/vienna_listings_no_outliers.csv")

vienna_listings_final <- vienna_listings[,c(3,21)]
vienna_listings_final <-scale(vienna_listings_final)
set.seed(200)
vienna_clus <- vienna_listings_final[sample(nrow(vienna_listings_final), 200), ]




clus.a <- tclust (vienna_clus, k = 3, alpha = 0.1, restr.fact =  1,
                  restr = "eigen", equal.weights = TRUE)
clus.b <- tclust (vienna_clus, k = 3, alpha = 0.1, restr.fact =  1,
                  equal.weights = TRUE)
clus.c <- tclust (vienna_clus, k = 3, alpha = 0.1, restr.fact =  1,
                  restr = "deter", equal.weights = TRUE)
clus.d <- tclust (vienna_clus, k = 3, alpha = 0.1, restr.fact = 50,
                  restr = "eigen", equal.weights = FALSE)

pa <- par (mfrow = c (2, 2))
plot (clus.a, main = "(a) tkmeans")
plot (clus.b, main = "(b) Gallegos and Ritter")
plot (clus.c, main = "(c) Gallegos")
plot (clus.d, main = "(d) tclust")



clustk <- tkmeans(vienna_clus, k = 4, alpha = 0.1)

pairs(vienna_clus, col = clustk$cluster+1)

plot (vienna_clus[,1], vienna_clus[,11],col = clustk$cluster + 1)










#Model Based Clustering

library(ggplot2)
library(factoextra)
library(mclust)

vienna_listings <- read.csv("C:/Users/Honest/Downloads/vienna_listings_no_outliers.csv")

library(robustbase)

vienna_listings_final <- vienna_listings[,c(3,21)]
vienna_listings_final <-scale(vienna_listings_final)
set.seed(200)
vienna_clus <- vienna_listings_final[sample(nrow(vienna_listings_final), 200), ]


mc <- Mclust(vienna_clus)


summary(mc)


# BIC values used for choosing the number of clusters
fviz_mclust(mc, "BIC", palette = "jco")
# Classification: plot showing the clustering
fviz_mclust(mc, "classification", geom = "point", 
            pointsize = 1.5, palette = "jco")
# Classification uncertainty
fviz_mclust(mc, "uncertainty", palette = "jco")
