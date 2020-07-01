install.packages("readr")
install.packages("corrplot")
install.packages("cluster")
install.packages("ggplot2")
install.packages("psych")
library(readr)
library(corrplot)
library(cluster)
library(ggplot2)
library(psych)

wine <- read.csv("C:\\Users\\91755\\Desktop\\Assignment\\12 - PCA\\wine.csv")
attach(wine)
View(wine)

Wine <- wine[, -1]
attach(Wine)
View(Wine)

summary(Wine)

#Graphical Representation
cor(Wine)
corrplot(Wine, method="color",addCoef.col = "black")
pairs.panels(Wine)

#PCA Model Building
Wine_PCA_model <- princomp(Wine, cor = TRUE, scores = TRUE, covmat = NULL)
summary(Wine_PCA_model)
str(Wine_PCA_model)

#Plotting
plot(Wine_PCA_model)
biplot(Wine_PCA_model)
Wine_PCA_model$scores[, 1:9]
plot(Wine_PCA_model$scores[, 1:9], pch = 18, cex = 0.3, lwd = 3)
text(Wine_PCA_model$scores[, 1:9], labels = c(1:25), cex = 1)

#Combining the PCA with Original Data
mydata <- cbind(Wine, Wine_PCA_model$scores[, 1:9])
View(mydata)

#Preparing for Clustering
clus <- mydata[, 14:22]
View(clus)

#Normalize Data
norm_clus <- scale(clus)

#Calculating Distance Matrix
dist_clus <- dist(norm_clus, method = "euclidean")
dist_clus

#Hierarcheal Clustering Using Default Complete Linakge
fit_clus_wine <- hclust(dist_clus)
plot(fit_clus_wine)
plot(fit_clus_wine, hang = 1)

#Hierarcheal Clustering Using Ward Linkage
fit_clus_wine1 <- eclust(norm_clus, "hclust", k=3
                         method = "ward.D", graph = FALSE)
head(fit_clus_wine1)

#Dendrogram
fviz_dend(fit_clus_wine1, rect = TRUE, show_labels =TRUE, cex = 0.5)

#Membership
group <- cutree(fit_clus_wine1, k=3)
table(group)
membership <- as.matrix(group)

aggregate(norm_clus, list(group), mean)

final1 <- data.frame(membership, wine)
head(final1)
View(final1)

##-----------------------------------------------------------------------------------------------------------------
#K Means Cluster
head(norm_clus)

#nrow(norm_clus) -1 = number of rows
wss = (nrow(norm_clus)-1)*sum(apply(norm_clus, 2, var))
for (i in 2:8) {
  wss[i] = sum(kmeans(norm_clus, centers = i)$withinss)
  print(wss[i])
}
plot(1:8, wss, type="b", xlab = "Number of Cluster", ylab="Within groups sum of squares")
title(sub = "K-Means Clustering Screw-Plot")

fit_clus_wine2 <- eclust(norm_clus, "kmenas", k = 3
                         nstart = 25, graph = FALSE)
fviz_dend(fit_clus_wine2, rect = TRUE, show_labels = TRUE, cex = 0.5)

final2 <- data.frame(fit_clus_wine2, wine)
head(final2)
View(final2)
