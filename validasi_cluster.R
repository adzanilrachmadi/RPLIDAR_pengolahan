
set.seed(1)

ancluster1 = select(Test.Api1.300,c(1,2))
ancluster2 = select(Test.Api1.600,c(1,2))
ancluster3 = select(Test.Api1.1200,c(1,2))
ancluster4 = select(Test.Api3.1200,c(1,2))
ancluster5 = select(Test.Api5.300,c(1,2))
ancluster6 = select(Test.Api5.600,c(1,2))

val.cluster1 <- kmeans(ancluster1, 5)
val.cluster2 <- kmeans(ancluster2, 5)
val.cluster3 <- kmeans(ancluster3, 5)
val.cluster4 <- kmeans(ancluster4, 8)
val.cluster5 <- kmeans(ancluster5, 8)
val.cluster6 <- kmeans(ancluster6, 7)

val.cluster1$cluster
val.cluster2$cluster
val.cluster3$cluster
val.cluster4$cluster
val.cluster5$cluster
val.cluster6$cluster

table(val.cluster1$cluster)
table(val.cluster2$cluster)
table(val.cluster3$cluster)
table(val.cluster4$cluster)
table(val.cluster5$cluster)
table(val.cluster6$cluster)

val.cluster1$centers #dataset 1m-300rpm
val.cluster2$centers
val.cluster3$centers
val.cluster4$centers
val.cluster5$centers
val.cluster6$centers

library(cluster)
#1
set.seed(1234)
jarak1 <- as.matrix(dist(ancluster1))
sil.41 <- mean(silhouette(val.cluster1$cluster,dmatrix=jarak1)[,3])
print(paste("koef silhouette = ", round(sil.41, 3)))

#2
jarak2 <- as.matrix(dist(ancluster2))
sil.42 <- mean(silhouette(val.cluster2$cluster,dmatrix=jarak2)[,3])
print(paste("koef silhouette = ", round(sil.42, 3)))

#3
jarak3 <- as.matrix(dist(ancluster3))
sil.43 <- mean(silhouette(val.cluster3$cluster,dmatrix=jarak3)[,3])
print(paste("koef silhouette = ", round(sil.43, 3)))

#4
jarak4 <- as.matrix(dist(ancluster4))
sil.44 <- mean(silhouette(val.cluster4$cluster,dmatrix=jarak4)[,3])
print(paste("koef silhouette = ", round(sil.44, 3)))

#5
jarak5 <- as.matrix(dist(ancluster5))
sil.45 <- mean(silhouette(val.cluster5$cluster,dmatrix=jarak5)[,3])
print(paste("koef silhouette = ", round(sil.45, 3)))

#6
jarak6 <- as.matrix(dist(ancluster6))
sil.46 <- mean(silhouette(val.cluster6$cluster,dmatrix=jarak6)[,3])
print(paste("koef silhouette = ", round(sil.46, 3)))


jarak1 <- as.matrix(dist(ancluster1))
evaluasi <- NULL 
for (k in 2:10){
  clustering <- kmeans(ancluster1, centers=k, iter.max=20)
  koef.silh <- mean(silhouette(clustering$cluster,dmatrix=jarak)[,3])
  evaluasi <- rbind(evaluasi, c(k, koef.silh))
}
plot(evaluasi[,1], evaluasi[,2], type="b", xlab="banyaknya cluster", ylab="koefisien silhouette")

#membuat 3 cluster
an.cluster <- kmeans(Test.Api1.300, 5)
#centroid dari setiap cluster
an.cluster$centers

#banyaknya anggota dari setiap cluster
table(an.cluster$cluster)

Test.Api1.300$cluster <- an.cluster$cluster

library(factoextra)
library(FactoMineR)
centroid <- an.cluster$centers
rownames(centroid) <- c("CL1", "CL2", "CL3")
res.pca <- PCA(centroid,  graph = FALSE)
fviz_pca_biplot(res.pca, repel = TRUE)

Angule <- dist(Test.Api1.300, method = 'euclidean')
hcluster <- hclust(Angule, method = 'ward.D2')
plot(hcluster)

