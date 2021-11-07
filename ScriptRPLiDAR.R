#Script RPLiDAR Adzanil

setwd("D:/S2/Tesis/dataeksperimen")
getwd()


#library for Cluster Analysis
library(cluster)
library(factoextra)
library(tidyverse)

#library for plot
library(gridExtra)
library(ggplot2)

library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(readxl)
library(ggpubr)

#prerequisiete packages and library
install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")

#Import dan deklarasi variabel
Test.Api1.300 <- read_excel("1-300.xlsx")
Test.Api1.600 <- read_excel("1-600.xlsx")
Test.Api1.1200 <- read_excel("1-1200.xlsx")
Test.Api3.1200 <- read_excel("3-1200.xlsx")
Test.Api5.300 <- read_excel("5-300.xlsx")
Test.Api5.600 <- read_excel("5-600.xlsx")
Test.Api5.1200 <- read_excel("5-1200.xlsx")


#Data tidak digunakan
#Test.Api2.300 <- read_excel("2-300.xlsx")
#Test.Api2.600 <- read_excel("2-600.xlsx")
#Test.Api2.1200 <- read_excel("2-1200.xlsx")
#Test.Api3.300 <- read_excel("3-300.xlsx")
#Test.Api3.600 <- read_excel("3-600.xlsx")
#Test.Api4.300 <- read_excel("4-300.xlsx")
#Test.Api4.600 <- read_excel("4-600.xlsx")
#Test.Api4.1200 <- read_excel("4-1200.xlsx")
#Test.Api5.1200 <- read_excel("5-1200.xlsx")

#unused data sumstat syntax
#summary(Test.Api2.300)
#summary(Test.Api2.600)
#summary(Test.Api2.1200)
#summary(Test.Api5.1200)
#summary(Test.Api4.300)
#summary(Test.Api4.600)
#summary(Test.Api4.1200)
#summary(Test.Api3.600)

#Summary statistics
summary(Test.Api1.300)
summary(Test.Api1.600)
summary(Test.Api1.1200)
summary(Test.Api3.1200)
summary(Test.Api5.300)
summary(Test.Api5.600)
summary(Test.Api5.1200)



#Bar PLot

barplot1 <- ggplot(Test.Api1.300, aes(x = Angule)) + geom_bar(aes(fill = Jenis)) +
  theme(axis.text.x = element_text(angle = 90))
barplot2 <- ggplot(Test.Api1.600, aes(x = Angule)) + geom_bar(aes(fill = Jenis)) +
  theme(axis.text.x = element_text(angle = 90))
barplot3 <- ggplot(Test.Api1.1200, aes(x = Angule)) + geom_bar(aes(fill = Jenis)) +
  theme(axis.text.x = element_text(angle = 90))
barplot4 <- ggplot(Test.Api3.1200, aes(x = Angule)) + geom_bar(aes(fill = Jenis)) +
  theme(axis.text.x = element_text(angle = 90))
barplot4
barplot5 <- ggplot(Test.Api5.300, aes(x = Angule)) + geom_bar(aes(fill = Jenis)) +
  theme(axis.text.x = element_text(angle = 90))
barplot6 <- ggplot(Test.Api5.600, aes(x = Angule)) + geom_bar(aes(fill = Jenis)) +
  theme(axis.text.x = element_text(angle = 90))
barplot7 <- ggplot(Test.Api5.1200, aes(x = Angule)) + geom_bar(aes(fill = Jenis)) +
  theme(axis.text.x = element_text(angle = 90))

#with remove text
ggarrange (barplot1, barplot2, barplot3+rremove("x.text"),labels = c("A","B","C"),ncol=1, nrow=3)

#no remove text
ggarrange (barplot5, barplot6,barplot7, labels = c("A","B","C"),ncol=1, nrow=3)


#Scatter Plot

colors <- c("Non-Polutan" = "red",
            "Polutan" = "green")

plot1 <- ggplot(Test.Api1.300) +
  geom_point(aes(x = Angule, y = Distance, color = Jenis))+ scale_color_manual(values = colors) + labs(title = 'Jarak 1m-300rpm')

plot2 <- ggplot(Test.Api1.600) +
  geom_point(aes(x = Angule, y = Distance, color = Jenis))+ scale_color_manual(values = colors) + labs(title = 'Jarak 1m-600rpm')

plot3 <- ggplot(Test.Api1.1200) +
  geom_point(aes(x = Angule, y = Distance, color = Jenis))+ scale_color_manual(values = colors) + labs(title = 'Jarak 1m-1200rpm')

plot4 <- ggplot(Test.Api3.1200) +
  geom_point(aes(x = Angule, y = Distance, color = Jenis))+ scale_color_manual(values = colors) + labs(title = 'Jarak 3m-1200rpm')
plot4
plot5 <- ggplot(Test.Api5.300) +
  geom_point(aes(x = Angule, y = Distance, color = Jenis))+ scale_color_manual(values = colors) + labs(title = 'Jarak 5m-300rpm')

plot6 <- ggplot(Test.Api5.600) +
  geom_point(aes(x = Angule, y = Distance, color = Jenis))+ scale_color_manual(values = colors) + labs(title = 'Jarak 5m-600rpm')
plot7 <- ggplot(Test.Api5.1200) +
  geom_point(aes(x = Angule, y = Distance, color = Jenis))+ scale_color_manual(values = colors) + labs(title = 'Jarak 5m-600rpm')

#with remove text
ggarrange (plot1, plot2, plot3+rremove("x.text"),labels = c("A","B","C"),ncol=1, nrow=3)

#without remove text
ggarrange (plot5, plot6, plot7, labels = c("A","B","C"),ncol=1, nrow=3)



#begin process the data

View(Test.Api1.300)
mydata1 = select(Test.Api1.300,c(1,2))

#K-Means Cluster Process
set.seed(123)
# function to compute total within-cluster sum of square 
wss <- function(k){
  kmeans(mydata1,k,nstart = 10)$tot.withinss
}
# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values,wss)

plot(k.values, wss_values,
     type="b", pch="19", frame=FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#column select
sel_column<-select(Test.Api1.300, c("Angule", "Distance"))
sel_column1<-select(Test.Api1.600, c("Angule", "Distance"))
sel_column2<-select(Test.Api1.1200, c("Angule", "Distance"))
sel_column3<-select(Test.Api3.1200, c("Angule", "Distance"))
sel_column4<-select(Test.Api5.300, c("Angule", "Distance"))
sel_column5<-select(Test.Api5.600, c("Angule", "Distance"))
sel_column6<-select(Test.Api5.1200, c("Angule", "Distance"))

#elbow method
set.seed(123)
elb1<-fviz_nbclust(sel_column, kmeans, method = "wss") + ggtitle("a.Elbow Method 1m-300rpm")
elb2<-fviz_nbclust(sel_column1, kmeans, method = "wss") + ggtitle("a.Elbow Method 1m-600rpm")
elb3<-fviz_nbclust(sel_column2, kmeans, method = "wss") + ggtitle("a.Elbow Method 1m-1200rpm")
elb4<-fviz_nbclust(sel_column3, kmeans, method = "wss") + ggtitle("a.Elbow Method 3m-1200rpm")
elb5<-fviz_nbclust(sel_column4, kmeans, method = "wss") + ggtitle("a.Elbow Method 5m-300rpm")
elb6<-fviz_nbclust(sel_column5, kmeans, method = "wss") + ggtitle("a.Elbow Method 5m-600rpm")
elb7<-fviz_nbclust(sel_column6, kmeans, method = "wss") + ggtitle("a.Elbow Method 5m-1200rpm")

#average silhouette method
asm1<-fviz_nbclust(sel_column, kmeans, method = "silhouette")+ ggtitle("b.Silhouette 1m-300rpm")
asm2<-fviz_nbclust(sel_column1, kmeans, method = "silhouette")+ ggtitle("b.Silhouette 1m-600rpm")
asm3<-fviz_nbclust(sel_column2, kmeans, method = "silhouette")+ ggtitle("b.Silhouette 1m-1200rpm")
asm4<-fviz_nbclust(sel_column3, kmeans, method = "silhouette")+ ggtitle("b.Silhouette 3m-1200rpm")
asm5<-fviz_nbclust(sel_column4, kmeans, method = "silhouette")+ ggtitle("b.Silhouette 5m-300rpm")
asm6<-fviz_nbclust(sel_column5, kmeans, method = "silhouette")+ ggtitle("b.Silhouette 5m-600rpm")
asm7<-fviz_nbclust(sel_column6, kmeans, method = "silhouette")+ ggtitle("b.Silhouette 5m-1200rpm")

#gap statistic method
set.seed(123)
gap_stat1 <- clusGap(sel_column, FUN = kmeans, nstart = 25,
                     K.max = 10, B = 50)
gap_stat2 <- clusGap(sel_column1, FUN = kmeans, nstart = 25,
                     K.max = 10, B = 50)
gap_stat3 <- clusGap(sel_column2, FUN = kmeans, nstart = 25,
                     K.max = 10, B = 50)
gap_stat4 <- clusGap(sel_column3, FUN = kmeans, nstart = 25,
                     K.max = 10, B = 50)
gap_stat5 <- clusGap(sel_column4, FUN = kmeans, nstart = 25,
                     K.max = 10, B = 50)
gap_stat6 <- clusGap(sel_column5, FUN = kmeans, nstart = 25,
                     K.max = 10, B = 50)
gap_stat7 <- clusGap(sel_column6, FUN = kmeans, nstart = 25,
                     K.max = 10, B = 50)
  # create variable to Print plot the result
  pgs1<-print(gap_stat1, method = "firstmax")
  pgs2<-print(gap_stat2, method = "firstmax")
  pgs3<-print(gap_stat3, method = "firstmax")
  pgs4<-print(gap_stat4, method = "firstmax")
  pgs5<-print(gap_stat5, method = "firstmax")
  pgs6<-print(gap_stat6, method = "firstmax")
  pgs7<-print(gap_stat7, method = "firstmax")
  #create variable with plot result
  gaps1<-fviz_gap_stat(gap_stat1)+ ggtitle("c.Gap Statistic 1m-300rpm")
  gaps2<-fviz_gap_stat(gap_stat2)+ ggtitle("c.Gap Statistic 1m-600rpm")
  gaps3<-fviz_gap_stat(gap_stat3)+ ggtitle("c.Gap Statistic 1m-1200rpm")
  gaps4<-fviz_gap_stat(gap_stat4)+ ggtitle("c.Gap Statistic 3m-1200rpm")
  gaps5<-fviz_gap_stat(gap_stat5)+ ggtitle("c.Gap Statistic 5m-300rpm")
  gaps6<-fviz_gap_stat(gap_stat6)+ ggtitle("c.Gap Statistic 5m-600rpm")
  gaps7<-fviz_gap_stat(gap_stat7)+ ggtitle("c.Gap Statistic 5m-1200rpm")

#extracting resilt
set.seed(123)
final1 <- kmeans(sel_column, 5, nstart = 25)
final2 <- kmeans(sel_column1, 5, nstart = 25)
final3 <- kmeans(sel_column2, 5, nstart = 25)
final4 <- kmeans(sel_column3, 8, nstart = 25)
final5 <- kmeans(sel_column4, 8, nstart = 25)
final6 <- kmeans(sel_column5, 7, nstart = 25)
final7 <- kmeans(sel_column6, 10, nstart = 25)

#extract cluster means and size
print(final1)
print(final2)
print(final3)
print(final4)
print(final5)
print(final6)

clust1<-fviz_cluster(final1, data = sel_column)+ ggtitle("Hasil Klaster 1m-300rpm")
clust2<-fviz_cluster(final2, data = sel_column1)+ ggtitle("Hasil Klaster 1m-600rpm")
clust3<-fviz_cluster(final3, data = sel_column2)+ ggtitle("Hasil Klaster 1m-1200rpm")
clust4<-fviz_cluster(final4, data = sel_column3)+ ggtitle("Hasil Klaster 3m-1200rpm")
clust5<-fviz_cluster(final5, data = sel_column4)+ ggtitle("Hasil Klaster 5m-300rpm")
clust6<-fviz_cluster(final6, data = sel_column5)+ ggtitle("Hasil Klaster 5m-600rpm")
clust7<-fviz_cluster(final7, data = sel_column6)+ ggtitle("Hasil Klaster 5m-1200rpm")

# plot the cluster into multiplot (1m) / run the line one by one  
grid.arrange(elb1, asm1, gaps1, ncol=1)
grid.arrange(elb2, asm2, gaps2, ncol=1)
grid.arrange(elb3, asm3, gaps3, ncol=1)

# plot the cluster into multiplot (3m) / run the line one by one
grid.arrange(elb4, asm4, gaps4, ncol=1)

# plot the cluster into multiplot (5m) / run the line one by one
grid.arrange(elb5, asm5, gaps5, ncol=1)
grid.arrange(elb6, asm6, gaps6, ncol=1)
grid.arrange(elb7, asm7, gaps7, ncol=1)

#plot the k-means cluster (1m)
grid.arrange(clust1, clust2, clust3, ncol=2)


clust1
clust2
clust3
clust4
clust5
clust6
clust7


#plot the k-means cluster (3m)
grid.arrange(clust4, ncol=1)

#plot the k-means cluster (5m)
grid.arrange(clust5, clust6, ncol=2)



plot2 <- ggplot(Test.Api1.600, aes(x = Angule)) + geom_bar(aes(fill = Distance)) +
  theme(axis.text.x = element_text(angle = 90)) + labs(title = "Scatter Plot 1m-600rpm")

ggplot(Test.Api1.300, aes(Angule, Distance)) + geom_boxplot()
plot3 <- ggplot(Test.Api1.1200, aes(Angule, Distance)) + geom_boxplot()

ggplot(Test.Api1.300, mapping = aes(x = `Angule`, y = Distance, color = `Angule`)) + geom_point() +
  labs(title = "Scatter Plot RPLiDAR - 300rpm")

scplot1<- ggplot(Test.Api1.300, mapping = aes(x = `Angule`, y = Distance, color = `Angule`)) + geom_point() 
scplot2<- ggplot(Test.Api1.600, mapping = aes(x = `Angule`, y = Distance, color = `Angule`)) + geom_point() 
scplot3<- ggplot(Test.Api1.1200, mapping = aes(x = `Angule`, y = Distance, color = `Angule`)) + geom_point()





#wssplot1 <- function(data, nc=15, seed=123)
#{
  #wss <- (nrow(data)-1)*sum(apply(data,2,var))
  #for (i in 2:nc) {
    #set.seed(seed)
    #wss[i] <- sum(kmeans(data,centers = i)$withinss)}
  #plot(1:nc, wss, type="b", xlab="Number of Clusters (K)",ylab="within the group of squares")
#}

#wssplot1(Test.Api1.300)

#menentukan jumlah cluster menggunakan metode silhouette
#set.seed(123)
#fviz_nbclust(Test.Api1.300, kmeans, method = "silhouette")
#fviz_nbclust(Test.Api1.600, kmeans, method = "silhouette")
#fviz_nbclust(Test.Api1.1200, kmeans, method = "silhouette")

#sil1.300 <- mean(silhouette(Test.Api1.300))

  # 1 meter 300 rpm
  #silhouette_score <- function(k){
   # km <- kmeans(Test.Api1.300, centers = k, nstart=25)
    #ss <- silhouette(km$cluster, dist(Test.Api1.300))
    #mean(ss[, 3])
  #}
  #k1.300 <- 2:10
  #avg_sil1.300 <- sapply(k, silhouette_score)
  #plot(k1.300, type='b', avg_sil1.300, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

  
  
  #ggarrange(k1.300, k1.600, k1.1200+rremove("x.text"),labels = c("A","B","C"),ncol=2, nrow=2)
  

#menentukan jumlah cluster menggunakan metode Elbow
#set.seed(123)
#fviz_nbclust(Test.Api1.300, kmeans, method = "wss")

# compute gap statistic
#set.seed(123)
#gap_stat <- clusGap(Test.Api1.5meter, FUN = kmeans, nstart = 25,
 #                   K.max = 10, B = 50)
# Print the result
#print(gap_stat, method = "firstmax")
#print(gap_stat)
#fviz_gap_stat(gap_stat)

# Compute k-means clustering with k = 4
#set.seed(123)
#melakukan transformasi scale
#table <- scale(Test.Api1.5meter)
#hapus kolom ke-3
#table[3] <- NULL

#final <- kmeans(table, 4, nstart = 25)
#print(final)
#fviz_cluster(final, data = table)

#Naive Bayes
library(caret)
library(e1071)

sampel <- sample(1:nrow(Test.Api1.300),0.8*nrow(Test.Api1.300),replace=TRUE)
sampelplus <- data.frame(Test.Api1.300)[sampel,]
sampelminus <- data.frame(Test.Api1.300)[-sampel,]
NB <- naiveBayes(Distance~.,data=sampelplus)
prediksi <- predict(NB, sampelminus)
hasil <- confusionMatrix(table(prediksi,sampelminus$Distance))
hasil
