library(stats)
library(ggplot2)
library(plyr)
library(dplyr)
install.packages("data.table")
library(data.table)
install.packages("corrplot")
library(corrplot)
install.packages("factoextra")
library(factoextra)
install.packages("NbClust")
library(NbClust)
install.packages("cluster")
library (cluster)

df <-read.csv("C:\\Cluster.csv",na.strings = c("NA","NAN",""),stringsAsFactors = T)

str(df)
summary(is.na(df))
subset<-as.data.frame(df[,c("Cat1","Cat2","Cat3","Cat4","SqFoot")])
summary(subset)
subset<- as.data.frame(scale(subset))
summary(subset)
set.seed(100)

fviz_nbclust(subset, kmeans, method = "wss")+labs(subtitle = "Elbow method")

# Silhouette method to identify the optimum no of cluster
fviz_nbclust(subset, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

kmean2.simple <- kmeans(subset,centers=4, iter.max = 25, nstart=100)
subset$cluster <- factor(kmean2.simple$cluster)
summary(subset)
set.seed(111)
kmean2.simple

#Helps to understand the size of per cluster
km.res <- kmeans(subset, 4, nstart = 25)
head(km.res$cluster, 4)
km.res$size

#HElps to underrstand the which cat is in which cluster
dd <- cbind(df, cluster = km.res$cluster)
head(dd)

#Plots the 4 Cluster with the point variablity
clusplot(subset, km.res$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

D<- daisy(subset)
plot(silhouette(kmean2.simple$cluster, D),col=1:2, border = NA)
