library(readr)
Pharmaceuticals <- read.csv("~/Desktop/Fundamentals of Machine Learning/Pharmaceuticals.csv")
View(Pharmaceuticals)
str(Pharmaceuticals)

#installing necessary packages and libraries
install.packages(c("Rcpp","tidyverse"))
install.packages("factoextra")

library(tidyverse)
library(factoextra)
library(cluster)
library(dplyr)
library(ggplot2)
library(gridExtra)  

#collecting numerical values coloumn 1 to 9
row.names(Pharmaceuticals)<- Pharmaceuticals[,1]
P<- Pharmaceuticals[,3:11]
head(P)

#Scaling the data using scale function
dataframe<- scale(P)
head(dataframe)

#Computing K-means clustering 
kmeans <- kmeans(dataframe, centers = 2, nstart = 25)
kmeans1 <- kmeans(dataframe, centers = 5, nstart = 25)
kmeans2 <- kmeans(dataframe, centers = 6, nstart = 25)

Plot1<-fviz_cluster(kmeans, data = dataframe)+ggtitle("k=2")
Plot2<-fviz_cluster(kmeans1, data = dataframe)+ggtitle("k=5")
Plot3<-fviz_cluster(kmeans2, data = dataframe)+ggtitle("k=6")
#Plot
grid.arrange(Plot1,Plot2,Plot3, nrow = 2)

#Determing optimal clusters using elbow method
distance<- dist(dataframe, method = "euclidean")
#calculating distance matrix between rows of a data matrix
fviz_dist(distance)

#For each k calcluate the total within-cluster sum of square
# appropriate number of clusters k=5

set.seed(64060)
wss<- function(k){kmeans(dataframe, k, nstart = 20)$tot.withinss}

#computing value for k= 1 nd k= 10
k.values<- 1:10
wss_clusters<- map_dbl(k.values, wss)
plot(k.values, wss_clusters, type = "b", pch = 16, 
     frame = TRUE, xlab = "Clusters Number",
     ylab = "Total within clusters")

#Using 5 clusters to extract the results and visualise 
set.seed(64060)
Result<- kmeans(dataframe, 5, nstart = 20)
print(Result)

fviz_cluster(Result, data= dataframe)

P%>%
  mutate(Cluster = Result$cluster) %>%
  group_by(Cluster)%>% summarise_all("mean")

#Plotting
clusplot(dataframe, Result$cluster, color = TRUE, labels = 2, lines =0)


#Clusterform
ClusterFormation<- Pharmaceuticals[,c(12,13,14)]%>% 
  mutate(clusters = Result$cluster)%>%
  arrange(clusters, ascending = TRUE)
ClusterFormation


#is there any pattern in the clusters with respect to numerical values
p_1<-ggplot(ClusterFormation, mapping = aes(factor(clusters),
                                       fill=Median_Recommendation))+geom_bar(position = 'dodge' )+
  labs( x = 'number of clusters')


p_2<-ggplot(ClusterFormation, mapping = aes(factor(clusters),
                                       fill=Location))+geom_bar(position = 'dodge' )+labs(
                                         x = 'number of clusters')



p_3<-ggplot(ClusterFormation, mapping = aes(factor(clusters),
                                       fill=Exchange))+geom_bar(position = 'dodge' )+labs(
                                         x = 'number of clusters')

grid.arrange(p_1,p_2,p_3)



