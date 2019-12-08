
setwd("~/Acad/Projects/nowthatswhaticallmusic")
set.seed(42)
library(tidyverse)

Data <- read_csv("Now_with_Spotify_Final.csv")

summary(Data)
glimpse(Data)
colnames(Data)

#removing all the rows with NA/Null
Data <- Data[complete.cases(Data),]  

#Finding unique songs
#Unique_songs <- unique(Data$spotify_id)
length(unique(Data$spotify_id))   #counting unique songs
length(unique(Data$title)) 

#Identifying features
index_cols = c("artist", "title", "spotify_id","volume_number","number")
cat_cols = c("mode","key")
num_cols = c("speechiness","duration","time_signature","acousticness","danceability","energy"
             ,"instrumentalness","liveness","loudness","valence","tempo")

#Subsetting numeric data
Num_Data <- Data %>%
  select(num_cols)

#Checking correlation between variables -- Not mandatory
library(corrplot)
M<-cor(Num_Data)
corrplot(M, method="circle")

#Pre-processing the data
boxplot(Num_Data)
Num_Data_Norm <- sapply(Num_Data, scale)   #Normalized Numerical Data
#boxplot(Num_Data_Norm)
summary(Num_Data_Norm)       


#Clustering the data ----

#Finding optimum number of clusters
library(NbClust)
optimum <- NbClust(Num_Data_Norm, distance='euclidean', max.nc=12, method="kmeans")
#table(optimum$Best.nc[1,])
barplot(table(optimum$Best.nc[1,]), xlab="Number of Clusters", ylab="Number of criteria", 
        main="Number of clusters chosen by criteria")


# Perform k-means cluster analysis, set 8 clusters as suggested by above method
fit.km <- kmeans(Num_Data_Norm, 8, nstart=25, iter.max = 30)

Data$Cluster <- fit.km$cluster

#Summary of the clusters
fit.km$size
fit.km$centers
fit.km$totss
fit.km$withinss
fit.km$tot.withinss
fit.km$betweenss
fit.km$iter
fit.km$ifault

#Visualizing clusters
plot(Num_Data, col=Data$Cluster)
#legend(x = 'bottomright', y = 3, legend = fit.km$cluster, col=1:8, pch=1)
#length(unique(Data$Cluster))


# Cluster Plot against 1st 2 principal components
library(cluster) 
clusplot(Num_Data, Data$Cluster, color=TRUE, shade=TRUE, 
         labels=8)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(Num_Data, Data$Cluster)

# Model Based Clustering
library(mclust)
plot(Mclust(Num_Data)) # plot results 
summary(Mclust(Num_Data))
