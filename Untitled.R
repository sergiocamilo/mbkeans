#install.packages("ggplot2")
#install.packages("ClusterR")
#install.packages("cluster")
#install.packages("fpc")

library(ggplot2)
library(fpc)
library("ClusterR")

data <- read.csv("data.csv", header = T)
head(data)


set.seed(3)
km =kmeans(data[,-1],3,nstart=20)
km$cluster

table(km$cluster, data$Country)

ggplot(data, aes(Per.capita.income , Infant.mortality, color=km$cluster , label=Country)) + geom_text(size=3) + scale_colour_gradientn(colours=rainbow(4))

#con datos escalados
k<-5
dataScale<- scale(data[,-1])
set.seed(k)
km2 =kmeans(dataScale,k,nstart=20)
km2$cluster

table(km2$cluster, data$Country)

centers=as.data.frame(km2$centers)

plotcluster(dataScale, km2$cluster)

#ingreso per capita vs alfabetismo 
ggplot(data, aes(dataScale[,1] , dataScale[,2] , color=km2$cluster, label= data[,1])) + geom_text(size=3.5) + scale_colour_gradientn(colours=rainbow(4)) +  xlab("Ingreso per capita") + ylab("Alfabetismo") + ggtitle("Ingreso per capita vs alfabetismo")

#ingreso per capita vs mortalidad infantil
ggplot(data, aes(dataScale[,1] , dataScale[,3] , color=km2$cluster, label= data[,1])) + geom_text(size=3.5) + scale_colour_gradientn(colours=rainbow(4)) +  xlab("Ingreso per capita") + ylab("Mortalidad infantil") + ggtitle("Ingreso per capita vs mortalidad infantil")

#ingreso per capita vs expectativa de vida
ggplot(data, aes(dataScale[,1] , dataScale[,4] , color=km2$cluster, label= data[,1])) + geom_text(size=3.5) + scale_colour_gradientn(colours=rainbow(4)) +  xlab("Ingreso per capita") + ylab("Expectativa de vida") + ggtitle("Ingreso per capita vs expectativa de vida")



MbatchKm = MiniBatchKmeans(dataScale, clusters = 5, batch_size = 5, num_init = 5, early_stop_iter = 10)
clusterMBKM = predict_MBatchKMeans(dataScale, MbatchKm$centroids, fuzzy = FALSE)
plotcluster(dataScale,clusterMBKM)

clusterMB = as.data.frame(as.vector(clusterMBKM))

#ingreso per capita vs alfabetismo 
ggplot(data, aes(dataScale[,1] , dataScale[,2] , color=clusterMB, label= data[,1])) + geom_text(size=3.5) + scale_colour_gradientn(colours=rainbow(4)) +  xlab("Ingreso per capita") + ylab("Alfabetismo") + ggtitle("Ingreso per capita vs alfabetismo")

#ingreso per capita vs mortalidad infantil
ggplot(data, aes(dataScale[,1] , dataScale[,3] , color=clusterMB, label= data[,1])) + geom_text(size=3.5) + scale_colour_gradientn(colours=rainbow(4)) +  xlab("Ingreso per capita") + ylab("Mortalidad infantil") + ggtitle("Ingreso per capita vs mortalidad infantil")

#ingreso per capita vs expectativa de vida
ggplot(data, aes(dataScale[,1] , dataScale[,4] , color=clusterMB, label= data[,1])) + geom_text(size=3.5) + scale_colour_gradientn(colours=rainbow(4)) +  xlab("Ingreso per capita") + ylab("Expectativa de vida") + ggtitle("Ingreso per capita vs expectativa de vida")



