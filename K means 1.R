# Read offers and transaction data
offers<-read.csv(file="OfferInformation.csv")
transactions<-read.csv(file="Transactions.csv")
head(offers)
head(transactions)
# Need to convert txns into frequency matrix
library(reshape)
# Preparing data for clustering: Melt transactions, cast customers by offers 
pivot<-melt(transactions[1:2])
pivot<-(cast(pivot,Customer.Last.Name~value,fill=0,fun.aggregate=length))
head(pivot)
# Create Clusters
library(fpc)
# We will run KMeans using pamk (more robust) with 4 clusters. 
cluster.kmeans<-pamk(pivot,k=4)
cluster.kmeans
# Use this to view the clusters
cluster.kmeans$pamobject$clusinfo
plotcluster(pivot,cluster.kmeans$pamobject$clustering)
# combining customer+offer+cluster in one view
cluster.deals<-merge(transactions[1:2],cluster.kmeans$pamobject$clustering,by.x = "Customer.Last.Name", by.y = "row.names")
colnames(cluster.deals)<-c("Name","Offer","Cluster")

# Top deals by cluster
cluster.pivot<-melt(cluster.deals,id=c("Offer","Cluster"))
cluster.pivot<-cast(cluster.pivot,Offer~Cluster,fun.aggregate=length)
head(cluster.pivot)
cluster.topDeals<-cbind(offers,cluster.pivot[-1])
write.csv(file="topdeals.csv",cluster.topDeals,row.names=F)
# Conclusion
# Cluster 3 has strong preference for Pinot Noir offer, Cluster 4 for Champagne. 

## Creating model using kmeans() from stats package

library(stats)
set.seed(101)
txn.cluster<-kmeans(pivot[-1],3)
txn.cluster$size
txn.cluster$centers
plotcluster(pivot,txn.cluster$cluster)
# Customer last name + Cluster
cust.cluster<-cbind(pivot[1],txn.cluster$cluster)
cust.cluster
# Customer + Offer + Cluster
cluster.offers<-merge(transactions,cust.cluster,by="Customer.Last.Name")
colnames(cluster.offers)<-c("Name","Offer","Cluster")
# Top deals by cluster
clustertopdeals<-melt(cluster.offers,id=c("Offer","Cluster"))
clustertopdeals<-cast(clustertopdeals,Offer~Cluster,fun.aggregate = length)
head(clustertopdeals)
clustertopdeals<-cbind(offers,clustertopdeals[-1])
write.csv(file="topdeals.csv",clustertopdeals,row.names=F)
# Conclusion: similar to above method

# Determining optimal number of clusters using NbClust
temp<-NbClust(data=pivot[-1],min.nc=2,max.nc = 8,method = "kmeans")

# Using Elbow method for determining # of clusters
k.max<-15
data<-pivot[-1]
wss<-sapply(1:k.max, 
            function(k){kmeans(data, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)
abline(v=4, lty=2)
