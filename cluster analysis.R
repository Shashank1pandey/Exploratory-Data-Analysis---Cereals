library(ggplot2)
library(dplyr)
library(plyr)
library(DMwR)
library(PCAmixdata)
library(factoextra)

setwd("C:/Users/Shashank/Desktop/project")
getwd()
pcr <- read.csv("cereals_data.csv")

# Changing column names for better understanding of visualizations
colnames(pcr) <- c("Product's Name","Manufacturer","Type","Calories","Protein","Fat",
                   "Sodium","Fiber","Carbohydrates","Sugars","Potass","Vitamins","Shelf","Weight",
                   "Cups","Rating")

# Vitamins and shelf are factors so we need to change its data type
pcr <- transform(pcr, Shelf=as.factor(Shelf), Vitamins=as.factor(Vitamins))

# Changing h and c in type column as hot and cold
pcr$Type <- revalue(pcr$Type, c("H"="Hot","C"="Cold"))

# Changing the companies name to real name
pcr$Manufacturer <- revalue(pcr$Manufacturer,c("N"="Nabisco","Q"="Quaker Oats","K"="Kelloggs", "R"="Ralston Purina","G"="General Mills","P"="Post","A"="AHFP"))

pcr <- knnImputation(pcr[,c(-1,-2,-3)])
sum(is.na(pcr))

# Principal Component Analysis
# Storing the data into 2 different quantitative and qualitative matrix
quanti <- pcr %>% select(Calories,Protein,Fat,Sodium,Fiber,Carbohydrates,Sugars,Potass,Weight,Cups,Rating)
head(quanti,3)
quali <- pcr %>% select(Vitamins,Shelf)
prca <- PCAmix(X.quanti=quanti, X.quali=quali, ndim=5, graph=T)
summary(prca)

# Looking at the the graph we can choose rating, calories, fiber, potass, weight as our variables for cluster analysis
c_cluster <- pcr %>% select(Rating, Calories, Fiber, Potass,Weight)
rownames(c_cluster) <- c_cluster$Product.s.Name
c_cluster$Calories <- as.numeric(c_cluster$Calories)
View(c_cluster)
c_cluster <- c_cluster[ ,-1]
View(pcr)

# Scaling so that the data points are in a particular range
set.seed(100)
ss = sample(1:77, 30)
df1 = c_cluster[ss,]
head(df1,3)
df1.scaled = scale(df1)
head(df1.scaled,3)

# Euclidean Distance
dist.eucl_15 = dist(df1.scaled, method='euclidean')
head(dist.eucl_15,3)
dist.eucl_15[is.na(dist.eucl_15)] <- 0 # Changing the NA values to 0
round(as.matrix(dist.eucl_15)[1:3,1:3], 1)
fviz_dist(dist.eucl_15)
View(c_cluster)

# Finding number of optimal clusters
class(df)
c_cluster <- as.matrix(c_cluster)
fviz_nbclust(c_cluster, kmeans, method='wss') + geom_vline(xintercept=4, linetype=5, col='orange')

# Looking at the elbow curve, it is wise to use 4 clusters
# K-means Clustering
set.seed(100)
km.res = kmeans(c_cluster, 4, nstart=40)
km.res

# Seeing means of variables in original data cluster-wise
aggregate(c_cluster, by = list(cluster=km.res$cluster), mean)
df_m = cbind(c_cluster, cluster = km.res$cluster)
head(df_m)
?palette
?fviz_cluster
fviz_cluster(km.res, data=c_cluster,
             palette=c('#2E9FDF','#E7B800','#FC4E07','#2edf51'),
             ellipse.type='convex',
             ellipse.alpha=0.4,
             pointsize=3,
             main="Calories Cluster",
             star.plot=T,
             geom="point",
             repel=T,
             ggtheme=theme_bw())

# Dissimilarity matrix
res.dist = dist(c_cluster, method='euclidean')
head(res.dist)
round(as.matrix(res.dist)[1:3, 1:3], 1)
fviz_dist(res.dist)
res.hc = hclust(d = res.dist, method = 'ward.D2')
fviz_dend(res.hc, cex = 0.5)
fviz_dend(res.hc, k=4,
          cex=0.5,
          k_colors=c('#2E9FDF','#00AFBB','#E7B800','#2edf51'),
          color_labels_by_k = T,
          rect = T)

# Cut tree
grp = cutree(res.hc, k=4)
head(grp,3)
table(grp)
rownames(df)[grp==1]
fviz_cluster(list(data=c_cluster, cluster=grp),
             palette=c('#00AFBB','#E7B800','#FC4E07','#2edf51'),
             ellipse=T,
             ellipse.type="convex",
             repel=T,
             geom="point",
             show.clust.cent=F,
             main="Cereals Cluster Plot",
             ggtheme=theme_bw())