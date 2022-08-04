#############Metric Spaces and Distances############
####################################################
#A metric space is a set of points equipped with a distance function. 
########Euclidean Distance########
#This is also known as l2-distance.
#Here is a simple implementation in R:
distance.l2 <- function(x,y){
     sqrt(sum((abs(x - y))^2))#abs(x) computes the absolute value of x, sqrt(x) computes the (principal) square root of x, √{x}.
}                             #Usage abs(x) sqrt(x)
#Let us use this function:
x <- c(1,0,0)
y <- c(0,1,0)
distance.l2(x,y)
?abs(x)
#########Manhattan Distance##########
#This is also known as city-block distance or l1-distance.
#Here is a simple implementation in R:
distance.l1 <- function(x,y){
     sum(abs(x - y))
}
#Using this function:
distance.l1(x,y)
############Max Distance##############
#Here is a simple implementation in R:
distance.linf <- function(x,y){
     max(abs(x-y))
}
#Using this function:
distance.linf(x,y)
############Minkowski Distance#########
#This distance is a generalization of the l1, l2, and max distances.
#Here is a simple implementation in R:
distance.lp <- function(x,y,p){
     sum((abs(x - y)^p))^(1/p)
}
#For p=1, it reduces to city-block distance. For p=2, it reduces to Euclidean distance. In the limit of p→∞, it reduces to max distance.
#Using this function:
distance.lp(x,y,p = 2)
distance.lp(x,y,p = .5)
distance.lp(x,y,p = 3)
############Canberra Distance###########
#Canberra Distance is a weighted version of the Manhattan distance
#Here is a simple implementation in R:
distance.canberra <- function(x,y){
     a1 <- abs(x - y)
     a2 <- abs(x) + abs(y)
     a3 <- a1/a2
     sum (a1/a2,na.rm = TRUE)
}
#Using this function:
distance.canberra(x,y)
###############Binary or Jaccard Distance#############
#Jaccard distance (a.k.a. binary distance) measures the dissimilarity between sample sets.
#Here is a simple implementation in R:
distance.binary <- function(x,y){
     x <- x != 0
     y <- y != 0
     a <- sum(x & y)
     b <- sum(x | y)
     1 - (a / b)
}
#Using this function:
a <- c(1,0,3,0)
b <- c(0,2,4,0)
distance.binary(a,b)
###############dist Function#############
#R provides a function named dist which can compute all the distances described above. 
#This function works on a data frame or a matrix. Every row is treated as a separate point 
#in space. If the data frame has n rows, then the function computes n(n−1)/2 distances. 
#It returns an object of which dist can be used to find out distances between each pair of 
#points. The dist object can be converted into a n×n symmetric matrix containing the distances. 
#By default, it computes Euclidean distances.
#We will compute distances between unit-vectors in 3-dimensional space:
eye <- diag(3)
eye
#Computing the distances:
d <- dist(eye)
#The distances in the form of a symmetric matrix:
as.matrix(d)
#Computing Manhattan distances:
d1 <- dist(eye,method = "manhattan")
as.matrix(d1)
#Computing maximum distances:
dinf <- dist(eye,method = "maximum")
as.matrix(dinf)
#Minkowski distances:
as.matrix(dist(eye,"minkowski",p = 0.5))
as.matrix(dist(eye,"minkowski",p = 3))
#Canberra distances:
as.matrix(dist(eye,"canberra"))
#It is also straightforward to compute distance between two points as follows:
a <- c(1,0,3,0)
b <- c(0,2,4,0)
dist(rbind(a,b))
dist(rbind(a,b),method = "manhattan")
dist(rbind(a,b),method = "maximum")
#Computing the binary distance:
dist(rbind(a,b),method = "binary")
##############Understanding the dist object#############
#The dist function returns an object of class dict.
#Let us create 4 points for this exercise:
points <- diag(c(1,2,3,4))
points
#Let us compute the city block distances between these points:
distances <- dist(points , method = "manhattan")
#Check the class of the returned value:
class(distances)
#Let us print the distances:
distances
as.matrix(distances)
#If you note carefully, you can see that the distances object contains the lower triangle of the
#distance matrix [below the diagonal]. For 4 points, it stores 6 distances (1 + 2 + 3 = 4 * 3 / 2 = 6).
#The number of points for which distances were calculated can be accessed from the dist object as follows:
attr(distances,"Size")
#The dist object is a one dimensional vector. Assuming that there are n-points, then the 
#distance between i-th point and j-th point where (1 <= i < j <= n) is stored at p-th 
#index in the dist object where p is given by the following formula:p=n(i−1)−i(i−1)/2+j−i
#Let us get n first:
n <- attr(distances,"Size")
#Let us say we want to find the distance between 2nd and 4th point:
i <- 2; j <- 4;
distances [n * (i - 1) - i * (i - 1)/2 + j - i]
#Let's match the same with the distance matrix presented above. I guess it is much easier to 
#first convert the dist object into a distance matrix and then work with it.
#Manhattan Distance Between Two Vectors
#create function to calculate Manhattan distance
manhattan_dist <- function(a, b){
     dist <- abs(a-b)
     dist <- sum(dist)
     return(dist)
}

#define two vectors
a <- c(2, 4, 4, 6)

b <- c(5, 5, 7, 8)

#calculate Manhattan distance between vectors
manhattan_dist(a, b)
#Manhattan Distance Between Vectors in a Matrix
#create four vectors
a <- c(2, 4, 4, 6)

b <- c(5, 5, 7, 8)

c <- c(9, 9, 9, 8)

d <- c(1, 2, 3, 3)

#bind vectors into one matrix
mat <- rbind(a, b, c, d)

#calculate Manhattan distance between each vector in the matrix
dist(mat, method = "manhattan")
#####################Hamming distance##########################
#create vectors
x <- c(0, 0, 1, 1, 1)
y <- c(0, 1, 1, 1, 0)

#find Hamming distance between vectors
sum(x != y)
#create vectors
x <- c(7, 12, 14, 19, 22)
y <- c(7, 12, 16, 26, 27)

#find Hamming distance between vectors
sum(x != y)
#create vectors
x <- c('a', 'b', 'c', 'd')
y <- c('a', 'b', 'c', 'r')

#find Hamming distance between vectors
sum(x != y)
?lower.tail = TRUE
##################Mahalanobis Distance in R#################
#create data
df = data.frame(score = c(91, 93, 72, 87, 86, 73, 68, 87, 78, 99, 95, 76, 84, 96, 76, 80, 83, 84, 73, 74),
                hours = c(16, 6, 3, 1, 2, 3, 2, 5, 2, 5, 2, 3, 4, 3, 3, 3, 4, 3, 4, 4),
                prep = c(3, 4, 0, 3, 4, 0, 1, 2, 1, 2, 3, 3, 3, 2, 2, 2, 3, 3, 2, 2),
                grade = c(70, 88, 80, 83, 88, 84, 78, 94, 90, 93, 89, 82, 95, 94, 81, 93, 93, 90, 89, 89))
#view first six rows of data
head(df)
#calculate Mahalanobis distance for each observation
mahalanobis(df, colMeans(df), cov(df))
#create new column in data frame to hold Mahalanobis distances
df$mahal <- mahalanobis(df, colMeans(df), cov(df))

#create new column in data frame to hold p-value for each Mahalanobis distance
df$p <- pchisq(df$mahal, df=3, lower.tail=FALSE)

#view data frame
df
#Typically a p-value that is less than .001 is considered to be an outlier. We can see that the
#first observation is an outlier in the dataset because it has a p-value less than .001.
#Depending on the context of the problem, you may decide to remove this observation from the 
#dataset since it’s an outlier and could affect the results of the analysis.
##############################################################
##################HIERARCHICAL-CLUSTERING#####################
##############################################################
library(datasets)
data(mtcars)
View(mtcars)
head(mtcars)
cars.dist <- dist(mtcars)
h_cars <- hclust(cars.dist)
plot(h_cars)
cars.dist <- dist(mtcars,method = "canberra")
h_cars <- hclust(cars.dist,method = "median")
plot(h_cars)
h_cars$ac
help("hclust")
#################H-CLUST ON IRIS DATASET#######################
df <- iris
df <- na.omit(df)
DATA <- scale(DATA)
D <- dist(DATA,method = "euclidean")
#Hierarchical clustering using complete linkage
hcl <- hclust(D,method = "complete")
#plot the obtained dendogram
plot(hcl)
#compute with agnes
#install.packages("cluster")
library(cluster)
HCL <- agnes(DATA,method = "complete")
A1 = c(2,3,5,7,8,10,20,21,23)
A2 = A1
A3 = A1
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(A1,A2,A3,angle = 25,type = "h")
demo <- hclust(dist(cbind(A1,A2,A3)))
plot(demo)
#################################################
##############K-MEAN CLUSTERING##################
#################################################
newiris <- iris
newiris$Species <- NULL #because clustering can be perform on numerical data only
#square root of (no. of observation)/2
k <- sqrt(nrow(newiris))/2
k
kc <- kmeans(newiris,2)#from stats package
?kmeans
kc
#Compare the species label with the clustering result
table(iris$Species,kc$cluster)
kc$iter
kc$size
table(iris$Species)#Let's compare separately
#plotting iris
plot(newiris[c("Sepal.Length","Sepal.Width")],col = kc$cluster)
#ploting centers
points(kc$centers[,c("Sepal.Length","Sepal.Width")],col = 1:3,pch =8, cex =2 )
######################Another_Package#########################
#install.packages("factoextra")
library(factoextra)#clustering algorithms and visualization
library(cluster)#clustering algorithms
k2 <- kmeans(newiris,2)
fviz_cluster(k2,data = newiris)
k3 <- kmeans(newiris,centers = 3,nstart = 25)
k4 <- kmeans(newiris,centers = 4,nstart = 25)
k5 <- kmeans(newiris,centers = 5,nstart = 25)
#plots to compare
p1 <- fviz_cluster(k2,geom = "point",data = newiris) + ggtitle("k = 2")
p2 <- fviz_cluster(k3,geom = "point",data = newiris) + ggtitle("k = 3")
p3 <- fviz_cluster(k4,geom = "point",data = newiris) + ggtitle("k = 4")
p4 <- fviz_cluster(k5,geom = "point",data = newiris) + ggtitle("k = 5")
library(gridExtra)
grid.arrange(p1,p2,p3,p4,nrow = 2)
######################Elbow_Method#########################
wss <- (nrow(newiris)-1)*sum(apply(newiris,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(newiris,centers = i)$withinss)
plot(1:15,wss,type = "b",xlab = "Number of clusters",
     ylab = "within groups sum of squares")
#END











######
normalize(x, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")
data(iris)
data(iris)
dv <- diana(iris, metric = "manhattan", stand = TRUE)
print(dv)
plot(dv)

## Cut into 2 groups:
dv2 <- cutree(as.hclust(dv), k = 2)
table(dv2) # 8 and 42 group members
rownames(votes.repub)[dv2 == 1]

## For two groups, does the metric matter ?
dv0 <- diana(votes.repub, stand = TRUE) # default: Euclidean
dv.2 <- cutree(as.hclust(dv0), k = 2)
table(dv2 == dv.2)## identical group assignments

str(as.dendrogram(dv0)) # {via as.dendrogram.twins() method}

data(agriculture)
## Plot similar to Figure 8 in ref
# }
# NOT RUN {
plot(diana(agriculture), ask = TRUE)
# }

