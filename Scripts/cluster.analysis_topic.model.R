# load libraries
packages <- c("fpc", "cluster", "topicmodels", "data.table", "ggplot2")
lapply(packages, require, character.only = T)

# load data
load("tdms.RData")
load("dtms.RData")
load("combi.RData")

# K-Means clustering
matrix <- as.matrix(tdms)
matrixTranspose <- t(matrix)
k = 3

# algorithm to randomly assign data points to k clusters
set.seed(123)
kMeansResult <- kmeans(matrixTranspose, k)

# Centroids calculated
str(kMeansResult)
round(kMeansResult$centers, 3)

# plot of clusters
plot(matrixTranspose, col = kMeansResult$cluster)
points(kMeansResult$centers, col = 1:3, pch = 8, cex = 2)

pdf("K Means Cluster.pdf")
clusplot(matrixTranspose, kMeansResult$cluster, color = T, shade = T, labels = 0, lines =0)
dev.off()

# topic models
lda <- LDA(dtm, k = 8) # find 8 topics
term <- terms(lda, 4) # first four terms of every topic
term

term <- apply(term, MARGIN = 2, paste, collapse = ", ")

topic <- topics(lda, 1)
topics <- data.frame(cuisine = combi$cuisine, topic)
qplot(cuisine, ..count.., data = topics, geom = "density",
      fill = term[topic], position = "stack")


