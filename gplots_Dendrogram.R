# Function that returns result from hierarchical clustering using choice of method
makeCluster <- function(data, method = "pearson") {
  if(method %in% c("pearson", "spearman")) {
    myCor <- cor(as.matrix(t(data)), method = method)
    myDist <- as.dist(1 - myCor)
  } else {
    myDist <- dist(data, method = method)
  }
  myCluster <- hclust(myDist, method = "complete")
  return(myCluster)
}

# Function that returns a dendrogram from clustering with choice of method
makeDendrogram <- function(data, method = "pearson") {
  myCluster <- makeCluster(data, method = method)
  myDend <- as.dendrogram(myCluster)
  myDend <- reorder(myDend, rowMeans(data, na.rm = T))
  return(myDend)
}