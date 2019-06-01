gaps.gen <- function(data, potential.centers, k = 6, init = "forgy", iter = 6) 
{
  #Randomly select potential centers and set them to be centroids
  centroids <- gaps.init(data, potential.centers, init, k)
  #Store the centroid cluster of each data point in this vector
  cluster <- rep(0, nrow(data))
  #The current epoch
  cur_iter <- 1
  #While the current epoch is not equal to the maximum epoch
  while(cur_iter <= iter) 
  {
    #Increment the current epoch
    cur_iter <- cur_iter + 1
    #For each data point in the list of data points
    for (i in 1:nrow(data)) 
    {
      #Stores the smallest distance calculated so far
      min.dist <- 1000000000
      #For each centroid in the list of centroids
      for (j in 1:k) 
      {
        #Calculate the distance
        centroid.dist <- gaps.dist(data[i,], centroids[j,], weighted = TRUE)
        #If the distance is smaller than the minimum distance variable
        if (centroid.dist <= min.dist) 
        {
          #Set the cluster of the data point to the index of the centroid
          cluster[i] <- j
          #Set the minimum distance variable to the distance to the centroid
          min.dist <- centroid.dist
        }
      }
    }
    #For each centroid in the list of centroids
    for (i in 1:k) 
    {
      x.coords <- 0.0
      y.coords <- 0.0
      total.in.cluster <- 0
      #For each data point in the list of data points
      for (j in 1:nrow(data))
      {
        #If the data point is in the cluster
        if (cluster[j] == i) 
        {
          #Add the current coordinates number to the x.coords and y.coords variable
          x.coords <- x.coords + data[j,]$X * data[j,]$COUNT
          y.coords <- y.coords + data[j,]$Y * data[j,]$COUNT
          #Increment the total.in.cluster variable by 1 times the count (weight)
          total.in.cluster <- total.in.cluster + 1 * data[j,]$COUNT
        }
      }
      #If there is more than one data point in the cluster
      if (total.in.cluster > 0) 
      {
        #Calculate the mean of the x.coords and y.coords variable
        centroids[i,]$X <- x.coords / total.in.cluster
        centroids[i,]$Y <- y.coords / total.in.cluster
      }
    }
    #For each centroid in the list of centroids
    for (i in 1:k) 
    {
      x.coords <- 0.0
      y.coords <- 0.0
      min.dist <- 10000000
      #For each data point in the list of data points
      for (j in 1:nrow(potential.centers))
      {
        potential.center.dist <- gaps.dist(centroids[i,], potential.centers[j,])
        if (potential.center.dist < min.dist)
        {
          min.dist <- potential.center.dist
          x.coords <- potential.centers[j,]$X
          y.coords <- potential.centers[j,]$Y
          centroids[i,]$NAME <- potential.centers[j,]$NAME
        }
      }
      centroids[i,]$X <- x.coords
      centroids[i,]$Y <- y.coords
    }
  }
  #Return the centroids to the user
  return(centroids)
}

gaps.init <- function(data, potential.centers, init, k) 
{
  #If the user wants to use the Forgy Method to initialize
  if (init == "forgy") 
  {
    #Randomly select potential centers from 
    return(potential.centers[sample(nrow(potential.centers),k),])
  }
}

gaps.calc.dists <- function(data, centroids, weighted = FALSE) 
{
  distances <- rep(0, nrow(data))
  for (i in 1:nrow(data))
  {
    min.dist <- 1000000000
    for (j in 1:nrow(centroids))
    {
      potential.dist <- gaps.dist(data[i,], centroids[j,], weighted = weighted)
      if (potential.dist < min.dist)
      {
        min.dist <- potential.dist
      }
    }
    distances[i] <- min.dist
  }
  return(distances)
}

gaps.inert <- function(data, centroids, metric = "mi", weighted = FALSE) 
{
  distances <- rep(0,sum(data$COUNT))
  iter <- 0
  for (i in 1:nrow(data)) 
  {
    min.dist <- 1000000000
    for (j in 1:nrow(centroids)) 
    {
      potential.dist <- gaps.dist(data[i,], centroids[j,], weighted = weighted)
      if (potential.dist < min.dist)
      {
        min.dist <- potential.dist
      }
    }
    for (k in 1:data[i,]$COUNT) 
    {
      iter <- iter + 1 
      distances[iter] <- min.dist
    }
  }
  return(distances)
}

gaps.dist <- function(loc.one, loc.two, metric = "mi", weighted = FALSE) 
{
  lat1 <- loc.one$Y * pi / 180.0
  lat2 <- loc.two$Y * pi / 180.0
  long1 <- loc.one$X * pi / 180.0
  long2 <- loc.two$X * pi / 180.0
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1))
  if (is.nan(d))
  {
    return(0)
  }
  if (metric == "mi")
  {
    dist <- d * 3958.8
  }
  else 
  {
    dist <- d * 6371.0
  }
  if (weighted == TRUE) 
  {
    return(dist * loc.one$COUNT)
  }
  return(dist)
}

gaps <- function(data, potential.centers, k = 6, init = "forgy", generations = 20, epoch = 6) 
{
  cur_generation <- 0
  fitness.score <- Inf
  opt.centers <- NULL
  while(cur_generation < generations) 
  {
    cur_generation <- cur_generation + 1
    potential.opt.centers <- gaps.gen(data, potential.centers, k = k, init = init, iter = epoch)
    distances <- gaps.calc.dists(data, potential.opt.centers, weighted = TRUE)
    new.fitness.score <- sum(distances * distances)
    if (new.fitness.score < fitness.score) 
    {
      fitness.score <- new.fitness.score
      opt.centers <- potential.opt.centers
    }
  }
  return(opt.centers)
}