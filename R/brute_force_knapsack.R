#'brute_force_knapsack
#'
#'@description The brute force search (the algorithm that has been used here in
#' this funciton) by going through all possible alternatives and return the
#' maximum value found is the only solution that is guaranteed to give a correct
#' answer in all situations for the knapsack problem. The knapsack problem is a
#' discrete optimization problem where we have a knapsack that can take a limited
#' weight W and we want to fill this knapsack with a number of items i = 1, ...,
#' n, each with a weight wi and a value vi. The goal is to find the knapsack with
#' the largest value of the elements added to the knapsack.This problem is
#' NP-hard. You can see more details about this problem here:
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#'
#'
#'
#'@param x data.frame  A data.frame that contains the weight and value of each
#'  element.
#'@param W integer  An integer that shows the limit for the weight that can be
#'  carried by the knapsack.
#' @examples brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#'# $value
#'# 16770
#'# $elements
#'# 5 8
#'
#'@export

brute_force_knapsack <- function(x, W, parallel) {
  if(!is.data.frame(x) || W < 0){
    stop("Errounous Input.")
  }

  if(parallel==FALSE)
  {
    n <- length(x[[1]])
    v <- x$v
    w <- x$w

    best <- rep(0, n)
    values <- c()

    lapply(1:(2^n-1), function(x) {
      m <- intToBits(x)

      values <- sum(v[m==1])
      bestval <- sum(v[best == 1])
      weig <- sum(w[m == 1])

      if (values > bestval &  weig <= W) {
        best <<- m
       }
      })
    }
   else
    {
    n <- length(x[[1]])
    v <- x$v
    w <- x$w

    best <- rep(0, n)
    values <- c()

    # Check the number of cores on the computer
    cores <- parallel::detectCores()

    cl <- parallel::makeCluster(cores, type = "PSOCK")

    # Parallel calculation (parLapply):
    resA <- parallel::parLapply(cl, 1:(2^n-1), fun= function(x)
    {
      m <- intToBits(x)

      weig <- sum(w[m == 1])
      if (weig <= W) {
        return(m)
      }
    })

    lapply(resA, function(m)
    {
      values <- sum(v[m==1])
      bestval <- sum(v[best == 1])

      if(values > bestval)
      {
        best <<- m
      }
    })

    # Shut down cluster
    parallel::stopCluster(cl)

   }
  res <- list(value = sum(v[best == 1]), elements = which(best == 1))

  return(res)
}

#set.seed(42)
#n <- 16
#knapsack_objects <-
#  data.frame(
#    w=sample(1:4000, size = n, replace = TRUE),
#    v=runif(n = n, 0, 10000)
#  )
#brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500,parallel = FALSE)
#brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500,parallel = TRUE)
