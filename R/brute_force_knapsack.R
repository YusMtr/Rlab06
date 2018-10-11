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

brute_force_knapsack <- function(x, W, parallel = FALSE) {
  stopifnot(W > 0 &
              is.data.frame(x) &
              is.vector(x$v) &
              is.vector(x$w) &
              length(x$w) == length(x$v))
  n <- length(x[[1]])
  v <- x$v
  w <- x$w
  best <- replicate(n, 0)

  if (parallel) {
    # Checks each row of M if the weight is allowed
    res_vec <- parallel::mclapply(1:(2^n-1), function(x, w, v, W) {
      m <- intToBits(x)
      if (sum(w[m == 1]) <= W) {
        return(m)
      }
    }, w, v, W, mc.cores = parallel::detectCores())

    # Takes all the allowed rows and calculates which one has the best value
    lapply(Filter(Negate(is.null), res_vec), function(m) {
      if (sum(v[m == 1]) > sum(v[best == 1])) {
        best <<- m
      }
    })
  } else {
    # Goes through each row of M to find the optimal value
    lapply(1:(2^n-1), function(x) {
      m <- intToBits(x)
      if (sum(v[m == 1]) > sum(v[best == 1]) & sum(w[m == 1]) <= W) {
        best <<- m
      }
    })
  }

  res <- list(value = sum(v[best == 1]), elements = which(best == 1))
  return(res)
}
