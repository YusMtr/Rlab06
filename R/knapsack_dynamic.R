#' knapsack_dynamic
#'
#' @description This function like two other functions
#'   \code{\link{brute_force_knapsack}} and \code{\link{greedy_knapsack}} is one
#'   of the aproaches for solving knapsack problem
#'   (\url{https://en.wikipedia.org/wiki/Knapsack_problem}). The pseudocode for
#'   this algorithm can be found
#'   here:\url{https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem}
#'
#'@param x data.frame  A data.frame that contains the weight and value of each
#'  element.
#'@param W integer  An integer that shows the limit for the weight that can be
#'  carried by the knapsack.
#' @examples knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#'# $value
#'# 16770
#'# $elements
#'# 5 8
#'
#'@export

knapsack_dynamic <- function(x, W) {
  stopifnot(W > 0 &
              is.data.frame(x) &
              is.vector(x$v) &
              is.vector(x$w) &
              length(x$w) == length(x$v))
  i <- which(x$w > W)
  if (length(i != 0)) {
    x <- x[-i, ]
  }
  
  v <- x$v
  w <- x$w
  n <-  length(v)
  m <- matrix(replicate(W * n, 0), nrow = n, ncol = W)
  for (j in 1:W) {
    m[1, j] <- 0
  }
  for (i in 2:n) {
    for (j in 1:W) {
      if (w[i] > j) {
        m[i, j] <- m[i - 1, j]
      }
      else {
        m[i, j] <- max(m[i - 1, j], m[i - 1, j - w[i]] + v[i])
      }
    }
  }
  temp <- c()
  wt <- W
  i <- n
  while (i > 1) {
    if (m[i, wt] > m[i - 1, wt]) {
      temp <- c(temp, i)
      wt <- wt - w[i]
      i <- i - 1
    } else {
      i <- i - 1
    }
  }
  return(list(value = m[n, W], elements = rev(temp)))
}

#knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
