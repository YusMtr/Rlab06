#'greedy_knapsack
#'
#'@description This function use a heuristic or approximation for the knapsack
#'  problem. This algorithm will not give an exact result (but it can be shown
#'  that it will return at least 50% of the true maximum value). A short
#'  description on how to implement the greedy approach can be found here:
#'  \url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm}
#'
#'@param x data.frame  A data.frame that contains the weight and value of each
#'  element.
#'
#'@param W integer  An integer that shows the limit for the weight that can be
#'  carried by the knapsack.
#'
#' @examples greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#'# $value
#'# 192647
#'# $elements
#'# 92 574 472 80 110 537 332 117 37 776 577 288 234 255 500 794 55
#'# 290 436 346 282 764 599 303 345 300 243 43 747 35 77 229 719 564
#'
#'@export

greedy_knapsack <- function(x, W) {
  if(W < 0){stop('wrong weight limit!')}
  if(sum(abs(x[,1]) == x[,1]) != length(x[,1]) &
     sum(abs(x[,2]) == x[,2]) != length(x[,2])){stop('wrong input!')}
  stopifnot(is.data.frame(x) &
              is.vector(x$v) &
              is.vector(x$w) &
              length(x$w) == length(x$v))
  v <- x$v
  w <- x$w
  n <-  length(v)
  bestValue <- replicate(n, 0)

  itre <- order(v / w, decreasing = TRUE)

  for(i in itre) {
    temp <- (W - w[i] - sum(w[bestValue > 0])) / w[i]
    if (temp > 0) {
      bestValue[i] <- temp
    } else {
      break
    }
  }

  result <- list(value = sum(v[bestValue > 0]), elements = which(bestValue > 0))
  return(result)
}

