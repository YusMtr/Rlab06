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

knapsack_dynamic <- function(x,W){
  if(W < 0){stop('wrong weight limit!')}
  if(sum(abs(x[,1]) == x[,1]) != length(x[,1]) &
     sum(abs(x[,2]) == x[,2]) != length(x[,2])){stop('wrong input!')}
n <- length(x[,1]) + 1
m <- matrix(0,nrow = n,ncol = W)
w <- c(0,x[,1])
v <- c(0,x[,2])
#---- Creating a list to save the selected elements
vec <- numeric()
list <- rep(list(vec),W)
elements <- rep(list(list), n)
#----
for(i in 2:n){
  for(j in 1:W){
    if(w[i] > j){m[i,j] <- m[i-1,j]
                 elements[[i]][[j]] <- elements[[i-1]][[j]]
    }else{
        m[i,j] <- max(m[(i-1),j],(m[(i-1),(j-w[i])] + v[i]))
        if(max(m[(i-1),j],(m[(i-1),(j-w[i])] + v[i])) == m[(i-1),j]){
          elements[[i]][[j]] <- elements[[i-1]][[j]]
        }else{elements[[i]][[j]] <- append(elements[[i-1]][[j-w[i]]],i)}
      }
  }
}
value <- round(m[n,W])
selected_elements <- elements[[n]][[W]] -1
output <- list(value = value, elements = selected_elements)
output
}



