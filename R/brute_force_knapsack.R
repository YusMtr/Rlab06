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

brute_force_knapsack <- function(x,W){
    if(W < 0){stop('wrong weight limit!')}
    if(sum(abs(x[,1]) == x[,1]) != length(x[,1]) &
       sum(abs(x[,2]) == x[,2]) != length(x[,2])){stop('wrong input!')}
    n <- length(x[,1])
    sample_space <- intToBits(c(1:2^n))
    mat <- matrix(sample_space,ncol = 32,byrow = TRUE)
    mat <- mat[,1:n]
    #each column shows one selection option
    mat <- t(mat)
    #---Weights
    mat_weight <- matrix(0,byrow = FALSE, nrow = n,ncol = 2^n)
    for(i in 1:2^n){
      mat_weight[,i] <- as.numeric(mat[,i])*x[,1]
    }
    weight_each_selection <- colSums(mat_weight)
    #---Values
    mat_value <- matrix(0,byrow = FALSE, nrow = n ,ncol = 2^n)
    for(i in 1:2^n){
      mat_value[,i] <- as.numeric(mat[,i])*x[,2]
    }
    value_each_selection <- colSums(mat_value)

    option <- data.frame(weight = weight_each_selection, value = value_each_selection)
    option <- option[order(option[,2], decreasing = TRUE),]

    value <- 0
    weight <- 0
    for (i in 1:2^n){
      value <- option[,2][i]
      weight <-  option[,1][i]
      if(weight <= W){break();}
    }

    selected <- mat[,which(value_each_selection == value)]
    selected <- which(selected != 00)

    output <- list(value = round(value) , elements = selected)
    output
}







