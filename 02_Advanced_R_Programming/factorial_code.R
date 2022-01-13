##### LOOP #####

factorial_loop = function(n) {
  #' Factorial loop
  #' Computes the factorial of an integer using looping (such as a for loop)
  #' 
  #' @param n integer
  #' 
  #' @return factorial of given integer

  
  # helper variables
  factorial = 1
  
  # if n was not a number / integer in the first place function returns error
  if (!is.numeric(n) | n != as.integer(n)) {
    stop("n must be an integer!")
    # if n is lower than 0 function returns error
    } else if(n<0){
      stop('n must be greater than or equal to 0')
      # factorial of 0 is defined as 1
      } else if(n == 0) {
        return(factorial)
      } else {
          # loop that calculates factorial for n > 0
          for(i in n:2) {
            factorial = factorial * i
          }
        return(factorial)
        }
}
  
##### REDUCE #####

factorial_reduce = function(n){
  #' Factorial_reduce:
  #' a version that computes the factorial using the reduce() function in the purrr package.
  #' Alternatively, you can use the Reduce() function in the base package.
  #' 
  #' @param n integer
  #' 
  #' @return factorial of given integer
  
  require(tidyverse)
  
  
  # helper variables
  factorial = 1
  
  # if n was not a number / integer in the first place function returns error
  if (!is.numeric(n) | n != as.integer(n)) {
    stop("n must be an integer!")
    # if n is lower than 0 function returns error
    } else if(n<0){
      stop('n must be greater than or equal to 0')
      # factorial of 0 is defined as 1
      } else if(n == 0) {
        return(factorial)
        } else {
        # calculating factorial for n > 0 using reduce()
          factorial = 1:n %>% reduce(`*`)
          return(factorial)
          }
}

##### FUNC #####

factorial_func = function(n){
  #' Factorial_func:
  #' a version that uses recursion to compute the factorial
  #' 
  #' @param n integer
  #' 
  #' @return factorial of given integer
  
  
  # helper variables
  factorial = 1
  
  # if n was not a number / integer in the first place function returns error
  if (!is.numeric(n) | n != as.integer(n)) {
    stop("n must be an integer!")
    # if n is lower than 0 function returns error
  } else if(n<0){
    stop('n must be greater than or equal to 0')
    # factorial of 0 is defined as 1
  } else if(n == 0) {
    return(factorial)
  } else {
    # calculating factorial for n > 0 using recursion
    factorial = factorial_func(n-1) * n
    return(factorial)
  }
}

##### MEM #####

factorial_table = c(1, rep(NA, 10))

factorial_mem = function(n){
  #' Factorial_func:
  #' a version that uses memoization to compute the factorial
  #' 
  #' @param n integer
  #' 
  #' @return factorial of given integer
  
  require(tidyverse)
  
  
  # helper variables
  factorial = 1
  
  # if n was not a number / integer in the first place function returns error
  if (!is.numeric(n) | n != as.integer(n)) {
    stop("n must be an integer!")
    # if n is lower than 0 function returns error
  } else if(n<0){
    stop('n must be greater than or equal to 0')
    # factorial of 0 is defined as 1
  } else if(n == 0) {
    return(factorial)
  } else {
    # calculating factorial for n > 0 using memoization
    if(is.na(factorial_table[n])){
      factorial_table[n] = 1:n %>% reduce(`*`)
    }
    return(factorial_table[n])
  }
}
