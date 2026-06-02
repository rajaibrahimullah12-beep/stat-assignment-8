#Task 15
repeat_operation <- function(f, x, n) {
  
  result <- x
  
  for (i in 1:n) {
    result <- f(result)
  }
  
  return(result)
}

square <- function(x) {
  x^2
}

repeat_operation(square, 2, 3)