# Task 1

say_hello <- function() {
  print("Hello, R programmer!")
}

# Task 2

square <- function(x) {
  return(x^2)
}

# Task 3

rectangle_area <- function(length, width) {
  return(length * width)
}

rectangle_area(4, 5)

# Task 4

power <- function(base, exponent = 2) {
  return(base^exponent)
}

power(3)
power(3, 4)

# Task 5

is_even <- function(n) {
  if (n %% 1 != 0) {
    warning("Input is not an integer")
  }
  return(n %% 2 == 0)
}

# Task 6

grade_score <- function(score) {
  if (score < 0 || score > 100) {
    return("Invalid score")
  } else if (score >= 90) {
    return("A")
  } else if (score >= 80) {
    return("B")
  } else if (score >= 70) {
    return("C")
  } else if (score >= 60) {
    return("D")
  } else {
    return("F")
  }
}

# Task 7

my_sum <- function(vec) {
  total <- 0
  
  for (i in vec) {
    total <- total + i
  }
  
  return(total)
}

# Task 8

factorial_loop <- function(n) {
  result <- 1
  
  for (i in 1:n) {
    result <- result * i
  }
  
  return(result)
}

# Task 9

mean_sd <- function(vec) {
  return(list(
    mean = mean(vec),
    sd = sd(vec)
  ))
}

# Task 10

min_max_range <- function(vec) {
  return(list(
    min = min(vec),
    max = max(vec),
    range = max(vec) - min(vec)
  ))
}

# Task 11

fahr_to_celsius <- function(f) {
  (f - 32) * 5/9
}

celsius_to_fahr <- function(c) {
  (c * 9/5) + 32
}

convert_temp <- function(temp, unit = "C") {
  if (unit == "C") {
    return(celsius_to_fahr(temp))
  } else if (unit == "F") {
    return(fahr_to_celsius(temp))
  } else {
    return("Invalid unit")
  }
}

# Task 12

standardize <- function(vec) {
  if (sd(vec) == 0) {
    return(rep(0, length(vec)))
  }
  
  return((vec - mean(vec)) / sd(vec))
}

# Task 13

is_prime <- function(n) {
  if (n <= 1) {
    return(FALSE)
  }
  
  for (i in 2:floor(sqrt(n))) {
    if (n %% i == 0) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

# Task 14

roll_dice <- function(n, sides = 6, target = sides) {
  
  rolls <- sample(1:sides, n, replace = TRUE)
  
  return(list(
    rolls = rolls,
    sum = sum(rolls),
    proportion = mean(rolls >= target)
  ))
}