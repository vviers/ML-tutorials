# Functions as functions

# functions can return functions
power <- function(expon) {
  
  fun <- function(x) {
    return(x^expon)
  }
  
  return(fun)
  
}

square <- power(2) # instanciate the function power()
square(3)

cube <- power(3) # instanciate the function power()
cube(3)

# funny things are possible like a list containing functions
list_powers <- lapply(1:3, power)
list_powers
list_powers[[2]](10)


# Example with Logarithms
logarithm <- function(b) {
  
  fun <- function(x) {
    return(log(x, base = b))
  }
  
  return(fun)
  
}

log42 <- logarithm(42)
log42(10)


good_plotter <- function(pch) {
  fun <- function(x, y) {
    plot(x, y, pch = pch)
  }
  return(fun)
}

plot_squares <- good_plotter(pch = 15)

plot_squares(c(1, 10, 50, 75, 100), log42(c(1, 10, 50, 75, 100)))