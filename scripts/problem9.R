# A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
# 
# a^2 + b^2 = c^2
# For example, 32 + 42 = 9 + 16 = 25 = 52.
# 
# There exists exactly one Pythagorean triplet for which a + b + c = 1000.
# Find the product abc.
# https://summathfun.files.wordpress.com/2010/12/fibonacci-pyth-triple.pdf
# https://www.mathsisfun.com/numbers/pythagorean-triples.html

find_triple_euclid <- function(target) {
  for (m in 1:target) {
    for (n in (m+1):target) {
      trip <- euclid_method(n, m)
      if (sum(trip) == target) {
        return(trip)
        break
      }
    }
  }
  return(NULL) # If nothing was found
}
euclid_method <- function(n, m) {
  a <- n^2 - m^2
  b <- 2*n*m
  c <- n^2 + m^2
  return(c(a, b, c))
}

find_triple <- function(target) {
  for (a in 1:target) {
    for (b in (a+1):target) {
      c <- target - a - b
      if (is_triplet(a, b, c) & sum(c(a,b,c)) == target) {
        return(c(a,b,c))
      }
    }
  }
  
}

is_triplet <- function(a, b, c) {
  return(a^2 + b^2 == c^2)
}

cat('Semi-brute force:\n')
print(system.time(brute <- find_triple(1000)))
print(brute)

cat("Euclid' Method:\n")
print(system.time(euclid <- find_triple_euclid(1000)))
print(euclid)

cat('Answer:\n')
print(prod(euclid))

# 
# # third attempt
# 
# pythagoras_method <- function(m) {
#   return(c(2*m, m^2 - 1, m^2 + 1))
# }
# 
# 
# # Second attempt
# 
# # First attempt
# 
# triple_sum1 <- function(target=1000) {
#   # calculate pythagorean triplets
#   # iterate until sum >= target (hopefully target)
#   odd_squares <- seq(3,target*2, by=2)
#   squares_test <- sqrt(odd_squares) - floor(sqrt(odd_squares)) == 0 
#   odd_squares <- odd_squares[squares_test]
#   
#   for (i in odd_squares) {
#     triplet <- py_triple(i)
#     print(triplet)
#     if (sum(triplet) >= target) { # we'll stop at the closest to the target
#       cat('Target:',target,'| Triplet:', paste(triplet, collapse=', '), '| Sum:', sum(triplet),'\n')
#       return(triplet)
#     }
#   }
#   
# }
# fibonacci <- function(odd_square) {
#   # a^2 == odd_square
#   a = sqrt(odd_square)
#   b = (odd_square + 1)/2 -1
#   c = sqrt(a^2 + b^2)
#   return(c(a,b,c))
# }
  