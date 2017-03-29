# The prime factors of 13195 are 5, 7, 13 and 29.
# 
# What is the largest prime factor of the number 600851475143?

primeFactors <- function(x) {
  # find the range of numbers that could be 
  # factors of x
  rng <- 2:floor(sqrt(x))
  # immediately ignore any even values
  rng <- rng[rng %% 2 != 0]
  
  # narrow down to just factors of x
  factors <- x %% rng == 0
  factors <- rng[factors]
  
  # for our remaining factors, test if they're prime
  primes <- sapply(factors, function(a) (all(a %% 2:ceiling(sqrt(a)) != 0)))
  
  # filter factors down to only primes
  primes <- factors[primes]
  
  # return the max of the values
  return(max(primes))
}

primeFactors(600851475143)

