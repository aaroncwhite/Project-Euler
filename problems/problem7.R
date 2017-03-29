# By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
# 
# What is the 10 001st prime number?

# Using Sieve of Erotosthenes (per wikipedia):
# 1 - Create a list of consecutive integers from 2 through n: (2, 3, 4, ..., n).
# 2 - Initially, let p equal 2, the smallest prime number.
# 3 - Enumerate the multiples of p by counting to n from 2p in increments of p, and 
#     mark them in the list (these will be 2p, 3p, 4p, ... ; the p itself should not be marked).
# 4 - Find the first number greater than p in the list that is not marked. If there was no such 
#     number, stop. Otherwise, let p now equal this new number (which is the next prime), and repeat from step 3.
nth_prime <- function(x) {
  primes <- n_primes(x)
  return(primes[x])
  
}

n_primes <- function(x) {
  n <- x
  
  primes <- c()
  while (length(primes) < x) {
    primes <- sieve(n)
    n <- n + x
  }
  
  return(primes[1:x])
  
}

sieve <- function(n) {
  # a range of numbers from 1:n 
  # determine which are prime
  if (n > 3) {
    prime <- c(F, rep(T, n-1))
    for (i in 2:floor(sqrt(n))) {
      if (prime[i] == T) {
        prime[seq(i^2, n, by=i)] <- F
      }
    }
    return(c(1:n)[prime])
  } 
  # some verification for the small numbers. 
  else if (n == 1) {
    return(c())
  }
  else {
    ifelse(n==3, return(c(2,3)), return(2))
  }

}

print(nth_prime(10001))

# Third Attempt. Still too slow. 
# nth_prime2 <- function(x) {
#   # x is the number of primes to generate
#   # how to generate large vector?
#   
#   ints <- 1:ceiling(x * x / 2) # from tests, this seems to be reasonable. 
#   primes <- rep(T, length(ints))
#   # n <- ceiling(sqrt(length(ints)))
#   prime_num <- NULL
#   i <- 2
#   while (length(prime_num) < x) {
#     if (primes[i] == T) {
#       primes[seq(i,length(ints), by=i)] <- F #
#       prime_num <- c(prime_num, i)
#       cat('\rPrimes found:', length(prime_num), '\tPotential Primes Remaining:',length(primes[primes == T]),rep(' ', 30))
#     }
#     i <- i + 1
#     
#   }
#   
#   
#   cat('\nMax Prime:', max(prime_num))
#   # return(max(prime_num))
# }



# first/second attempt
# nth_prime1 <- function(x) {
#   # x is the number of primes to generate
#   # how to generate large vector?
#   
#   ints <- 2:ceiling(x * x / 2) # from tests, this seems to be reasonable. 
#   ints <- ints[ints %% 2 != 0] # halve the number of values to test right away. 
#   primes <- c(2)
#   while (length(ints) > 0 & length(primes) < x) {
#     primes <- c(primes, ints[1]) # add the prime to our vector
#     ints <- ints[ints %% ints[1] != 0] # remove anything that evenly divides by the first number (prime)
#     
#     cat('\rPrimes found:', length(primes), '\tPotential Primes Remaining:',length(ints),rep(' ', 30))
#     
#     flush.console()
#   }
#   cat('\n')
#   return(max(primes))
# }
# 
# 
# 
# 
# 
# 
# 
