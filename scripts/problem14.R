# The following iterative sequence is defined for the set of positive integers:
#   
#   n  n/2 (n is even)
# n  3n + 1 (n is odd)
# 
# Using the rule above and starting with 13, we generate the following sequence:
#   
#   13  40  20  10  5  16  8  4  2  1
# It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
# 
# Which starting number, under one million, produces the longest chain?
# 
# NOTE: Once the chain starts the terms are allowed to go above one million.


max_collatz <- function(n, odds_only=F, primes_only=F) {
  # find the number that produces the maximum
  # collatz sequence length from n:1
  max_n <- 1
  max <- 1
  if (primes_only == T) {
    vals <- sieve(n)
  }
  else if (odds_only == T) {
    vals <- rev(seq(1,n, by=2))
  }
  else {
    vals <- n:1
  }
  
  # run the test
  for (n in vals) {
    test <- collatz_sequence(n)
    if (test$value > max) {
      max <- test$value
      max_n <- n
    }
  }
  return(max_n)
}

collatz_sequence<- function(n) {
  chain <- n
  while (n > 1) {
    if (n%%2 == 0) {n <- n/2} else {n <- 3*n + 1}
    chain <- c(chain, n)
  }
  return(list('value'=length(chain), 'chain' = chain))
} 

collatz_sequence_length <- function(n) {
  chain <- 1
  while (n > 1) {
    if (n%%2 == 0) {n <- n/2} else {n <- 3*n + 1}
    chain <- chain + 1
  }
  return(list('value'=length(chain), 'chain' = chain))
} 

sieve <- function(n) {
  # a range of numbers from 1:n 
  # determine which are prime
  if (n > 3) {
    prime <- c(F, rep(T, n-1))
    for (i in 2:n) {
      if (prime[i] == T & i^2 <= n) {
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


