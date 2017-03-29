# The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
# 
# Find the sum of all the primes below two million.

# going to borrow Euclid's seive from earlier, but it may need
# some additional improvements. 

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

print(system.time(s<-sum(as.numeric(sieve(2000000))))) # So many integers, it flips out
print(s)
