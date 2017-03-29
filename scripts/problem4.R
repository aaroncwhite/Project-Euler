# A palindromic number reads the same both ways. The largest palindrome made from 
# the product of two 2-digit numbers is 9009 = 91 × 99.
# 
# Find the largest palindrome made from the product of two 3-digit numbers.
library(plyr)
palindrome <- function(x) {
  forwards <- as.character(x)
  backwards <- paste(rev(unlist(strsplit(as.character(x), ''))), collapse='')
  return(backwards == forwards)
}

max_palindrome <- function(bottom, top) {
  vals <- bottom:top
  
  mat <- matrix(nrow=length(vals), ncol=length(vals))
  rownames(mat) <- vals
  colnames(mat) <- vals
  for (i in 1:length(vals)) {
    mat[i,] <- vals[i] * vals
  }
  
  pdromes <- apply(mat, 1, function(x) sapply(x, function(j) palindrome(j)))
  
  mat <- mat[pdromes]
  
  return(max(mat))
}
print(max_palindrome(100, 999))
