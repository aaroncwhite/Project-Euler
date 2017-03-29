# 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
# 
# What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

# After some research, recursively calculating the least common multiple seems to be best
# http://stackoverflow.com/questions/17980184/find-the-smallest-equally-divisible-in-a-range-of-numbers-in-python-puzzle

lcm <- function(nums) {
  # first chose the biggest value in nums vector
  # and set the biggest two numbers first (hopefully this reduces calc time)
  nums <- nums[order(nums, decreasing = T)]
  g <- max(nums)
  
  while (length(nums) > 1) {
    # take the first two elements and find
    # lcm
    x <- nums[1]
    y <- nums[2]
    gcd <- hcf(x, y)
    cat(paste0("(",x, '*', y,')/', gcd))
    lcm <- x * y/ gcd
    cat('=', lcm,'\n')

    nums <- c(lcm,nums[-(1:2)]) # remove the two vals we just used and replace with lcm
    # iterate
  }

  cat('\n')
  return(nums)
}

hcf <- function(x, y) {
  # find highest common factor (gcd)
  s <- min(c(x, y))
  for (i in s:1) { # decrement
    if ((x %% i==0) && (y %% i == 0)) {
      break # stop as soon as we find one that matches both
    }
  }
  return(i)
  
  
}

print(lcm(1:20))

# this way took WAY too long since it is incrementing by 1 each time.  
# while (1==1) {
#   cat('\r',x,'&', y,'lcm=',g,rep(' ', 30))
#   if ((g %% x == 0) & (g %% y == 0)) { # no remainder for either into larger number
#     lcm = g
#     break
#   }
#   flush.console()
#   g = g + 1 # we failed, so increment the value up
# }

# another old way that was too long
# tests <- 1:20
# inc <- max(tests)
# val <- inc
# i <- inc
# pass <- 1
# while (i > 0) {
# 
#   i <- max(tests)
#   while (i > 0) {
#     cat('\rVal=',val, '/', i, '\tPass:',pass,rep(' ', 40))
#     if (val %% i == 0) {
#       i <- i - 1
#       flush.console()
#     }
#     else {
#       flush.console()
#       break 
#     }
#   }
#   # how to increment value efficiently?
#   val <- val + inc
#   pass <- pass + 1
# }




