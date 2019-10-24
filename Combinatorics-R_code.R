# Combinatorics functions
# V.1.5


# Factorial function ------------------------------------------------------

f.fact <- function(x) {
  if (x[1] == 0) {
    # "[1]" was added in case x is a vector: otherwise there will warning messages about the length of x compared to a single value (ie 0)
    return(1)
  } else {
    #print(x)
    return(x * f.fact(x - 1))
  }
}

# Test factorial function
fact.numbers <- c(4:5)
factorial(fact.numbers) == f.fact(fact.numbers)
# #NB# for values above 49 the two functions return the same (?) number,
# but they are not considered equivalent. But for > 100 yes, because they
# both return "Inf".

# #NB# Factorial works with vectors, while my function does not.
# How do I pass a vector to my function?
# #SOLVED# r always passes argument as vectors! Try this:
f.fact(c(2:4))

# #IDEA# "Deploy" this functions in a Shiny app


# Permutations ------------------------------------------------------------
# Order matters (eg 1,2,3 is different from 3,2,1)
# Two special cases: 1. when k=n, with rep=FALSE; and when sum(k)=n, with rep=TRUE

f.permutation <- function(n, k, repetition) {
  # Check for user's error
  #if (n == 0) return(print("Number of elements equals zero"))
  #if (k == 0) return(print("Classes equals zero"))
  #if (n < k) return(print("Classes are higher than elements"))
  # Look for the right formula
  if (repetition == FALSE) {
    # check if permutation apply
    if (n == k) return (f.fact(n))           # Special case
    # if not use, general formula 
    return(f.fact(n)/f.fact(n - k))
  } else {
    # if sum of k equals n use this formula
    if (sum(k) == n) {                       # Special case
      return(f.fact(n) / prod(f.fact(k)))
    } 
    # otherwise
    return (n^k)
  }
}

# Test permutation functions
no.elements <- 4
no.classes <- 2 #c(1,2,1)
f.permutation(no.elements, no.classes, repetition = TRUE)


# Combinations ------------------------------------------------------------
# The order of selection does not matter (eg 1,2,3 == 3,2,1)

f.combinations <- function(n, k, repetition) {
  # Look for the right formula
  if (repetition == FALSE) {
    return(f.fact(n)/(f.fact(k)/(f.fact(n-k))))
  } else {
    return((f.fact(n+k-1)/(f.fact(k)*f.fact(n-1))))
  }
}

# Test combinations function
no.elements <- 4
no.classes <- 1
f.combinations(no.elements,no.classes, repetition = TRUE)
