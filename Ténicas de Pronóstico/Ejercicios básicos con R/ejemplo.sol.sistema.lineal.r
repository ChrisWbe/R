options(digits=3)
set.seed(3)
m = matrix(runif(16), nrow = 4)
m
b = runif(4)
b
solve(m,b)
# recupera b
m%*%solve(m,b) # Should recover b