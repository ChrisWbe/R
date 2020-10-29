# --------ejemplo simple de objeto list
a = matrix(c(2,3,4,5),2,2)
b = c("complex","real")
d = runif(120) # 120 números aleatorios entre 0 y 1
L  = list(a=a,b=b,d=d)
L$b
[1] "complex"  "real"
 L$b[1]
 "complex"

(L$a)
     [,1] [,2]
[1,]    2    4
[2,]    3    5

y = L$d
hist(y,30)