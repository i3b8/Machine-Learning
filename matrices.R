library(tidyverse)
library(dslabs)

x <- matrix(rnorm(100*10), 100, 10)
x
dim(x)
nrow(x)
ncol(x)


x_new1 <- sweep(x, 1, 1:nrow(x),"+")
x_new <-  x + seq(nrow(x))


identical(x_new,x_new1)


## row 
rowMeans(x)
colMeans(x)
