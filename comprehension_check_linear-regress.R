library(tidyverse)
library(caret)

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
sigma
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

head(dat)
set.seed(1, sample.kind="Rounding")
result <-replicate(100,{
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index) 
  
  fit <- lm(y ~ x, data = train_set)

  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

mean(result)
sd(result)


build_data_set <- function(n_2){
  dat <- MASS::mvrnorm(n=n_2, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  #set.seed(1, sample.kind="Rounding")
  result_1 <-replicate(100,{
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index) 
    
    fit <- lm(y ~ x, data = train_set)
    
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  c(mean(result_1),sd(result_1))
}

n_3 <- c(100,500,1000, 5000, 10000)
set.seed(1, sample.kind="Rounding")
sapply(n_3,build_data_set)



## same thing but with higher correlation 
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
sigma
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1, sample.kind="Rounding")
result <-replicate(100,{
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index) 
  
  fit <- lm(y ~ x, data = train_set)
  
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

mean(result)
sd(result)

help(matrix)


## 2 variables 
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
#Q6----Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index) 

fit_x1 <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit_x1, test_set)
sqrt(mean((y_hat - test_set$y)^2))

fit_x2 <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit_x2, test_set)
sqrt(mean((y_hat - test_set$y)^2))

fit_x12 <- lm(y ~ x_1+x_2, data = train_set)
y_hat <- predict(fit_x12, test_set)
sqrt(mean((y_hat - test_set$y)^2))
