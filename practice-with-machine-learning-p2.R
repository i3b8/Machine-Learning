library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

#head(iris)

 set.seed(2) # if using R 3.5 or earlier
#create an even split of the data into train and test
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
##
test <- iris[test_index,]
train <- iris[-test_index,]
train
min_se_length <-min(train$Sepal.Length)
min_se_length
## 

cutoff  <- seq(min(train$Sepal.Length),max(train$Sepal.Length),0.1)
hihgest_accura <-map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat==train$Species)
  
})

data.frame(cutoff,hihgest_accura) %>% 
  ggplot(aes(cutoff, hihgest_accura)) + 
  geom_point() + 
  geom_line()

max(hihgest_accura)


cutoff  <- seq(min(train$Petal.Length),max(train$Petal.Length),0.1)
hihgest_accura <-map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat==train$Species)
  
})

data.frame(cutoff,hihgest_accura) %>% 
  ggplot(aes(cutoff, hihgest_accura)) + 
  geom_point() + 
  geom_line()

## 4.7

cutoff  <- seq(min(train$Petal.Width),max(train$Petal.Width),0.1)
hihgest_accura <-map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat==train$Species)
  
})

data.frame(cutoff,hihgest_accura) %>% 
  ggplot(aes(cutoff, hihgest_accura)) + 
  geom_point() + 
  geom_line()

## 1.7 
y_hat <- ifelse(train$Petal.Width > 1.7 | train$Petal.Length >4.7, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
mean(y_hat==train$Species)



## val = 4.7


cutoff  <- seq(min(train$Petal.Width),max(train$Petal.Width),0.1)
hihgest_accura <-map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat==y)
  
})

data.frame(cutoff,hihgest_accura) %>% 
  ggplot(aes(cutoff, hihgest_accura)) + 
  geom_point() + 
  geom_line()

max(hihgest_accura)
## val = 1.5



cutoff  <- seq(min(train$Sepal.Width),max(train$Sepal.Width),0.1)
hihgest_accura <-map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat==y)
  
})

data.frame(cutoff,hihgest_accura) %>% 
  ggplot(aes(cutoff, hihgest_accura)) + 
  geom_point() + 
  geom_line()

max(hihgest_accura)
## val = 1.5




foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
head(predictions)
cutoff <- (which.max(predictions$Petal.Length)-1)*0.1+range(train$Petal.Length)[1]
cutoff
y_hat <- ifelse(test$Petal.Length > cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
mean(y_hat==test$Species)

sapply(predictions,max)	
