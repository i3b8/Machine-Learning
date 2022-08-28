library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type


# what proportion of online and inclass students 
# are females 
dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))


## accuracy calculation based on the this algorithm 
## based on most prevalent sex for each type: we predict the sex for each type 
## than compute accuracy 
y_hat <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(y))
mean(y_hat==y)


## confusion matrix 
table(y_hat, y)

## sensitivity 
sensitivity(y_hat, y)
## specificy 
specificity(y_hat,y)

## Prevalance in the dat dataset(% of females )
mean(y == "Female")
