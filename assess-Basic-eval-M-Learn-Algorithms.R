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

#head(dat)
#y
#x

# prop-inclass-female 
p_incl_fem <- mean(x=="inclass" & y=="Female")
p_incl_fem

# prop_online -female 
prop <-dat %>% filter(type =="online")
online_fem <-mean(prop$sex=="Female")
##inclass 
prop <-dat %>% filter(type =="inclass")
inclass_fem <-mean(prop$sex=="Female")

online_fem
inclass_fem


y_hat<- ifelse(x=="inclass","Female","Male")%>%
  factor(levels = c("Female","Male"))
mean(y_hat==y)

## Confusion matrix 
table(y_hat, y)

## sensitivity 
sensitivity(data = y_hat, reference = y)
## specificy 
specificity(data = y_hat, reference = y)

#prevalence of female in dat 
dat %>% summarize(mean(sex=="Female"))
