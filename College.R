library(tidyverse)
library(ISLR2)
library(ggplot2)

College <- read.csv("~/Downloads/College.csv")

?College
glimpse(College)

############### Exploratory graphics ###############
ggplot(College, aes(x = Grad.Rate)) +
  geom_histogram()

suspicious <- filter(College, Grad.Rate >= 100)
View(suspicious)

ggplot(College, aes(x = log10(F.Undergrad),
                    y = Grad.Rate)) +
  geom_point()
####################################################

################ Filtering columns #################
college_sm <- College %>%
  mutate(log_full = log10(F.Undergrad)) %>%
  select(Grad.Rate,
         log_full,
         Private,
         Top25perc)
View(college_sm)
####################################################

################# Basic Modelling ##################
ggplot(College, aes(x = log10(F.Undergrad),
                    y = Grad.Rate)) +
  geom_point() +
  geom_smooth(method = "lm")

model_undergrad <- lm(Grad.Rate ~ log_full,
                      data = college_sm)
summary(model_undergrad)
plot(model_undergrad)
#####################################################

##### Private schools effect on graduation rate #####
ggplot(College, aes(x = log10(F.Undergrad),
                    y = Grad.Rate,
                    color= Private)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  scale_color_brewer(palette = "Dark2")

model_private <- lm(Grad.Rate ~ Private + log_full,
                    data = college_sm)
summary(model_private)
###################################################### 

######### Interaction: Private & F.Undergrad #########
model_private_int <- lm(Grad.Rate ~ Private * log_full,
                    data = college_sm)
summary(model_private_int)

anova(model_private_int)
######################################################

#######################################################
# Conclusion: When private schools are considered, 
schools with larger full time enrollment, 
have higher graduation rate
######################################################## 


