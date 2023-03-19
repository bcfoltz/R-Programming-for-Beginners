library(tidyverse)
data()

dim(starwars)
str(starwars)
glimpse(starwars)
View(starwars)
head(starwars)
tail(starwars)

attach(starwars)

names(starwars)
length(starwars)
class(hair_color)
length(hair_color)
unique(hair_color)
table(hair_color)
sort(table(hair_color), decreasing = TRUE)
View(sort(table(hair_color), decreasing = TRUE))
barplot(sort(table(hair_color), decreasing = TRUE))

starwars %>%
  select(hair_color) %>%
  count(hair_color) %>%
  arrange(desc(n)) %>%
  View()

View(starwars[is.na(hair_color), ])

class(height)
length(height)
summary(height)
sum_height <- summary(height)
hist(height)
boxplot(height)
