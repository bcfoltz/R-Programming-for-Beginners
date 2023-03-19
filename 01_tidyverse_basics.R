library(tidyverse)
data()

view(starwars)

starwars %>%
  select(gender, mass, height, species) %>%
  na.omit()
  mutate(height = height / 100) %>%
  mutate(BMI = mass / height^2)

