library(tidyverse)

starwars

sw <- starwars %>%
  select(name, height, mass, gender) %>%
  rename(weight = mass) %>% 
  na.omit() %>%
  mutate(height = height / 100) %>%
  filter(gender %in% c("masculine", "feminine")) %>%
  mutate(gender = recode(gender,
                         masculine = "M",
                         feminine = "F")) %>%
  mutate(size = height > 1 & weight > 75,
         size = if_else(size == TRUE, "BIG", "SMALL"))
  