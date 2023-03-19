library(tidyverse)

starwars$gender <- as.factor(starwars$gender)
class(starwars$gender)
levels(starwars$gender)

starwars$gender <- factor((starwars$gender),
                          levels = c("masculine", "feminine"))
levels(starwars$gender)

starwars %>%
  select(name, height, ends_with("color")) %>%
  names()
 unique(hair_color)

starwars %>%
  select(name, height, ends_with("color")) %>%
  filter(hair_color %in% c("blond", "brown") &
           height < 180)

mean(starwars$height, na.rm = TRUE)

starwars %>%
  select(name, gender, hair_color, height) %>%
  filter(complete.cases(.))

starwars %>%
  select(name, gender, hair_color, height) %>%
  filter(!complete.cases(.))

starwars %>%
  select(name, gender, hair_color, height) %>%
  filter(!complete.cases(.)) %>%
  drop_na(height)

starwars %>%
  select(name, gender, hair_color, height) %>%
  filter(!complete.cases(.)) %>%
  mutate(hair_color = replace_na(hair_color, "none"))

duplicated(starwars$hair_color)

starwars %>% 
  select(name, gender) %>%
  mutate(gender = recode(gender,
                         "masculine" = 1,
                          "feminine" = 2))
