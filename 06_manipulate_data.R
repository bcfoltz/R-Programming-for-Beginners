library(tidyverse)

glimpse(msleep)
View(msleep)

msleep  %>%
  rename("conserve" = "conservation") %>%
  glimpse()

msleep %>%
  select(vore, name, everything())


msleep$vore <- as.factor(msleep$vore)

msleep %>%
  mutate(vore = as.character(vore)) %>%
  glimpse()

names(msleep)

msleep %>%
  select(2:4,
         awake,
         starts_with("sleep"),
         contains("wt"))

unique(msleep$order)

msleep %>%
  filter((order == "Carnivora" |
            order == "Primates") &
           sleep_total > 8) %>%
  select(name, order, sleep_total) %>%
  View()

msleep %>%
  filter(order %in% c("Carnivora", "Primates") & sleep_total > 8) %>%
  select(name, order, sleep_total) %>%
  arrange(order) %>%
  View()

msleep %>%
  mutate(brainwt_in_grams = brainwt * 1000) %>%
  View()

msleep$brainwt
msleep$brainwt > 0.01

size_of_brain <- msleep %>%
  select(name, brainwt) %>%
  drop_na(brainwt) %>%
  mutate(brain_size = if_else(brainwt > 0.01,
                              "large",
                              "small"))

size_of_brain %>%
  mutate(brain_size = recode(brain_size,
                             "large" = 1,
                             "small" = 2))

library(gapminder)
view(gapminder)
names(gapminder)

gap_data <- select(gapminder, country, year, lifeExp)

View(gap_data)

wide_data <- gap_data %>%
  pivot_wider(names_from = year, values_from = lifeExp)

long_data <- wide_data %>%
  pivot_longer(2:13,
               names_to = "year",
               values_to = "lifeExp")
