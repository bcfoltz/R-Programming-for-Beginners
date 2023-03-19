# https://www.youtube.com/watch?v=2TZYeFcJQIk&ab_channel=UQLibrary

library(tidyverse)

climate_raw <- read.csv("https://raw.githubusercontent.com/uqlibrary/technology-training/master/R/tidyverse_next_steps/data_wb_climate.csv", na = "..")

# tidyr and purr

climate_long <- pivot_longer(climate_raw,
                             "X1990":"X2011",
                             names_to = "year",
                             values_to = "value")

climate_long  <- climate_long %>%
  mutate(year = substr(year, start = 2, stop = 5)) %>%
  mutate(year = as.integer(year))


# find the unique combinations of two variables, the codebook
codes <- unique(climate_long[,c("Series.code", "Series.name")])

# widen

climate_tidy <- climate_long %>%
  select(-Series.name, -SCALE, -Decimals) %>%
  pivot_wider(names_from = 'Series.code',
              values_from = value)

# remove useless country groups

groups <- c("Europe & Central Asia",
            "East Asia & Pacific",
            "Euro area",
            "High income",
            "Lower middle income",
            "Low income",
            "Low & middle income",
            "Middle income",
            "Middle East & North Africa",
            "Latin America & Caribbean",
            "South Asia",
            "Small island developing states",
            "Sub-Saharan Africa",
            "Upper middle income",
            "World")

climate_tidy <- climate_tidy %>% 
  filter(!`Country.name` %in% groups)

# visulize with ggplot

climate_tidy %>%
  ggplot(aes(x = year,
             y = EN.ATM.CO2E.KT,
             group = Country.name)) +
  geom_line()

# visualize global emissions

climate_tidy %>%
  group_by(year) %>%
  summarise(CO2 = sum(EN.ATM.CO2E.KT, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = CO2)) +
  geom_point()

# remove years without data

climate_tidy %>%
  group_by(year) %>%
  summarise(CO2 = sum(EN.ATM.CO2E.KT, na.rm = TRUE)) %>%
  filter(year < 2009) %>%
  ggplot(aes(x = year, y = CO2)) +
  geom_point()

## using purr for iterating with functional programming

mtcars
class(mtcars)         

# build a proper loop in R

output <- vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
  output[[i]]  <- median(mtcars[[i]])
}

# output of medians
output

# use the map family in purr this is much better than a loop
car_medians <- map_dbl(mtcars, median)

# different data type as output

starwars
map_lgl(starwars, is_character)

# default the default behavior of function applies

map_dbl(mtcars, mean, trim = 0.2)

# use more elaborate formulas in this case rounding along the way

map_dbl(mtcars, ~round(mean(.x)))

map_lgl(mtcars, ~max(.x) > 3 * min(.x))

# find out the number of unique values in each variable of the starwars data

#map_int(starwars, ~count(unique(.x)))
map_int(starwars, ~length(unique(.x)))

# splitting data

unique(mtcars$cyl)

#split cars on cyl and then apply the summary function to each group
mtcars %>%
  split(.$cyl) %>%
  map(summary)

mtcars %>%
  split(.$cyl) %>%
  map(~ggplot(., aes(mpg, wt)) + geom_point() + geom_smooth())

# predicates

str(iris)
iris %>%
  map_dbl(mean)

# discard the Species variable which is a factor
iris %>%
  discard(is.factor) %>%
  map_dbl(mean)

starwars %>%
  keep(is.character) %>%
  map_int(~length(unique(.x)))

# keep everything but only apply function on some variables

iris %>%
  map_if(is.numeric, round) %>%
  str()

# cumulative and yearly change in CO2 emissions for all countries

climate_cumul <- climate_tidy %>% 
  arrange(`Country.name`, year) %>% 
  group_by(`Country.name`) %>%
  mutate(cumul.CO2.KT = cumsum(EN.ATM.CO2E.KT),
         dif.CO2.KT = EN.ATM.CO2E.KT - lag(EN.ATM.CO2E.KT)) %>%
  map_at(vars(ends_with("KT")), ~ .x / 10^6) %>% 
  as_tibble() %>%  # from list to tibble
  rename_with(~ str_replace(.x, "KT", "PG"))

# visualize

p <- climate_cumul %>%
  ggplot() +
  aes(x = year,
      y = cumul.CO2.PG,
      colour = `Country.name`) +
  geom_line() +
  theme(legend.position = "none")
p
