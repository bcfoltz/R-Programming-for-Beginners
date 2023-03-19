library(tidyverse)
library(gapminder)

view(gapminder)

gapminder %>% 
  separate(col = year,
           into = c("century", "year"),
           sep = 2) %>%
  unite(col = location,
        country, continent,
        sep = " ",
        remove = F) %>% 
  View()

