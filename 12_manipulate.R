library(tidyverse)

gapminder <- read.csv("gapminder5yr.csv")

summary(gapminder)

# 1. filer(): pick observations ----
#logical operators

australia <- filter(gapminder, country == "Australia")

life80 <- filter(gapminder, lifeExp > 81)

# 2. arrange(): Used to reorder rows ----

arrange(gapminder, gdpPercap)
head(arrange(gapminder, desc(gdpPercap)))

# 3. select(): pick variables from dataset ----

gapminder_small <- select(gapminder,
                          year,country,gdpPercap)

# combine operations

gap_small_97 <- filter(gapminder_small, year == "1997")

#same thing as:

gap_small_97 <- filter(select(gapminder, year, country, gdpPercap), 
                       year == 1997)

# same thing, but with pipe operator

gap_small_97 <- gapminder %>% 
  select(year, country, gdpPercap) %>% 
  filter(year == 1997)

gapminder %>% 
  summary()

# challenge: 2002 life expectancy observation for Eritrea

lifeExp_Eritrea <- gapminder %>% 
  select(country, lifeExp, year) %>% #load in all the variables you are going to need
  filter(country == "Eritrea", year == 2002) # multiple filter parameters in the same call

# 4. mutate(): to create new variables ----

gap_gdp_billions <- gapminder %>% 
  mutate(gdp = (gdpPercap * pop)/1000000000) %>%  #in billions
  arrange(desc(gdp))

# 5. summarise(): collapse to a single summary

gapminder %>% 
  summarise(meanLE = mean(lifeExp))
  
# 6. group_by():change the scope ---- 

gapminder %>% 
  group_by(continent)

# mean life expectancy for each continent in 2007

gapminder %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarise(meanLE = mean(lifeExp))

# challenge: find max life expentancy for each country

gapminder %>% 
  group_by(country) %>% 
  summarise(maxLE = max(lifeExp)) %>% 
  arrange(maxLE)

# using starwars data

starwars %>% 
  group_by(species) %>% 
  summarise(n = n(),
            mass = mean(mass, na.rm = T)) %>% 
  filter(n > 1)

# associating with ggplot2

library(ggplot2)

gapminder %>% 
  filter(continent == "Europe") %>% 
  group_by(year) %>% 
  summarise(sum = sum(pop)) %>% 
  ggplot(aes(x = year,
             y = sum)) +
  geom_line()


