library(tidyverse)

starwars %>%
  filter(height > 150 & mass < 200) %>%
  mutate(height_in_meters = height/100) %>%
  select(height_in_meters, mass) %>%
  arrange(mass) %>%
  #View()
  plot()

msleep

names(msleep)

unique(msleep)

missing_msleep <- !complete.cases(msleep)
missing_msleep

msleep[missing_msleep, ]

starwars %>% 
  select(name, height, mass)

starwars %>% 
  select(1:3)

df <- starwars
df$sex <- as.factor(df$sex)
levels(df$sex)
df <- df %>% 
  mutate(sex = factor(sex,
                      levels = c("male", "female", "hermadroditic", none)))

starwars %>% 
  select(mass, sex) %>% 
  filter(mass < 55 & sex == "male")

starwars %>% 
  select(sex) %>% 
  mutate(sex = recode(sex,
                      "male" = "m",
                      "female" = "f"))

mean(starwars$height, na.rm = TRUE)

starwars %>% 
  mutate(height_m = height / 100) %>% 
  select(name, height, height_m) %>% 
  mutate(tallness = 
           if_else(height_m < 1,
                   "short",
                   "tall"))

plot(pressure)


ggplot(data = starwars,
       mapping = aes(x = gender)) + geom_bar()

starwars %>% 
  drop_na(height) %>% 
  ggplot(mapping = aes(x = height)) + geom_histogram()

starwars %>% 
  drop_na(height) %>% 
  ggplot(aes(x = height)) +
  geom_boxplot(fill = "steelblue") +
  theme_bw() +
  labs(title = "Boxplot of height", x = "Height of characters")

starwars %>% 
  drop_na(height) %>% 
  filter(sex %in% c("male", "female")) %>% 
  ggplot(aes(x = height,
             color = sex,
             fill = sex))+
  geom_density(alpha = 0.2) +
  theme_bw()

starwars %>% 
  filter(mass < 200) %>% 
  ggplot(aes(height, mass, color = sex)) +
  geom_point(size = 5, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Height and mass by sex")

starwars %>% 
  filter(mass < 200) %>% 
  ggplot(aes(height, mass, color = sex)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth() +
  facet_wrap(~sex) +
  theme_bw() +
  labs(title = "Height and mass by sex")

library(gapminder)

gapminder %>% 
  filter(continent %in% c("Africa", "Europe")) %>% 
  t.test(lifeExp ~ continent, data = .,
         alternative = "two.sided",
         paired = FALSE)

gapminder %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c("Americas", "Europe", "Asia")) %>% 
  aov(lifeExp ~ continent, data = .) %>% 
  summary()

gapminder %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c("Americas", "Europe", "Asia")) %>% 
  aov(lifeExp ~ continent, data = .) %>% 
  TukeyHSD() %>% 
  plot()

flowers <- iris %>% 
  mutate(size = cut(Sepal.Length,
                    breaks = 3,
                    labels = c("Small", "Medium", "Large"))) %>% 
  select(Species, size)

flowers %>% 
  select(size) %>% 
  table() %>% 
  chisq.test()

cars %>% 
  lm(dist ~ speed, data = .) %>% 
  summary()

