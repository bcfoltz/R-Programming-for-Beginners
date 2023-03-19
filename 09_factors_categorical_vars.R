library(tidyverse)
library(forcats)
library(patchwork)

?forcats
data()

View(gss_cat)
glimpse(gss_cat)

unique(gss_cat$race)

gss_cat %>% 
  pull(race) %>% 
  unique()

count(gss_cat, race, sort = T)

gss_cat %>% 
  count(race)

gss_cat %>% 
  pull(race) %>% 
  levels()

gss_cat %>% 
  select(race) %>% 
  table()

gss_cat %>% 
  mutate(race = fct_drop(race)) %>% 
  pull(race) %>% 
  levels()

gss_cat %>% 
  drop_na(tvhours) %>% 
  group_by(relig) %>% 
  summarise(mean_tv = mean(tvhours)) %>% 
  ggplot(aes(mean_tv, relig)) + geom_point(size = 4)

gss_cat %>% 
  drop_na(tvhours) %>% 
  group_by(relig) %>% 
  summarise(mean_tv = mean(tvhours)) %>% 
  mutate(relig = fct_reorder(relig, mean_tv)) %>% 
  ggplot(aes(mean_tv, relig)) + geom_point(size = 4)

gss_cat %>% 
  drop_na(age) %>% 
  filter(rincome != "Not applicable") %>% 
  group_by(rincome) %>% 
  summarise(mean_age = mean(age)) %>% 
  ggplot(aes(mean_age, rincome)) + geom_point(size = 4)

gss_cat %>% 
  drop_na(age) %>% 
  filter(rincome != "Not applicable") %>% 
  group_by(rincome) %>% 
  summarise(mean_age = mean(age)) %>% 
  mutate(rincome = fct_rev(rincome)) %>% 
  ggplot(aes(mean_age, rincome)) + geom_point(size = 4)

gss_cat %>% 
  count(marital)

gss_cat %>% 
  mutate(marital = fct_infreq(marital)) %>% 
  ggplot(aes(marital)) +
  geom_bar(fill = "steelblue", alpha = 0.5) +
  theme_bw()

unique(gss_cat$partyid)

gss_cat %>% 
  mutate(relig = fct_lump(relig, n = 2)) %>% 
  count(relig)

gss_cat %>% 
  filter(!is.na(age)) %>% 
  filter(marital %in% c("Never married",
                        "Married",
                        "Widowed")) %>% 
  count(age, marital) %>% 
  group_by(age) %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot(aes(age, prop, color = marital)) +
  geom_line(size = 2, na.rm = T) +
  theme_minimal()

gss_cat %>% 
  filter(!is.na(age)) %>% 
  filter(marital %in% c("Never married",
                        "Married",
                        "Widowed")) %>% 
  count(age, marital) %>% 
  group_by(age) %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(marital = fct_reorder2(marital, age, prop)) %>% 
  mutate(marital = fct_rev(marital)) %>% 
  ggplot(aes(age, prop, color = marital)) +
  geom_line(size = 2, na.rm = T) +
  theme_minimal()

