library(tidyverse)

msleep

msleep %>%
  drop_na(vore) %>%
  group_by(vore) %>%
  summarise(Lower = min(sleep_total),
            Average = mean(sleep_total),
            Upper = max(sleep_total),
            Range = max(sleep_total) - min(sleep_total)) %>%
  arrange(Average) %>%
  View()

library(MASS)
attach(Cars93)

glimpse(Cars93)

table(Origin)
table(AirBags, Origin)
addmargins(table(AirBags, Origin),1)
addmargins(table(AirBags, Origin),2)
addmargins(table(AirBags, Origin))

table(AirBags, Origin)
prop.table(table(AirBags, Origin),2)*100
round(prop.table(table(AirBags, Origin),2)*100)

Cars93 %>%
  group_by(Origin, AirBags) %>%
  summarise(number = n()) %>%
  pivot_wider(names_from = Origin,
              values_from = number)
