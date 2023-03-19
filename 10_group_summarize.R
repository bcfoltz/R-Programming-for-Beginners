library(tidyverse)

msleep %>% 
  drop_na(sleep_rem, vore) %>% 
  group_by(vore) %>% 
  summarise('Average total sleep' = mean(sleep_total),
            'Maximum rem sleep' = max(sleep_rem)) %>% 
  View()

