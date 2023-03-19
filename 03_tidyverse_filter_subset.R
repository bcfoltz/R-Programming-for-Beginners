library(tidyverse)
view(msleep)

mydata <- msleep %>%
  select(name, sleep_total, order, bodywt) %>%
  #filter(order == "Primates" | bodywt > 20) %>%
  # filter(between(sleep_total, 16, 18))
  filter(near(sleep_total, 17, tol = 0.5))