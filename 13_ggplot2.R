library(tidyverse)

ggplot(data = economics,
       mapping = aes(x = date,
                     y = unemploy)) +
  geom_point()

ggplot(data = mpg,
       mapping = aes(x = displ,
                     y = hwy)) +
  geom_point(aes(color = class, size = 3, alpha = 0.3)) +
  geom_smooth(method = "lm", se = F)

ggplot(data = economics,
       mapping = aes(x = date,
                     y = unemploy)) +
  geom_point(mapping = aes(color = uempmed))

# bar sharts

ggplot(diamonds,
       aes(x = cut,
           fill = clarity)) +
  geom_bar()

ggplot(diamonds,
       aes(x = cut)) +
  geom_bar(fill = "tomato") +
  labs(x = "Quality of cut",
       y = "Number of Diamonds") +
  coord_flip() +
  theme_classic()
