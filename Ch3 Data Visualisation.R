setwd("C:/Users/Vanessa/Documents/R/R for Data Science")
library(tidyverse)
mpg
? mpg
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = cty, y = hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), color = "green")

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color= cty))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, shape= cty))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color= cty, shape= drv))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color= cty, shape= drv, size=year))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color= cty, stroke= 1), shape=22)

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color= displ < 5))
