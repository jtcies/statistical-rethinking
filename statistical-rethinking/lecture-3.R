library(tidyverse)
library(rethinking)
theme_set(theme_minimal())

data(Howell1)

d <- Howell1
d2 <- d[d$age >= 18 , ]

set.seed(2971)

N <- 100
a <- rnorm(N, 178, 20)
b <- rnorm(N, 0, 10)

x <- d2$weight
xbar <- mean(d2$weight)

lin_mod <- function(a, b) { 
  
  mu <- a + b * (x - xbar)
  
  tibble(a, b, x, mu)
    
}

map2_dfr(a, b, lin_mod) %>% 
  ggplot(aes(x, mu, group = paste(a,b))) +
    geom_line(alpha = 0.2)

set.seed(2971)

a2 <- rnorm(N, 178, 20)
b2 <- rlnorm(N, 0 ,10)

map2_dfr(a2, b2, lin_mod) %>% 
  ggplot(aes(x, mu, group = paste(a,b))) +
  geom_line(alpha = 0.2)
