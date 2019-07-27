library(tidyverse)
library(rethinking)

theme_set(theme_minimal())

data(homeworkch3)

# hard problems

births <- c(birth1, birth2)

pct_boy <- sum(births) / length(births)

p_grid <- seq(0, 1, length.out = 1000)

prior <- rep(0.5, 1000)

likelihood <- dbinom(sum(births), length(births), prob = p_grid)

post <- likelihood * prior

post_std <- post / sum(post)

qplot(p_grid, post_std)

samples <- sample(p_grid, prob = post_std, size = 1e5, replace = TRUE)

qplot(samples)

mean_samples <- mean(samples)
sd_samples <- sd(samples)

c(.5, .89, .97) %>% 
  purrr::map(~ qnorm(.x, mean = mean_samples, sd = sd_samples))
