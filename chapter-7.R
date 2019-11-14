library(tidyverse)
library(rethinking)

theme_set(theme_minimal())

data(Howell1)
d <- Howell1 %>% 
  tbl_df()

d <- d %>% 
  mutate_at(vars(age, height), scale)

set.seed( 1000 ) 
i <- sample(1:nrow(d),size=nrow(d)/2) 
d1 <- d[ i , ] 
d2 <- d[ -i , ]

m1 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * age,
    a ~ dnorm(0, 0.2),
    b1 ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d1
)

m2 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * age + b2 * age ^ 2,
    a ~ dnorm(0, 0.2),
    b1 ~ dnorm(0, 0.5),
    b2 ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d1
)

m3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * age + b2 * age ^ 2 + b3 * age ^ 3,
    a ~ dnorm(0, 0.2),
    b1 ~ dnorm(0, 0.5),
    b2 ~ dnorm(0, 0.5),
    b3 ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d1
)


m4 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * age + b2 * age ^ 2 + b3 * age ^ 3 + b4 * age ^ 4,
    a ~ dnorm(0, 0.2),
    b1 ~ dnorm(0, 0.5),
    b2 ~ dnorm(0, 0.5),
    b3 ~ dnorm(0, 0.5),
    b4 ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d1
)

m5 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * age + b2 * age ^ 2 + b3 * age ^ 3 + b4 * age ^ 4 + b5 * age ^ 5,
    a ~ dnorm(0, 0.2),
    b1 ~ dnorm(0, 0.5),
    b2 ~ dnorm(0, 0.5),
    b3 ~ dnorm(0, 0.5),
    b4 ~ dnorm(0, 0.5),
    b5 ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d1
)

m6 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * age + b2 * age ^ 2 + b3 * age ^ 3 + b4 * age ^ 4 + b5 * age ^ 5 + b6 * age ^ 6,
    a ~ dnorm(0, 0.2),
    b1 ~ dnorm(0, 0.5),
    b2 ~ dnorm(0, 0.5),
    b3 ~ dnorm(0, 0.5),
    b4 ~ dnorm(0, 0.5),
    b5 ~ dnorm(0, 0.5),
    b6 ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d1
)

compare(m1, m2, m3, m4, m5, m6)

extract_precis <- function(model) {

  extract.samples(model) %>% 
    gather(param, val) %>% 
    group_by(param) %>% 
    summarise(
      mean = mean(val),
      high = qnorm(0.985, mean(val), sd(val)),
      low = qnorm(0.015, mean(val), sd(val)),
    )
}

all_precis <- list(m1, m2, m3, m4, m5, m6) %>% 
  purrr::map_dfr(extract_precis, .id = "model")

m1_precis <- all_precis %>%
  filter(model == 1) 

d1 %>% 
  mutate(
    mid_pred = m1_precis$mean[1] + m1_precis$mean[2] * age,
    low_pred = m1_precis$low[1] + m1_precis$low[2] * age,
    high_pred = m1_precis$mean[1] + m1_precis$high[2] * age,
    )

ggplot(m1_pred, aes(age, height)) +
    geom_point() +
    geom_line(aes(y = mid_pred)) +
    geom_ribbon(aes(ymin = low_pred, ymax = high_pred), alpha = 0.2)

m2_precis <- all_precis %>%
  filter(model == 2) 

m2_pred <- d1 %>% 
  mutate(
    mid_pred = m2_precis$mean[1] + m2_precis$mean[2] * age + m2_precis$mean[3] * age ^ 2,
    low_pred = m2_precis$low[1] + m2_precis$low[2] * age + m2_precis$mean[3] * age ^ 2,
    high_pred = m2_precis$mean[1] + m2_precis$high[2] * age + m2_precis$mean[3] * age ^ 2,
  )

ggplot(m2_pred, aes(age, height)) +
  geom_point() +
  geom_line(aes(y = mid_pred)) +
  geom_ribbon(aes(ymin = low_pred, ymax = high_pred), alpha = 0.2)

m3_precis <- all_precis %>%
  filter(model == 3) 

m3_pred <- d1 %>% 
  mutate(
    mid_pred = m3_precis$mean[1] + m3_precis$mean[2] * age + m3_precis$mean[3] * age ^ 2 + m3_precis$mean[4] * age ^ 3,
    low_pred = m3_precis$low[1] + m3_precis$low[2] * age + m3_precis$mean[3] * age ^ 2 + m3_precis$mean[4] * age ^ 3,
    high_pred = m3_precis$mean[1] + m3_precis$high[2] * age + m3_precis$mean[3] * age ^ 2 + m3_precis$mean[4] * age ^ 3,
  )

ggplot(m3_pred, aes(age, height)) +
  geom_point() +
  geom_line(aes(y = mid_pred)) +
  geom_ribbon(aes(ymin = low_pred, ymax = high_pred), alpha = 0.2)

m4_precis <- all_precis %>%
  filter(model == 4) 

m4_pred <- d1 %>% 
  mutate(
    mid_pred = m4_precis$mean[1] + m4_precis$mean[2] * age + m4_precis$mean[3] * age ^ 2 + m4_precis$mean[4] * age ^ 3 + m4_precis$mean[5] * age ^ 4,
    low_pred = m4_precis$low[1] + m4_precis$low[2] * age + m4_precis$mean[3] * age ^ 2 + m4_precis$mean[4] * age ^ 3 + m4_precis$mean[5] * age ^ 4,
    high_pred = m4_precis$mean[1] + m4_precis$high[2] * age + m4_precis$mean[3] * age ^ 2 + m4_precis$mean[4] * age ^ 3 + m4_precis$mean[5] * age ^ 4,
  )

ggplot(m4_pred, aes(age, height)) +
  geom_point() +
  geom_line(aes(y = mid_pred)) +
  geom_ribbon(aes(ymin = low_pred, ymax = high_pred), alpha = 0.2)
