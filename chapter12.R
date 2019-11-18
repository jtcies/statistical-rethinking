library(tidyverse)
library(rethinking)
## 12.2 zero-inflated Poisson

prob_drink <- 0.2
rate_work <- 1
N <- 365

set.seed(2019)

drink <- rbinom(N, 1, prob_drink)

y <- (1 - drink) * rpois(N, rate_work)

tibble(y, drink) %>% 
  ggplot(aes(y, fill = fct_rev(factor(drink)))) +
    geom_histogram(binwidth = 1)

m12_4 <- ulam(
  alist(
    y ~ dzipois(p, lambda),
    logit(p) <- ap,
    log(lambda) <- al,
    ap ~ dnorm(-1.5, 1),
    al ~ dnorm(1, 0.5)
  ),
  data = list(y = as.integer(y))
)

precis(m12_4)

## 12.3 - order categorical outcomes

data(Trolley)

t <- Trolley

t %>% 
  ggplot(aes(response)) +
    geom_histogram(binwidth = 1)

t_summary <- t %>%
  group_by(response) %>% 
  summarise(n = n()) %>% 
  mutate(
    pct = n / sum(n),
    cum_pct = cumsum(pct)
  )

t_summary %>% 
  ggplot(aes(response, cum_pct)) +
    geom_line()

logit <- function(x) { log(x / (1-x)) }

t_summary %>% 
  mutate(lco = logit(cum_pct)) %>% 
  ggplot(aes(response, lco)) +
    geom_line()

m12_5 <- ulam(
  alist(
    R ~ dordlogit(0, cutpoints),
    cutpoints ~ dnorm(0, 1.5)
  ),
  data = list(R = t$response)
)


precis(m12_5, depth = 2)

tibble(x = 1:(length(coef(m12_5)) + 1), y = c(inv_logit(coef(m12_5)), 1)) %>% 
  mutate(z = y - lag(y, default = 0)) %>% 
  ggplot(aes(x, y)) +
    geom_line() +
    geom_col(aes(y = z))


t2_dat <- list(
  R = t$response,
  A = t$action,
  I = t$intention,
  C = t$contact
)

m12_6 <- ulam(
  alist(
    R ~ dordlogit(phi, cutpoints),
    phi <- bA * A + bC * C + BI * I,
    BI <- bI + bIA * A + bIC * C,
    c(bA, bI, bC, bIA, bIC) ~ dnorm(0, 0.5),
    cutpoints ~ dnorm(0, 1.5)
  ),
  data = t2_dat
)

precis(m12_6)

extract.samples(m12_6) %>%
  as_tibble() %>% 
  select(starts_with("b")) %>% 
  gather(param, est) %>% 
  group_by(param) %>% 
  summarise(
    mean = mean(est),
    pi_low = PI(est)[1],
    pi_high = PI(est)[2]
  ) %>% 
  ggplot(aes(param, mean)) +
    geom_point() +
    geom_linerange(aes(ymin = pi_low, ymax = pi_high)) +
    coord_flip()

sim_m12_6 <- crossing(
  A = 0:1,
  C = 0:1,
  I = 0:1
) %>% 
  mutate(combn = paste0("V", row_number()))

sim(m12_6, data = sim_m12_6) %>% 
  as_tibble() %>% 
  gather(combn, pred) %>% 
  left_join(sim_m12_6) %>% 
  ggplot(aes(pred, fill = factor(I))) +
    geom_histogram(binwidth = 1, position = "dodge") +
    facet_grid(A ~ C)

# 12.4 ordered categorical predictors -----------------

data(Trolley)

t <- Trolley

edu_levels <- c(6, 1, 8, 4, 7, 2, 5, 3)
t$edu_leveled <- edu_levels[t$edu]

dat_m12_7 <- list(
  R = t$response,
  A = t$action,
  I = t$intention,
  C = t$contact,
  E = as.integer(t$edu_leveled),
  alpha = rep(2, 7)
)

m12_7 <- ulam(
  alist(
    R ~ dordlogit(phi, kappa),
    phi <- bE * sum(delta_j[1:E]) + bA * A + bI * I + bC * C,
    kappa ~ dnorm(0, 1.5),
    c(bA, bI, bC, bE) ~ dnorm(0, 1),
    vector[8]: delta_j <<- append_row(0, delta),
    simplex[7]: delta ~ dirichlet(alpha)
  ),
  data = dat_m12_7
)
