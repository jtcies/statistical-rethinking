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

