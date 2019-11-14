library(tidyverse)
library(rethinking)

# homework week 3

data(foxes)

foxes_std <- foxes %>% 
  mutate_at(vars(2:5), scale) %>% 
  tbl_df()

# q1: prior predictive sim

set.seed(1985)

N <- 100

a <- rnorm(N, 0, 0.5)
b1 <- rnorm(N, 0, 0.5)
b2 <- rnorm(N, 0, 0.5)

x1 <- foxes_std$area
x2 <- foxes_std$avgfood


lin_mod <- function(a, b1, b2) { 
  
  mu <- a + b1 * x1 + b2 * x2
  
  tibble(a, b1, b2, x1, x2, mu)
  
}

pmap_dfr(list(a, b1, b2), lin_mod, .id = "group") %>% 
  ggplot(aes(x1, mu, group = group)) +
  geom_line(alpha = 0.2) 

## create model

m1 <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b1 * area,
    a ~ dnorm(0, 0.5),
    b1 ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = foxes_std
)

precis(m1)

# dag

library(dagitty)

fox_dag <- dagitty("dag {
    A -> F -> W
    F -> G -> W
}")

coordinates(fox_dag) <- list(
  x = c(A = 1, F = 0, W = 1, G = 2),
  y = c(A = 2, F = 1, W = 0, G = 1)
)

plot(fox_dag)

impliedConditionalIndependencies(fox_dag)

adjustmentSets(fox_dag, exposure = "A", outcome = "W")

# waffle house questions (6H)

data(WaffleDivorce)

d <- WaffleDivorce %>%
    tbl_df()

wh_dag <- dagitty( "dag {
    S -> A -> D
    S -> M -> D
    S -> W -> D
    A -> M
}")

coordinates(wh_dag) <- list(
    x = c(S = 0, W = 2, D = 2, M = 1, A = 0),
    y = c(S = 0, W = 0, D = 2, M = 1, A = 2)
)

plot(wh_dag)

adjustmentSets(wh_dag, exposure = "W", outcome = "D")

ds <- d %>%
    mutate(
        D = scale(Divorce),
        W = scale(WaffleHouses),
        M = scale(Marriage),
        A = scale(MedianAgeMarriage),
        S = South
    )

wh_m1  <- quap(
     alist(
        D ~ dnorm(mu, sigma),
        mu <- a + bw * W + bs * S,
        a ~ dnorm(0, 0.2),
        bw ~ dnorm(0, 0.5),
        bs ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = ds
)

precis(wh_m1)

adjustmentSets(wh_dag, exposure = "S", outcome = "D")

wh_m2  <- quap(
     alist(
        W ~ dnorm(mu, sigma),
        mu <- a + bm * M + bs * S,
        a ~ dnorm(0, 0.2),
        bs ~ dnorm(0, 0.5),
        bm ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = ds
)

precis(wh_m2)

adjustmentSets(wh_dag, exposure = "M", outcome = "D")

impliedConditionalIndependencies(wh_dag)

