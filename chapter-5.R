library(tidyverse)
library(rethinking)

theme_set(theme_minimal())
data(milk)

d <- milk

d$K <- scale( d$kcal.per.g )
d$N <- scale( d$neocortex.perc )
d$M <- scale( log(d$mass) )

dcc <- d[ complete.cases(d$K,d$N,d$M) , ] %>% 
  tbl_df()

m5.7 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bN*N + bM*M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bN ~ dnorm( 0 , 0.5 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data=dcc )

precis(m5.7)


xseq <- seq( from=min(dcc$M)-0.15 , to=max(dcc$M)+0.15 , length.out=30 )

mu <- link(m5.7 , data = data.frame(M = xseq , N = 0))

data.frame(
  mu_mean = apply(mu,2,mean),
  low_bound = apply(mu,2,PI)[1, ],
  high_bound = apply(mu,2,PI)[2, ]
)

# hard practice problems

data(foxes)

foxes <- tbl_df(foxes) %>%
    tbl_df()

foxes_std <- foxes %>%
    mutate_at(vars(avgfood, groupsize, area, weight), scale)

m1 <- quap(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + b * area,
        a ~ dnorm(0, 0.5),
        b ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = foxes_std
)

precis(m1)

area_sim <- seq(min(foxes_std$area) - 0.15, max(foxes_std$area) + 0.15,
                length.out = nrow(foxes_std))

m1_mu <- link(m1, data = data.frame(area = area_sim))

data.frame(
    area = area_sim,
    mu_mean = apply(m1_mu, 2, mean),
    low_bound = apply(m1_mu, 2, PI)[1, ],
    high_bound = apply(m1_mu, 2, PI)[2, ]
) %>%
    ggplot(aes(area_sim, mu_mean, ymin = low_bound, ymax = high_bound)) +
        geom_ribbon(alpha = 0.2) +
        geom_line()

m2 <- quap(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + b * groupsize,
        a ~ dnorm(0, 0.5),
        b ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = foxes_std
)

precis(m2)

groupsize_sim <- seq(min(foxes_std$groupsize) - 0.15,
                     max(foxes_std$groupsize) + 0.15,
                     length.out = nrow(foxes_std))

m2_mu <- link(m2, data = data.frame(groupsize = groupsize_sim))

data.frame(
    groupsize = groupsize_sim,
    mu_mean = apply(m2_mu, 2, mean),
    low_bound = apply(m2_mu, 2, PI)[1, ],
    high_bound = apply(m2_mu, 2, PI)[2, ]
) %>%
    ggplot(aes(groupsize_sim, mu_mean, ymin = low_bound, ymax = high_bound)) +
        geom_ribbon(alpha = 0.2) +
        geom_line()

# 5H2

m3 <- quap(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + b1 * area + b2 * groupsize,
        a ~ dnorm(0, 0.5),
        b1 ~ dnorm(0, 0.5),
        b2 ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = foxes_std
)

m3_mu <- link(m3, data = data.frame(groupsize = groupsize_sim, area = area_sim))

data.frame(
    area = area_sim,
    groupsize = groupsize_sim,
    mu_mean = apply(m3_mu, 2, mean),
    low_bound = apply(m3_mu, 2, PI)[1, ],
    high_bound = apply(m3_mu, 2, PI)[2, ]
) %>%
    gather(var, val, 1:2) %>%
    ggplot(aes(val, mu_mean, ymin = low_bound, ymax = high_bound)) +
        geom_ribbon(alpha = 0.2) +
        geom_line() +
        facet_wrap(~ var)

m5_mu <- link(m3, data = data.frame(groupsize = 0, area = area_sim))

data.frame(
    area = area_sim,
    mu_mean = apply(m5_mu, 2, mean),
    low_bound = apply(m5_mu, 2, PI)[1, ],
    high_bound = apply(m5_mu, 2, PI)[2, ]
) %>%
    ggplot(aes(area_sim, mu_mean, ymin = low_bound, ymax = high_bound)) +
        geom_ribbon(alpha = 0.2) +
        geom_line()

m4_mu <- link(m3, data = data.frame(groupsize = groupsize_sim, area = 0))

data.frame(
    groupsize = groupsize_sim,
    mu_mean = apply(m4_mu, 2, mean),
    low_bound = apply(m4_mu, 2, PI)[1, ],
    high_bound = apply(m4_mu, 2, PI)[2, ]
) %>%
    ggplot(aes(groupsize_sim, mu_mean, ymin = low_bound, ymax = high_bound)) +
        geom_ribbon(alpha = 0.2) +
        geom_line()
