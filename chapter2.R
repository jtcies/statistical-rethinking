library(tidyverse)
library(rethinking)

# grid

prior_grid <- seq(0, 1, length.out = 100)

prior <- ifelse(prior_grid < 0.5, 0, 1)

likelihood <- dbinom(6, size = 9, prob = prior_grid)

posterior <- prior * likelihood

sp <- posterior / sum(posterior)

qplot(prior_grid, sp, geom = "line")



## 2.6

globe_qa <- quap(
    alist(
        W ~ dbinom(W + L, p),
        p <- dunif(0, 1)
    ), data = list(W = 6, L = 3)
)

precis(globe_qa)

#2.8

n_samples <- 1000
p <- rep( NA, n_samples )
p[1] <- 0.5
W <- 6
L <- 3
for ( i in 2:n_samples ) {
    p_new <- rnorm( 1, p[ i - 1 ], 0.1 )
    if ( p_new < 0 ) p_new <- abs( p_new )
    if ( p_new > 1 ) p_new <- 2 - p_new
    q0 <- dbinom( W, W + L, p[2 - 1] )
    q1 <- dbinom( W, W + L, p_new )
    p[2] <- ifelse( runif(1) < q1 / q0, p_new, p[2 - 1] )
}

## Homework week 1

### Number 3


