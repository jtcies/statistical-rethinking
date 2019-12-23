library(tidyverse)
library(rethinking)

# question 12H1

data(bangladesh)

bangladesh_processed <- bangladesh %>%
    mutate(district_id = as.integer(as.factor(district)))

dat_list <- list(
    children = bangladesh_processed$living.children,
    urban = bangladesh_processed$urban,
    contraception = bangladesh_processed$use.contraception,
    district_id = bangladesh_processed$district_id
)

fixed_model <- ulam(
    alist(
        contraception ~ dbinom(1, p),
        logit(p) <- a + b[district_id],
        a ~ dnorm(0, 1),
        b[district_id] ~ dnorm(0, 0.5)
    ), 
    data = dat_list
)

mixed_model <- ulam(
    alist(
        contraception ~ dbinom(1, p),
        logit(p) <- a_bar + b[district_id] * sigma_dist,
        b[district_id] ~ dnorm(0, 1),
        sigma_dist ~ dexp(1),
        a_bar ~ dnorm(0, 0.5)
    ), 
    data = dat_list
)

preds <- data.frame(district_id = 1:60)

apply_preds <- function(model, data) {

    dat <- link(model, data)
    mean <- apply(dat, 2, mean)
    interval <- apply(dat, 2, PI) 
    list(mean = mean, lower = interval[1, ], higher = interval[2, ])

}

fixed_preds <- apply_preds(fixed_model, preds)
mixed_preds <- apply_preds(mixed_model, preds)


