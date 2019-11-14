library(tidyverse)
library(rethinking)
library(jtcr)

theme_set(theme_jtc())

# poisson regression
data(Kline)

d <- Kline

d %>%
    ggplot(aes(log(population))) +
        geom_histogram()

d$log_pop_scaled <- scale(log(d$population))

d$contact_id <- if_else(d$contact == "high", 2, 1)

mean_prior <- 3
sd_prior <- 0.5
N <- 1e5

tibble(lambda_prior = rlnorm(N, mean_prior, sd_prior)) %>%
    ggplot(aes(lambda_prior)) +
        geom_density() +
        xlim(c(0, 100))

a_prior <- rnorm(N, mean_prior, sd_prior)
lambda_prior <- exp(a_prior)
mean(lambda_prior)

N_sim <- 100
a <- rnorm(N_sim, mean_prior, sd_prior)
b <- rnorm(N_sim, 0, 0.2)

tibble(a, b) %>% 
    mutate(group = row_number()) %>%
    crossing(x = seq(-2, 2, by = 0.1)) %>%
    mutate(y = exp(a + b * x)) %>%
    ggplot(aes(x, y, group = group)) +
        geom_line() +
        ylim(c(0, 100))


tools_dat <- list(
    pop = d$log_pop_scaled,
    tools = d$total_tools,
    contact = ifelse(d$contact == "high", 2L, 1L)
)

m11_10 <- ulam(
    alist(
        tools ~ dpois(lambda),
        log(lambda) <- a[contact] + b[contact] * pop,
        a[contact] ~ dnorm(3, 0.5),
        b[contact] ~ dnorm(0, 0.2)
    ),
    data = tools_dat
)

precis(m11_10, depth = 2)

pred_dat <- crossing(
    contact = c(1, 2), 
    pop = seq(-3, 3, length.out = 100)
)

lambda_pred <- link(m11_10, data = pred_dat)

lambda_mean <- apply(lambda_pred, 2, mean)
lambda_pi <- apply(lambda_pred, 2, PI)

preds <- pred_dat %>%
    mutate(
        lamba_mean = lambda_mean,
        lambda_high = lambda_pi[2, ],
        lambda_low = lambda_pi[1, ]
)

ggplot() +
    geom_point(
        aes(population, total_tools, color = contact),
        data = d,
        size = 2
    ) +
    geom_line(
        aes(exp(pop * 1.53 + 9), 
            lambda_mean, color = factor(contact)),
        data = preds,
        size = 1.5
    ) +
    geom_ribbon(
        aes(exp(pop * 1.53 + 9), 
            ymax = lambda_high, ymin = lambda_low,
            fill = factor(contact)),
        data = preds,
        alpha = 0.2
    )
