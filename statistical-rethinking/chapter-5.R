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
  mu_mean = apply(mu,2,mean)
  low_bound = apply(mu,2,PI)[1, ],
  high_bound = apply(mu,2,PI)[2, ]
)


