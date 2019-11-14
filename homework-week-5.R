library(rethinking)
library(tidyverse)

data("Wines2012")

d <- Wines2012 %>% 
  janitor::clean_names() %>% 
  tbl_df()
