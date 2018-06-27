library(tidyverse)
library(simrel)
load("design.rdata")
source("00-function.r")

rep <- 1:100

sfun <- function(seed=NULL) {
  set.seed(seed)
  design %>% 
    get_design(1) %>% 
    simulate()
}

sobj <- sfun(seed = 123)

theo_est_err <- sobj$minerror/(sobj$n - (sobj$p - 2)) * 
  sum(map_dbl(1:sobj$p, ~exp(sobj$gamma * (.x - 1))))
print(theo_est_err)

sobj <- sfun()
