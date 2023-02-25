library(rjson)
library(dplyr)

load("data/candidates.rda")

candidates1 <- candidates

usethis::use_data(candidates1, overwrite = T)

