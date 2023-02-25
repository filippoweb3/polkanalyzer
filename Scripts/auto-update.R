library(rjson)
library(dplyr)

devtools::load_all(".")

load("data/eras_data.rda")
load("data/candidates.rda")

candidates <- fetch_candidates()
usethis::use_data(candidates, overwrite = T)

eras_data <- update_watcher_data(data = eras_data, era = 997)
usethis::use_data(eras_data, overwrite = T)
