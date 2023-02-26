library(rjson)
library(stringdist)
library(countrycode)
library(maps)
library(Polkanalyzer)

candidates <- fetch_candidates()

usethis::use_data(candidates, overwrite = T)

load("./data/eras_data.rda")

diff <- (Sys.Date() - 1) - as.Date("2020-06-02",format="%Y-%m-%d")

eras_data <- update_watcher_data(data = eras_data, era = diff)

usethis::use_data(eras_data, overwrite = T)



