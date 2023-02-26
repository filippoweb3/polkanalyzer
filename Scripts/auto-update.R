library(rjson)
library(stringdist)
library(countrycode)
library(maps)
library(Polkanalyzer)

candidates <- fetch_candidates()

usethis::use_data(candidates, overwrite = T)

