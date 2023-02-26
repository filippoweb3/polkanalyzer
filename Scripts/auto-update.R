library(rjson)

candidates <- fetch_candidates()

usethis::use_data(candidates, overwrite = T)

