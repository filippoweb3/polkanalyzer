library(rjson)
library(dplyr)

# Load data ----

#eras_data <- fetch_watcher_data(chain = "polkadot", era.interval = c(165, 886))

#usethis::use_data(eras_data, overwrite = T)

candidates <- fetch_candidates()
usethis::use_data(candidates, overwrite = T)

# Update watcher data ----

eras_data <- update_watcher_data(data = eras_data, era = 895)

usethis::use_data(eras_data, overwrite = T)

# Select Validators ----

selection <- select_validator(data = eras_data, look.back = 30,
                              criteria = list(pct = 0.6,
                                              self = 5000,
                                              total = 2500000,
                                              comm = 5,
                                              n = 15, era_points = 55000))

selection <- merge(selection, candidates, by = "stash_address")

selection <- selection[!selection$provider == "Hetzner Online GmbH" &
                         selection$id_verified == TRUE &
                         selection$democracyVoteCount >= 1 &
                         selection$councilVoteCount >= 1,]

val_names <- na.omit(selection$validator_name)

sync_validators(data = eras_data, names = val_names, look.back = 30)

# Plots ----

plot_validator(data = eras_data$eras, val_names[1])


pct_less_100_comm <- group_by(eras_data$eras, era) %>% summarise(sum(commission_percent < 100)/length(commission_percent)*100)

plot(pct_less_100_comm, xlab = "Eras", ylab = "Pct Valitators < 100% comm", type = "l")


plot_coverage(data = eras_data, names = val_names, look.back = 30)

