library(rjson)
library(dplyr)

# Load data ----

#eras_data <- fetch_watcher_data(chain = "polkadot", era.interval = c(165, 886))

#usethis::use_data(eras_data, overwrite = T)

candidates <- fetch_candidates()
usethis::use_data(candidates, overwrite = T)

# Update watcher data ----

eras_data <- update_watcher_data(data = eras_data, era = 915)

usethis::use_data(eras_data, overwrite = T)

# Select Validators ----

selection <- select_validator(data = eras_data, look.back = 50,
                              criteria = list(pct = 0.8,
                                              self = 5800,
                                              total = 2500000,
                                              comm = 5,
                                              n = 30, era_points = 50000))

selection <- merge(selection, candidates, by = "stash_address")

selection <- selection[!selection$provider == "Hetzner Online GmbH" &
                         selection$id_verified == TRUE &
                         selection$democracyVoteCount >= 3 &
                         selection$councilVoteCount >= 3 &
                         selection$n_subid <= 3,]

val_names <- as.vector(na.omit(selection$validator_name))

sync_val <- sync_validators(data = eras_data, names = val_names, look.back = 50)

# Plots ----

plot_validator(data = eras_data$eras, sync_val[[1]][1], look.back = 50)


pct_less_100_comm <- group_by(eras_data$eras, era) %>% summarise(sum(commission_percent < 100)/length(commission_percent)*100)

plot(pct_less_100_comm, xlab = "Eras", ylab = "Pct Valitators < 100% comm", type = "l")


plot_coverage(data = eras_data, names = sync_val[[1]], look.back = 40)

