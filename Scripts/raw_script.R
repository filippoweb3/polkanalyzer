library(rjson)
library(dplyr)
library(maps)
library(countrycode)

# Fetch candidate data ----

#eras_data <- fetch_watcher_data(chain = "polkadot", era.interval = c(165, 886))

#usethis::use_data(eras_data, overwrite = T)

candidates <- fetch_candidates()
usethis::use_data(candidates, overwrite = T)

# Update watcher data ----

eras_data <- update_watcher_data(data = eras_data, era = 997)

usethis::use_data(eras_data, overwrite = T)

# Select Validators ----

selection <- select_validator(data = eras_data, look.back = 30,
                              criteria = list(self_stake = 5000,
                                              total_stake = 2130000,
                                              commission = 5,
                                              n_active = 31,
                                              mean_era_points = 50000,
                                              max_era_points = 80000,
                                              last_active = 31))

selection <- merge(selection, candidates, by = "stash_address")

selection <- selection[!selection$provider == "Hetzner Online GmbH" &
                         selection$id_verified == TRUE &
                         selection$democracyVoteCount >= 1 &
                         selection$councilVoteCount >= 1 &
                         selection$n_subid <= 3 &
                         selection$faluts <= 0 &
                         selection$offline <= 0,]

na.omit(selection[,colnames(selection) %in% c("validator_name",
                                       "m_era",
                                       "max_era",
                                       "n_active",
                                       "m_comm",
                                       "m_self",
                                       "m_total",
                                       "last_active",
                                       "location",
                                       "continent",
                                       "provider")])


minorities <- selection$validator_name[selection$continent %in%
                          c("Africa", "Asia", "Oceania")]

minorities

val_names <- as.vector(na.omit(selection$validator_name))

# Synchronize Validators ----

sync_val <- sync_validators(data = eras_data, names = val_names, look.back = 30)

sync_sel <- merge(sync_val, selection,by = "validator_name")

sync_sel <- sync_sel[order(sync_sel$run, sync_sel$coverage),]

# Plots ----

plot_validator(data = eras_data$eras, sync_val[[1]][1], look.back = 30)

pct_less_100_comm <- group_by(eras_data$eras, era) %>% summarise(sum(commission_percent < 100)/length(commission_percent)*100)

plot(pct_less_100_comm, xlab = "Eras", ylab = "Pct Valitators < 100% comm", type = "l")

plot_coverage(data = eras_data, names = sync_val[[1]], look.back = 30)

