library(rjson)
library(dplyr)

# Load data ----

#eras_data <- fetch_watcher_data(chain = "polkadot", era.interval = c(165, 886))

#usethis::use_data(eras_data, overwrite = T)

candidates <- fetch_candidates()

usethis::use_data(candidates, overwrite = T)

# Update watcher data ----

eras_data <- update_watcher_data(data = eras_data, era = 891)

usethis::use_data(eras_data, overwrite = T)

# Select Validators ----

selection <- select_validator(data = eras_data, look.back = 30,
                              criteria = list(pct = 0.6,
                                              self = 5000,
                                              total = 2500000,
                                              comm = 5,
                                              n = 5, era_points = 55000))

selection <- merge(selection, candidates, by = "stash_address")

selection <- selection[!selection$provider == "Hetzner Online GmbH" &
                         selection$id_verified == TRUE &
                         selection$democracyVoteCount >= 10 &
                         selection$councilVoteCount >= 1,]

val_names <- selection$validator_name

plot_validator(data = eras_data$eras, val_names[1])

# Plots ----

pct_less_100_comm <- group_by(eras_data$eras, era) %>% summarise(sum(commission_percent < 100)/length(commission_percent)*100)

plot(pct_less_100_comm, xlab = "Eras", ylab = "Pct Valitators < 100% comm", type = "l")



last_era <- eras_data$interval[2]

par(mfrow = c(2,1))

for(i in 1:length(val_names)){

  sub_data <- subset(eras_data$eras, name == val_names[i])

  col <- ifelse(sub_data$era_points <= 40000, "red", ifelse(sub_data$era_points <= 60000 & sub_data$era_points > 40000, "orange", "green"))

  if(i == 1){

    plot(sub_data$era, rep(i, length(sub_data$era)), col = col, pch = 19, cex = 0.5,
         xlim = c(last_era - 30, last_era), ylim =c(1,length(val_names)), xlab = "Era", ylab = "Validator ID")

  }

  abline(h = i, col = "grey", lwd = 0.2)
  points(sub_data$era, rep(i, length(sub_data$era)), col = col, pch = 19, cex = 0.5)

}

for(i in 1:length(val_names)){

  sub_data <- subset(eras_data$eras, name == val_names[i])

  if(i == 1){

  plot(sub_data$era, rep(1, length(sub_data$era)), cex = 3, pch = 19, col = rgb(0,0,0,0.2),
       xlim = c(last_era - 30, last_era), ylim =c(0,2), xlab = "Era", ylab = "Validator ID")
  abline(h = 1, col = "grey", lwd = 0.2)

  }

  points(sub_data$era, rep(1, length(sub_data$era)), col = rgb(0,0,0,0.2), pch = 19, cex = 3)

}



