library(dplyr)

# Load data ----

#eras_data <- fetch_watcher_data(chain = "polkadot", era.interval = c(165, 886))

usethis::use_data(eras_data, overwrite = T)

# Update data ----

eras_data <- update_watcher_data(data = eras_data, era = 889)

usethis::use_data(eras_data, overwrite = T)

selection <- select_validator(data = eras_data, look.back = 80,
                 criteria = list(pct = 0.6,
                                 self = 1000,
                                 total = 2300000,
                                 comm = 5,
                                 n = 8))

val_names <- selection$name

plot_data(data = eras_data$eras, val_names[1])

val_names <- unique(eras_data$name)
val_names <- val_names[!val_names == ""]
val_name <- val_names[order(val_names)]
val_ID <- c(1:length(val_name))







# Plots ----

pct_less_100_comm <- group_by(eras_data$eras, era) %>% summarise(sum(commission_percent < 100)/length(commission_percent)*100)

plot(pct_less_100_comm, xlab = "Eras", ylab = "Pct Valitators < 100% comm", type = "l")



for(i in 1:length(val_names)){

  last_era <- eras_data$interval[2]

  sub_data <- subset(eras_data$eras, name == val_names[i])

  col <- ifelse(sub_data$era_points <= 40000, "red", ifelse(sub_data$era_points <= 60000 & sub_data$era_points > 40000, "orange", "green"))

  if(i == 1){

    plot(sub_data$era, rep(i, length(sub_data$era)), col = col, pch = 19, cex = 0.5, xlim = c(800, last_era), ylim =c(1,length(val_names)))

  }

  abline(h = i, col = "grey", lwd = 0.2)
  points(sub_data$era, rep(i, length(sub_data$era)), col = col, pch = 19, cex = 0.5)

}
