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



data_sel <- eras_data$eras[eras_data$eras$name %in% val_names,]

data_sel <- data_sel[data_sel$era <= eras_data$interval[2] & data_sel$era >= (eras_data$interval[2] - 30) ,]

era_coverage <- data.frame(group_by(data_sel, era) %>% summarise(n = length(unique(name))))



final_selection <- c()

eras <- c()

for(j in 1:16){

  names_left <- val_names[!val_names %in% final_selection]

  best_cov <- c()

  for(i in 1:length(names_left)){ #for each val, maxim history covered eras within the specified interval

    era_covered <- data_sel[data_sel$name %in% names_left[i],]$era

    sum_cov <- unique(c(era_covered, eras))

    best_cov[i] <- sum(era_coverage$era %in% sum_cov)

  }

  sel_names <- names_left[best_cov == max(best_cov)] #prioritize val with best coverage

  if(length(sel_names) > 1){#if multiple names with best coverage, further selection with average era points

    sub_sel <- data_sel[data_sel$name %in% sel_names,]

    sum_sub_sel <- group_by(sub_sel, name) %>% summarize(m = mean(era_points))

    sel_name <- sum_sub_sel[sum_sub_sel$m == max(sum_sub_sel$m),]$name

    final_selection[j] <- sel_name

    eras <- c(eras, data_sel[data_sel$name %in% sel_name,]$era)

  } else if(length(sel_names) == 1){

    final_selection[j] <- sel_names

    eras <- c(eras, data_sel[data_sel$name %in% sel_names,]$era)

  }

  progress <- sum(era_coverage$era %in% eras)/length(era_coverage$era)*100

  print(progress)

  if(progress == 100){

    break

  }

}








# Plots ----

plot_validator(data = eras_data$eras, val_names[1])



pct_less_100_comm <- group_by(eras_data$eras, era) %>% summarise(sum(commission_percent < 100)/length(commission_percent)*100)

plot(pct_less_100_comm, xlab = "Eras", ylab = "Pct Valitators < 100% comm", type = "l")


plot_coverage(data = eras_data, names = val_names, look.back = 30)

