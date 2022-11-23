fetch_watcher_data <- function(chain, era.interval){

  first_era <- era.interval[1] - 1

  last_era <- era.interval[2]

  era_difference <- last_era - first_era

  eras <- list()

  pb <- utils::txtProgressBar(min = 0, max = era_difference, style = 3)

  for(i in 1:era_difference){

    era <- i + first_era

    URL <- url(paste("https://storage.googleapis.com/watcher-csv-exporter/",
                     chain, "_validators_era_", era, ".csv", sep = ""))

    tab <- tryCatch(

      read.csv(URL),
      error = function(e) e

    )

    if (inherits(tab, "error")) {

      close(URL)

      next

    }

    if(sum(colnames(tab) == "active") == 1){

      tab <- tab[tab$active == 1,]

    }


    if(is.null(tab$num_voters)){

      tab <- tab[,colnames(tab) %in% c("era", "session", "block_number", "name",
                                       "stash_address", "controller_address",
                                       "commission_percent", "self_stake", "voters",
                                       "total_stake", "num_stakers", "era_points")]

      colnames(tab)[colnames(tab) == "voters"] <- "num_voters"

    } else if(!is.null(tab$num_voters)){

      tab <- tab[,colnames(tab) %in% c("era", "session", "block_number", "name",
                                       "stash_address", "controller_address",
                                       "commission_percent", "self_stake", "num_voters",
                                       "total_stake", "num_stakers", "era_points")]

    }

    eras[[i]] <- tab

    utils::setTxtProgressBar(pb, i)

  }

  eras.clean <- eras[!unlist(lapply(eras, is.null))]

  eras_data <- do.call(rbind, eras.clean)

  return(list(eras = eras_data, chain = chain, interval = era.interval))

}

fetch_candidates <- function(){

  URL <- url("https://polkadot.w3f.community/candidates")

  candidates <- fromJSON(file = URL, unexpected.escape = "skip")

  n <- length(candidates)

  data <- data.frame(NA, ncol = 9, nrow = n)

  pb <- utils::txtProgressBar(min = 0, max = n, style = 3)

  for(i in 1:n){

    data[i,1] <- candidates[[i]]$name
    data[i,2] <- candidates[[i]]$stash
    data[i,3] <- candidates[[i]]$identity$name

    if(is.null(candidates[[i]]$identity$sub)){

      data[i,4] <- NA

    } else {

      data[i,4] <- candidates[[i]]$identity$sub

    }

    data[i,5] <- candidates[[i]]$identity$verified
    data[i,6] <- candidates[[i]]$identity$`_id`

    if(is.null(candidates[[i]]$location)){

      data[i,7] <- NA

    } else {

      data[i,7] <- candidates[[i]]$location

    }

    data[i,8] <- candidates[[i]]$provider

    if(is.null(candidates[[i]]$councilVotes)){

      data[i,9] <- NA

    } else {

      data[i,9] <- length(candidates[[i]]$councilVotes)

    }

    if(is.null(candidates[[i]]$democracyVoteCount)){

      data[i,10] <- NA

    } else {

      data[i,10] <- candidates[[i]]$democracyVoteCount

    }

    utils::setTxtProgressBar(pb, i)

  }

  colnames(data) <- c("name",
                      "stash_address",
                      "id_name",
                      "id_sub",
                      "id_verified",
                      "id_id",
                      "location",
                      "provider",
                      "councilVoteCount",
                      "democracyVoteCount")

  return(data)

}

update_watcher_data <- function(data, era){

  chain <- data$chain

  max_era <- data$interval[2]

  if(era == max_era){

    print("Dataset up to date.")

    return(data)

  } else {

    start <- max_era + 1

    new_eras <- fetch_watcher_data(chain = chain, era.interval = c(start, era))

    updated <- list(

      eras = rbind(data$eras, new_eras$eras),
      chain = chain,
      interval = c(data$interval[1], era)

    )

    return(updated)

  }

}

select_validator <- function(data, look.back = 40, criteria){

  last_era <- max(data$eras$era)

  with_id <- data$eras[!data$eras$name == "",]

  sub <- subset(with_id, era >= (last_era - look.back))

  sum <- data.frame(group_by(sub, stash_address, name) %>%
                      summarise(sum(era_points >= 40000)/length(era_points),
                                ep = mean(era_points),
                                n = length(era_points),
                                comm = mean(commission_percent),
                                ss = mean(self_stake)/10^10,
                                ts = mean(total_stake)/10^10))

  colnames(sum) <- c("stash_address", "validator_name", "pct", "m_era", "n_active", "m_comm", "m_self", "m_total")

  selection <- subset(sum, m_self >= criteria$self &
                        m_total <= criteria$total &
                        pct >= criteria$pct &
                        m_comm <= criteria$comm &
                        n_active <= criteria$n & n_active > 1 &
                        m_era >= criteria$era_points)

  return(selection)

}

sync_validators <- function(data, names, look.back){


  data_sel <- data$eras[data$eras$name %in% names,]

  data_sel <- data_sel[data_sel$era <= data$interval[2] & data_sel$era >= (data$interval[2] - look.back) ,]

  era_coverage <- data.frame(group_by(data_sel, era) %>% summarise(n = length(unique(name))))


  final_selection <- list()

  for (k in 1:16){#multiple selection rounds for backup coverage

    eras <- c()

    partial_selection <- c()

    names_left <- names[!names %in% unlist(final_selection)]

    for(j in 1:16){

      best_cov <- c()

      for(i in 1:length(names_left)){ #for each val, maxim history covered eras within the specified interval

        era_covered <- data_sel[data_sel$name %in% names_left[i],]$era

        sum_cov <- unique(c(era_covered, eras))

        best_cov[i] <- sum(era_coverage$era %in% sum_cov)

      }

      sel_names <- names_left[best_cov == max(best_cov)] #prioritize val with best coverage

      if(sum(sel_names %in% partial_selection) > 1){

        break

      }

      if(length(sel_names) > 1){#if multiple names with best coverage, further selection with average era points

        sub_sel <- data_sel[data_sel$name %in% sel_names,]

        sum_sub_sel <- group_by(sub_sel, name) %>% summarize(m = mean(era_points))

        sel_name <- sum_sub_sel[sum_sub_sel$m == max(sum_sub_sel$m),]$name

        partial_selection[j] <- sel_name

        eras <- c(eras, data_sel[data_sel$name %in% sel_name,]$era)

      } else if(length(sel_names) == 1){

        partial_selection[j] <- sel_names

        eras <- c(eras, data_sel[data_sel$name %in% sel_names,]$era)

      }

      progress <- sum(era_coverage$era %in% eras)/length(era_coverage$era)*100

      print(progress)

      if(progress == 100){

        break

      }

    }

    final_selection[[k]] <- partial_selection

    if(length(unlist(final_selection)) > 16){

      break

    }

  }

  return(final_selection)

}

plot_validator <- function(data, validator.name, look.back = 80){

  max.era <- max(data$era)

  min.era <- max.era - look.back

  filter <- data$name %in% validator.name

  val.data <- data[filter,]

  val.data <- subset(val.data, era >= min.era)

  par(mfrow=c(2,2))

  plot(val.data$era, val.data$era_points, type ="b", xlab = "Eras", ylab = "Era Points", ylim = c(0, 90000))
  abline(h = 40000, lty = 2, col = "grey70")

  plot(val.data$era, val.data$self_stake/10^10, type ="b", xlab = "Eras", ylab = "Self Stake", ylim = c(1000, 15000))
  abline(h = 5000, lty = 2, col = "grey70")

  plot(val.data$era, val.data$total_stake/10^10, type ="b", xlab = "Eras", ylab = "Total Stake", ylim = c(1800000, 2800000))
  abline(h = 2000000, lty = 2, col = "grey70")

  plot(val.data$era, val.data$commission_percent, type ="b", xlab = "Eras", ylab = "Commission (%)", ylim = c(0, 10))
  abline(h = 5, lty = 2, col = "grey70")

}

plot_coverage <- function(data, names, look.back){

  last_era <- data$interval[2]

  par(mfrow = c(2,1))

  for(i in 1:length(names)){

    sub_data <- subset(data$eras, name == names[i])

    col <- ifelse(sub_data$era_points <= 40000, "red", ifelse(sub_data$era_points <= 60000 & sub_data$era_points > 40000, "orange", "green"))

    if(i == 1){

      plot(sub_data$era, rep(i, length(sub_data$era)), col = col, pch = 19, cex = 0.5,
           xlim = c(last_era - look.back, last_era), ylim =c(1,length(names)), xlab = "Era", ylab = "Validator ID")

    }

    abline(h = i, col = "grey", lwd = 0.2)
    points(sub_data$era, rep(i, length(sub_data$era)), col = col, pch = 19, cex = 0.5)

  }

  for(i in 1:length(names)){

    sub_data <- subset(eras_data$eras, name == names[i])

    if(i == 1){

      plot(sub_data$era, rep(1, length(sub_data$era)), cex = 3, pch = 19, col = rgb(0,0,0,0.2),
           xlim = c(last_era - look.back, last_era), ylim =c(0,2), xlab = "Era", ylab = "Validator ID")
      abline(h = 1, col = "grey", lwd = 0.2)

    }

    points(sub_data$era, rep(1, length(sub_data$era)), col = rgb(0,0,0,0.2), pch = 19, cex = 3)

  }


}




