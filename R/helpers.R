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




