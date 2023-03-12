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

  data <- data.frame()

  pb <- utils::txtProgressBar(min = 0, max = n, style = 3)

  for(i in 1:n){

    data[i,1] <- candidates[[i]]$name
    data[i,2] <- candidates[[i]]$stash

    if(is.null(candidates[[i]]$offlineAccumulated)){

      data[i,3] <- NA

    } else {

      data[i,3] <- candidates[[i]]$offlineAccumulated

    }

    if(is.null(candidates[[i]]$faults)){

      data[i,4] <- NA

    } else {

      data[i,4] <- candidates[[i]]$faults

    }

    if(is.null(candidates[[i]]$identity$name)){

      data[i,5] <- NA

    } else {

      data[i,5] <- candidates[[i]]$identity$name

    }

    if(is.null(candidates[[i]]$identity$subIdentities)){

      data[i,6] <- NA

    } else {

      data[i,6] <- length(candidates[[i]]$identity$subIdentities)

    }

    if(is.null(candidates[[i]]$identity$verified)){

      data[i,7] <- NA

    } else {

      data[i,7] <- candidates[[i]]$identity$verified

    }

    if(is.null(candidates[[i]]$identity$`_id`)){

      data[i,8] <- NA

    } else {

      data[i,8] <- candidates[[i]]$identity$`_id`

    }

    if(is.null(candidates[[i]]$location)){

      data[i,9] <- NA

    } else {

      data[i,9] <- candidates[[i]]$location

      dist <- stringdist(candidates[[i]]$location, world.cities$name, method = "jw")

      country.data <- world.cities[dist == min(dist),]

      if(length(country.data[,1]) == 1){

        data[i,10] <- country.data$lat
        data[i,11] <- country.data$lon

        country <- country.data$country.etc

        data[i,12] <- country

        data[i,13] <- countrycode(sourcevar = country,
                                  origin = "country.name",
                                  destination = "continent")

      } else {

        onecountry.data <- country.data[country.data$pop == max(country.data$pop),]

        data[i,10] <- onecountry.data$lat
        data[i,11] <- onecountry.data$lon

        country <- onecountry.data$country.etc

        data[i,12] <- country

        data[i,13] <- countrycode(sourcevar = country,
                                  origin = "country.name",
                                  destination = "continent")

      }

    }

    if(is.null(candidates[[i]]$provider)){

      data[i,14] <- NA

    } else {

      data[i,14] <- candidates[[i]]$provider

    }

    if(is.null(candidates[[i]]$councilVotes)){

      data[i,15] <- NA

    } else {

      data[i,15] <- length(candidates[[i]]$councilVotes)

    }

    if(is.null(candidates[[i]]$democracyVoteCount)){

      data[i,16] <- NA

    } else {

      data[i,16] <- candidates[[i]]$democracyVoteCount

    }

    utils::setTxtProgressBar(pb, i)

  }

  colnames(data) <- c("name",
                      "stash_address",
                      "offline",
                      "faluts",
                      "id_name",
                      "n_subid",
                      "id_verified",
                      "id_id",
                      "location",
                      "lat",
                      "lon",
                      "country",
                      "continent",
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
                      summarise(ep = mean(era_points),
                                mp = max(era_points),
                                n = length(era_points),
                                comm = mean(commission_percent),
                                ss = mean(self_stake)/10^10,
                                ts = mean(tail(total_stake, n = 3))/10^10,
                                last_era = abs(max(era)-last_era)))

  colnames(sum) <- c("stash_address", "validator_name", "m_era", "max_era", "n_active", "m_comm", "m_self", "m_total", "last_active")

  selection <- subset(sum, m_self >= criteria$self_stake &
                        m_total <= criteria$total_stake &
                        m_comm <= criteria$commission &
                        n_active <= criteria$n_active & n_active >= 3 &
                        m_era >= criteria$mean_era_points &
                        max_era >= criteria$max_era_points &
                        last_active <= criteria$last_active)

  return(selection)

}

sync_validators <- function(data, names, look.back){

  data_sel <- data$eras

  era_coverage <- seq(data$interval[2] - look.back, data$interval[2])

  final_selection <- list()

  runs <- c()

  pcts <- c()

  for (k in 1:5){

    eras <- c()

    partial_selection <- list()

    names_left <- names[!names %in% unlist(final_selection)]

    for(j in 1:16){

      names_left_partial <- names_left[!names_left %in% unlist(partial_selection)]

      if(length(names_left_partial) == 0){

        break

      }

      runs <- c(runs, k)

      best_cov <- c()

      for(i in 1:length(names_left_partial)){

        era_covered <- data_sel[data_sel$name %in% names_left_partial[i],]$era

        sum_cov <- unique(c(era_covered, eras))

        best_cov[i] <- sum(era_coverage %in% sum_cov)

      }

      sel_names <- names_left_partial[best_cov == max(best_cov)]

      if(length(sel_names) > 1){

        sub_sel <- data_sel[data_sel$name %in% sel_names,]

        sum_sub_sel <- group_by(sub_sel, name) %>% summarize(m = mean(self_stake))

        sel_names <- sum_sub_sel[sum_sub_sel$m == max(sum_sub_sel$m),]$name

        partial_selection[[j]] <- sel_names

      } else if(length(sel_names) == 1){

        partial_selection[[j]] <- sel_names

      }

      data_sel_names <- data_sel[data_sel$name %in% sel_names,]

      eras <- unique(c(eras, data_sel_names[data_sel_names$era %in% era_coverage,]$era))

      progress <- length(eras)/length(era_coverage)*100

      pcts <- c(pcts, progress)

      if(progress == 100){

        break

      }

    }

    final_selection[[k]] <- unlist(partial_selection)

  }

  out <- data.frame(run = runs, coverage = pcts, validator_name = unlist(final_selection))

  return(out)

}

plot_validator <- function(data, validator.name, look.back = 80){

  max.era <- max(data$era)

  min.era <- max.era - look.back

  filter <- data$name %in% validator.name

  val.data <- data[filter,]

  val.data <- subset(val.data, era >= min.era)

  par(mfrow=c(2,2))

  plot(val.data$era, val.data$era_points, type ="b", xlab = "Eras", ylab = "Era Points", ylim = c(0, 100000))
  abline(h = 60000, lty = 2, col = "grey70")

  plot(val.data$era, val.data$self_stake/10^10, type ="b", xlab = "Eras", ylab = "Self Stake", ylim = c(3000, 30000))
  abline(h = 5000, lty = 2, col = "grey70")

  plot(val.data$era, val.data$total_stake/10^10, type ="b", xlab = "Eras", ylab = "Total Stake", ylim = c(1000000, 3000000))
  abline(h = 1800000, lty = 2, col = "grey70")

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




