fetch_data <- function(chain, era.interval){

  first_era <- era.interval[1] - 1

  last_era <- era.interval[2]

  era_difference <- last_era - first_era

  eras <- list()

  pb <- utils::txtProgressBar(min = 0, max = era_difference, style = 3)

  for(i in 1:era_difference){

    utils::setTxtProgressBar(pb, i)

    era <- i + first_era

    tab <- tryCatch(

      read.csv(url(paste("https://storage.googleapis.com/watcher-csv-exporter/",
                         chain, "_validators_era_", era, ".csv", sep = ""))),
      error = function(e) e

    )

    if (inherits(tab, "error")) {

      next

    }

    if(sum(colnames(tab) == "active") == 1){

      tab <- tab[tab$active == 1,]

    }

    tab <- tab[,colnames(tab) %in% c("era", "session", "block_number", "name",
                                     "stash_address", "controller_address",
                                     "commission_percent", "self_stake",
                                     "total_stake", "num_stakers", "era_points")]

    eras[[i]] <- tab

  }

  eras.clean <- eras[!unlist(lapply(eras, is.null))]

  eras_data <- do.call(rbind, eras.clean)

  return(list(eras = eras_data, chain = chain, interval = era.interval))

}

update_data <- function(data, era){

  chain <- data$chain

  max_era <- data$interval[2]

  if(era == max_era){

    print("Dataset up to date.")

    return(data)

  } else {

    start <- max_era + 1

    new_eras <- fetch_data(chain = chain, era.interval = c(start, era))

    updated <- list(

      eras = rbind(data$eras, new_eras$eras),
      chain = chain,
      interval = c(data$interval[1], era)

    )

    return(updated)

  }

}

plot_data <- function(data, validator.name, look.back = 80){

  max.era <- max(data$era)

  min.era <- max.era - look.back

  filter <- data$name %in% validator.name

  val.data <- data[filter,]

  val.data <- subset(val.data, era >= min.era)

  par(mfrow=c(2,2))

  plot(val.data$era, val.data$era_points, type ="b", xlab = "Eras", ylab = "Era Points", ylim = c(0, 90000))

  plot(val.data$era, val.data$self_stake/10^10, type ="b", xlab = "Eras", ylab = "Self Stake")

  plot(val.data$era, val.data$total_stake/10^10, type ="b", xlab = "Eras", ylab = "Total Stake")

  plot(val.data$era, val.data$commission_percent, type ="b", xlab = "Eras", ylab = "Commission (%)")

}

select_validator <- function(data, look.back = 80, criteria){

  last_era <- max(data$eras$era)

  sub <- subset(data$eras,era > (last_era - look.back))

  sum <- data.frame(group_by(sub, name) %>%
                      summarise(sum(era_points > 50000)/length(era_points),
                                n = length(era_points),
                                comm = mean(commission_percent),
                                ss = mean(self_stake)/10^10,
                                ts = mean(total_stake)/10^10))

  colnames(sum) <- c("name","pct", "n", "m_comm", "m_self", "m_total")

  selection <- subset(sum, m_self > criteria$self &
                        m_total < criteria$total &
                        pct > criteria$pct & pct < 1 &
                        m_comm <= criteria$comm &
                        n >= criteria$n)

  return(selection)

}
