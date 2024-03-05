library(dplyr)
library(ggplot2)
library(gridExtra)
library(geckor)

## DOT Price

dot <- coin_history(coin_id = "polkadot", vs_currency = "usd", days = "max")
dot <- as.data.frame(dot)
dot$timestamp <- as.Date(dot$timestamp, format = "%d.%m.%y")
dot <- dot[,c(1,4:6)]
colnames(dot) <- c("date", "price", "volume", "mcap")


## Nodes with < 100% comm. ----

date <- unique(Sys.Date() + (eras_data$eras$era - max(eras_data$eras$era)))

pct_less_100_comm <- group_by(eras_data$eras, era) %>% summarise(n100 = sum(commission_percent < 100), n = length(commission_percent))

pct_less_100_comm <- data.frame(date, pct_less_100_comm)


## Node Total Stake ----

tot_stake <- group_by(eras_data$eras, era) %>%
  summarize(m = mean(total_stake, na.rm = T), se = sd(total_stake, na.rm = T)/sqrt(length(total_stake)))

tot_stake <- data.frame(date = date, tot_stake)

tot_stake <- merge(tot_stake, dot, by = "date")

tot_stake$m.USD <- tot_stake$m * tot_stake$price
tot_stake$se.USD <- tot_stake$se * tot_stake$price


## Stakers Voters Data prep ----

stakers <- group_by(stakers_voters$stakers, era, address) %>%
  summarize(active_bond = sum(active_bond, na.rm = T), .groups = "keep")


voters <- group_by(stakers_voters$voters, era, address) %>%
  select(-c("validator")) %>% filter(!duplicated(address))

sv <- merge(stakers, voters, by = c("era", "address"))

## MAB ----

eras <- unique(sv$era)
n <- length(eras)

mab <- c()

for (i in 1:n){

  sub <- subset(sv, era == eras[i])

  sub <- sub[order(sub$active_bond),][c(1:5),]

  mab[i] <- median(sub$bond, na.rm = T)

}

dates <- Sys.Date() + (eras - max(eras))

mab_data <- data.frame(eras, date = dates, mab)

mab_data <- merge(mab_data, dot, by = "date")

mab_data$mab.USD <- mab_data$mab * mab_data$price

## All Voters and Stakers ----

all_voters <- group_by(voters, era) %>% summarize(n_voters = length(address))

all_stakers <- group_by(stakers, era) %>% summarize(n_stakers = length(address))

all_stakers_voters <- merge(all_voters, all_stakers, by = "era")

all_stakers_voters$date <- Sys.Date() + (all_stakers_voters$era - max(all_stakers_voters$era))

## Summary Stats ----



## Plots ----

lookback = 15


plot1 <- ggplot(data = mab_data, aes(x = date, y = mab)) +
  geom_line() +
  ylab("Minimum Active Bond (DOT)") + xlab("Date") +
  xlim(c(max(date) - lookback, max(date))) + ylim(c(550, 600))

plot1a <- ggplot(data = mab_data, aes(x = date, y = mab.USD)) +
  geom_line() +
  ylab("Minimum Active Bond (USD)") + xlab("Date") +
  xlim(c(max(date) - lookback, max(date))) + ylim(4000, 6000)

data_plot1 <- mab_data[mab_data$date >= (max(mab_data$date) - lookback),]
mab_delta <- round((data_plot1$mab[data_plot1$date == max(data_plot1$date)] - data_plot1$mab[data_plot1$date == min(data_plot1$date)]), 2)
mab_end <- round(data_plot1$mab[data_plot1$date == max(data_plot1$date)], 2)
mab_start <- round(data_plot1$mab[data_plot1$date == min(data_plot1$date)], 2)
mabPct <- round((data_plot1$mab[data_plot1$date == max(data_plot1$date)] - data_plot1$mab[data_plot1$date == min(data_plot1$date)])/data_plot1$mab[data_plot1$date == min(data_plot1$date)]*100, 2)


colors <- c("Voters" = "black", "Stakers" = "blue")

plot2 <- ggplot(data = all_stakers_voters, aes(x = date)) +
         geom_line(aes(y = n_voters, color = "Voters")) +
         geom_line(aes(y = n_stakers, color = "Stakers")) +
  labs(x = "Date", y = "Number of Accounts", color = "Legend") +
  geom_hline(yintercept = 22500, color="grey40", linetype="dotted") +
  scale_color_manual(values = colors) +
  xlim(c(max(date) - lookback, max(date)))

data_plot2 <- all_stakers_voters[all_stakers_voters$date >= (max(all_stakers_voters$date) - lookback),]

stakers_delta <- round((data_plot2$n_stakers[data_plot2$date == max(data_plot2$date)] - data_plot2$n_stakers[data_plot2$date == min(data_plot2$date)]), 2)
stakers_end <- round(data_plot2$n_stakers[data_plot2$date == max(data_plot2$date)], 2)
stakers_start <- round(data_plot2$n_stakers[data_plot2$date == min(data_plot2$date)], 2)
stakersPct <- round((data_plot2$n_stakers[data_plot2$date == max(data_plot2$date)] - data_plot2$n_stakers[data_plot2$date == min(data_plot2$date)])/data_plot2$n_stakers[data_plot2$date == min(data_plot2$date)]*100, 2)

voters_delta <- round((data_plot2$n_voters[data_plot2$date == max(data_plot2$date)] - data_plot2$n_voters[data_plot2$date == min(data_plot2$date)]), 2)
voters_end <- round(data_plot2$n_voters[data_plot2$date == max(data_plot2$date)], 2)
voters_start <- round(data_plot2$n_voters[data_plot2$date == min(data_plot2$date)], 2)
votersPct <- round((data_plot2$n_voters[data_plot2$date == max(data_plot2$date)] - data_plot2$n_voters[data_plot2$date == min(data_plot2$date)])/data_plot2$n_voters[data_plot2$date == min(data_plot2$date)]*100, 2)


plot3 <- ggplot(data = tot_stake, aes(x = date, y = m/10^16)) +
  geom_line() +
  geom_ribbon(aes(ymin = (m - se)/10^16,
                  ymax = (m + se)/10^16), alpha = 0.5) +
  ylab("Average total stake per node (MDOT)") + xlab("Date") +
  xlim(c(max(date) - lookback, max(date))) + ylim(c(2.4, 2.6))

plot3a <- ggplot(data = tot_stake, aes(x = date, y = m.USD/10^16)) +
  geom_line() +
  geom_ribbon(aes(ymin = (m.USD - se.USD)/10^16,
                  ymax = (m.USD + se.USD)/10^16), alpha = 0.5) +
  ylab("Average total stake per node (M USD)") + xlab("Date") +
  xlim(c(max(date) - lookback, max(date))) + ylim(c(15, 25))


data_plot3 <- tot_stake[tot_stake$date >= (max(tot_stake$date) - lookback),]
stake_delta <- round((data_plot3$m[data_plot3$date == max(data_plot3$date)] - data_plot3$m[data_plot3$date == min(data_plot3$date)])/10^10, 2)
stake_start <- round(data_plot3$m[data_plot3$date == min(data_plot3$date)]/10^16, 2)
stake_end <- round(data_plot3$m[data_plot3$date == max(data_plot3$date)]/10^16, 2)
stakePct <- round((data_plot3$m[data_plot3$date == max(data_plot3$date)] - data_plot3$m[data_plot3$date == min(data_plot3$date)])/data_plot3$m[data_plot3$date == min(data_plot3$date)]*100, 2)

stake_delta_se <- round((data_plot3$se[data_plot3$date == max(data_plot3$date)] - data_plot3$se[data_plot3$date == min(data_plot3$date)])/10^10, 2)
stake_start_se <- round(data_plot3$se[data_plot3$date == min(data_plot3$date)]/10^10, 2)
stake_end_se <- round(data_plot3$se[data_plot3$date == max(data_plot3$date)]/10^10, 2)
stakePct_se <- round((data_plot3$se[data_plot3$date == max(data_plot3$date)] - data_plot3$se[data_plot3$date == min(data_plot3$date)])/data_plot3$se[data_plot3$date == min(data_plot3$date)]*100, 2)


plot4 <- ggplot(data = pct_less_100_comm, aes(x = date, y = n100)) +
  geom_line(colour = "black") +
  ylab("Nodes available for nominations") + xlab("Date") +
  xlim(c(max(date) - lookback, max(date))) + ylim(c(150, 180)) #+
#theme(panel.background = element_rect(fill = 'darkblue', color = 'purple'),
#      panel.grid.major = element_line(color = 'white', linetype = 'dotted', size = 0.2),
#      panel.grid.minor = element_line(color = 'white', size = 0.2))

data_plot4 <- pct_less_100_comm[pct_less_100_comm$date >= (max(pct_less_100_comm$date) - lookback),]
avail_delta <- round((data_plot4$n100[data_plot4$date == max(data_plot4$date)] - data_plot4$n100[data_plot4$date == min(data_plot4$date)]), 2)
avail_start <- round(data_plot4$n100[data_plot4$date == min(data_plot4$date)], 2)
avail_end <- round(data_plot4$n100[data_plot4$date == max(data_plot4$date)], 2)
availPct <- round((data_plot4$n100[data_plot4$date == max(data_plot4$date)] - data_plot4$n100[data_plot4$date == min(data_plot4$date)])/data_plot4$n100[data_plot4$date == min(data_plot4$date)]*100, 2)



grid.arrange(plot1, plot1a, plot2, plot3, plot3a, plot4, nrow = 2)

## Text ----

paste("The minimum active bond increased by ", mab_delta, "DOT (", mabPct, "%), from", mab_start, "DOT to", mab_end, "DOT.")

paste("The number of stakers increased by ", stakers_delta, "accounts (", stakersPct, "%), from", stakers_start, " to", stakers_end, "accounts.")
paste("The number of voters increased by ", voters_delta, "accounts (", votersPct, "%), from", voters_start, " to", voters_end, "accounts.")

paste("The average total stake per node increased by ", stake_delta, "DOT (", stakePct, "%), from", stake_start, "MDOT to", stake_end, "MDOT.")
paste("The variability (standard error) in total stake per node increased by ", stake_delta_se, "DOT (", stakePct_se, "%), from", stake_start_se, "DOT to", stake_end_se, "DOT.")

paste("The number of nodes available for nomination increased by ", avail_delta, " (", availPct, "%), from", avail_start, "nodes to", avail_end, "nodes.")

