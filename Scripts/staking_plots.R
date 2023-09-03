library(dplyr)
library(ggplot2)
library(gridExtra)

## Nodes with < 100% comm. ----

date <- unique(Sys.Date() + (eras_data$eras$era - max(eras_data$eras$era)))

pct_less_100_comm <- group_by(eras_data$eras, era) %>% summarise(pct = sum(commission_percent < 100)/length(commission_percent)*100)

pct_less_100_comm <- data.frame(date, era = pct_less_100_comm[,1], pct = pct_less_100_comm[,2])


## Node Total Stake ----

tot_stake <- group_by(eras_data$eras, era) %>%
  summarize(m = mean(total_stake, na.rm = T), se = sd(total_stake, na.rm = T)/sqrt(length(total_stake)))

tot_stake <- data.frame(date = date, tot_stake)


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

  if(sum(sub$active_bond < 120) >= 1){

    sub <- sub[sub$active_bond < 120,]

  } else {

    sub <- sub[sub$active_bond == min(sub$active_bond),]

  }

  mab[i] <- median(sub$bond, na.rm = T)

}

dates <- Sys.Date() + (eras - max(eras))

mab_data <- data.frame(eras, date = dates, mab)


## All Voters and Stakers ----

all_voters <- group_by(voters, era) %>% summarize(n_voters = length(address))

all_stakers <- group_by(stakers, era) %>% summarize(n_stakers = length(address))

all_stakers_voters <- merge(all_voters, all_stakers, by = "era")

all_stakers_voters$date <- Sys.Date() + (all_stakers_voters$era - max(all_stakers_voters$era))


## Plots ----

plot1 <- ggplot(data = pct_less_100_comm, aes(x = date, y = pct)) +
  geom_line() +
  ylab("Nodes < 100% commission") + xlab("Date") +
  xlim(c(max(date) - 7, max(date))) + ylim(c(50, 60))

plot2 <- ggplot(data = tot_stake[tot_stake$era > 400,], aes(x = date, y = m/10^16)) +
  geom_line() +
  geom_ribbon(aes(ymin = (m - se)/10^16,
                  ymax = (m + se)/10^16), alpha = 0.5) +
  ylab("Total Stake (MDOT)") + xlab("Date") +
  xlim(c(max(date) - 7, max(date)))  + ylim(c(2, 2.4))

plot3 <- ggplot(data = mab_data, aes(x = date, y = mab)) +
  geom_line() +
  ylab("Minimum Active Bond (DOT)") + xlab("Date") +
  xlim(c(max(date) - 7, max(date))) + ylim(c(400, 500))

plot4 <- ggplot(data = all_stakers_voters, aes(x = date)) +
         geom_line(aes(y = n_voters)) +
         geom_line(aes(y = n_stakers), color="steelblue", linetype="twodash") +
  ylab("Number of Accounts") + xlab("Date") + geom_hline(yintercept = 22500, color="grey40", linetype="dotted") +
  xlim(c(max(date) - 7, max(date)))

grid.arrange(plot1, plot2, plot3, plot4, nrow = 2)
