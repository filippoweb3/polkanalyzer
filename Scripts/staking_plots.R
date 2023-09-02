library(dplyr)
library(ggplot2)

## Nodes with < 100% comm.

date <- unique(Sys.Date() + (eras_data$eras$era - max(eras_data$eras$era)))

pct_less_100_comm <- group_by(eras_data$eras, era) %>% summarise(pct = sum(commission_percent < 100)/length(commission_percent)*100)

pct_less_100_comm <- data.frame(date, era = pct_less_100_comm[,1], pct = pct_less_100_comm[,2])

ggplot(data = pct_less_100_comm, aes(x = date, y = pct)) +
  geom_line() +
  ylab("Nodes < 100% commission") + xlab("Date")

## Node Total Stake

tot_stake <- group_by(eras_data$eras, era) %>%
  summarize(m = mean(total_stake, na.rm = T), se = sd(total_stake, na.rm = T)/sqrt(length(total_stake)))

tot_stake <- data.frame(date = date, tot_stake)

ggplot(data = tot_stake[tot_stake$era > 400,], aes(x = date, y = m/10^16)) +
  geom_line() +
  geom_ribbon(aes(ymin = (m - se)/10^16,
                  ymax = (m + se)/10^16), alpha = 0.5) +
  ylab("Total Stake (MDOT)") + xlab("Date")

