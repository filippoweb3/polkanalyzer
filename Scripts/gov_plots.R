library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)


refn <- 219
N <- 14165

mydata <- data.frame()

for (i in 1:15){

  id <- seq(0, 14)

  mydata <- rbind(mydata, read.csv(paste0("/Users/filippo/Downloads/referenda2_vote-219 (",id[i],").csv")))

}

# Data Prep ----

mydata$Time <- format(as.POSIXct(mydata$Time,
                  format = '%Y-%m-%d %H:%M:%S'),
       format = '%Y-%m-%d')

mydata$Time <- as.Date(mydata$Time)

mydata <- mydata[mydata$Voted %in% c("Nay","Aye","Abstain"),]

mydata$singleVote <- rep(1)

mydata$delegation <- ifelse(!mydata$Delegate.To == "-", "Delegated", "Solo")

# Summary Stats ----

sum_tab <- as.data.frame(group_by(mydata, delegation, Voted) %>% summarise(n_acc = length(Account), n_votes = sum(Value), n_eff_votes = sum(Effective.Votes), n_deleg = sum(!Delegate.To == "-")))

sum_tab$n_votes <- round(sum_tab$n_votes/10^6)
sum_tab$n_eff_votes <- round(sum_tab$n_eff_votes/10^6)

#table(mydata$Conviction)

# Plots ----

p0 <- ggplot(sum_tab, aes(y=n_acc, x=delegation, fill = Voted)) +
  geom_bar(stat = "identity") +
  labs(y = "Accounts", x = "") +
  scale_fill_manual(values=c("Abstain" = "#89CFF0", "Aye" = "#50C878", "Nay" = "red")) +
  geom_text(aes(label=replace(n_acc, n_acc == 0, "")), size=3.5, position = position_stack(vjust = 0.5))

p1 <- ggplot(sum_tab, aes(y=n_eff_votes, x = delegation, fill = Voted)) +
  geom_bar(stat = "identity") +
  theme(legend.position="none") +
  labs(y = "Votes (Conviction)", x = "") +
  scale_fill_manual(values=c("#89CFF0","#50C878","red")) +
  scale_y_continuous(breaks = c(1000000, 10000000, max(sum_tab$n_eff_votes)),
                     labels = c("1M","10M",paste0(round(max(sum_tab$n_eff_votes)),"M"))) +
  geom_text(aes(label=replace(n_eff_votes, n_eff_votes == 0, "")), size=3.5, position = position_stack(vjust = 0.5))

p2 <- ggplot(mydata, aes(x=Value, fill=Voted)) +
  geom_histogram(position="dodge") +
  theme(legend.position="none") +
  scale_x_continuous(trans='log10', breaks = c(0.1, 100, 1000, 10000, 100000, 1000000, max(mydata$Value)),
                     labels = c("0.1","100","1K","10K","100K","1M",paste0(round(max(mydata$Value)/10^6),"M"))) +
  labs(y = "Count", x = "Votes") +
  scale_fill_manual(values=c("#89CFF0","#50C878","red"))

p3 <- ggplot(mydata, aes(x=Effective.Votes, fill=Voted)) +
  geom_histogram(position="dodge") +
  scale_x_continuous(trans='log10',
                     breaks = c(0.1, 100, 1000, 10000, 100000, 1000000, max(mydata$Effective.Votes)),
                     labels = c("0.1","100","1K","10K","100K","1M", paste0(round(max(mydata$Effective.Votes)/10^6),"M"))) +
  theme(legend.position="none") +
  labs(y = "Count", x = "Votes (Conviction)") +
  scale_fill_manual(values=c("#89CFF0","#50C878","red"))

p4 <- ggplot(mydata, aes(x=Conviction, fill=Voted)) +
  geom_histogram(position="dodge") +
  theme(legend.position="none") +
  labs(y = "Accounts", x = "Conviction Used") +
  scale_fill_manual(values=c("#89CFF0","#50C878","red")) +
  scale_x_continuous(breaks = c(0.1, 1, 2, 3, 4, 5, 6),
                     labels = c("0.1x","1x","2x","3x","4x","5x","6x"))

p5 <- ggplot(mydata, aes(x=Time, fill=Voted)) +
  geom_histogram(position="dodge") +
  theme(legend.position="none") +
  labs(y = "Accounts", x = "Time") +
  scale_fill_manual(values=c("#89CFF0","#50C878","red"))

grid.arrange(p0, p1, p2, p3, p4, p5,
             top = paste("Polkadot OpenGov Referendum:",refn,"| Requested:",N,"DOT | ", Sys.Date()),
             layout_matrix = matrix(c(1,2,3,3,4,4,5,6), ncol=2, byrow=TRUE))
