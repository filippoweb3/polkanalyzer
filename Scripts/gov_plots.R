library(dplyr)
library(ggplot2)
library(gridExtra)

mydata <- data.frame()

for (i in 1:22){

  id <- seq(20, 41)

  mydata <- rbind(mydata, read.csv(paste0("/Users/filippo/Downloads/referenda2_vote-213 (",id[i],").csv")))

}

mydata$Time <- format(as.POSIXct(mydata$Time,
                  format = '%Y-%m-%d %H:%M:%S'),
       format = '%Y-%m-%d')

mydata$Time <- as.Date(mydata$Time)


mydata <- mydata[mydata$Voted %in% c("Nay","Aye","Abstain"),]

mydata$singleVote <- rep(1)


group_by(mydata, Voted) %>% summarise(n_acc = length(Account), n_votes = sum(Value), n_eff_votes = sum(Effective.Votes), n_deleg = sum(!Delegate.To == "-"))

table(mydata$Conviction)



p0 <- ggplot(mydata, aes(x=singleVote, fill=Voted)) +
  geom_histogram(position="dodge") +
  scale_x_continuous(trans='log10') +
  theme(legend.position="top") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = "Number of Accounts", x = "", title = "Polkadot OpenGov Referendum 213")
p1 <- ggplot(mydata, aes(x=Value, fill=Voted)) +
  geom_histogram(position="dodge") +
  scale_x_continuous(trans='log10') +
  theme(legend.position="none") +
  labs(y = "Count", x = "Votes")
p2 <- ggplot(mydata, aes(x=Effective.Votes, fill=Voted)) +
  geom_histogram(position="dodge") +
  scale_x_continuous(trans='log10') +
  theme(legend.position="none") +
  labs(y = "Count", x = "Votes (Coinviction Adjusted)")
p3 <- ggplot(mydata, aes(x=Conviction, fill=Voted)) +
  geom_histogram(position="dodge") +
  theme(legend.position="none") +
  labs(y = "Count", x = "Conviction Used")
p4 <- ggplot(mydata, aes(x=Time, fill=Voted)) +
  geom_histogram(position="dodge") +
  theme(legend.position="none") +
  labs(y = "Count", x = "Time")

grid.arrange(p0,p1,p2,p3,p4, nrow=5)


