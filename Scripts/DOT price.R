library(quantmod)
library(dplyr)

getSymbols(paste0(from = "USD", to = "CHF", "=X"), from = "2021-12-27", to = "2023-01-31")

getSymbols("DOT-USD", from = "2022-01-01", to = "2023-01-31")

DOT_USD <- as.data.frame(`DOT-USD`)
USD_CHF <- as.data.frame(`USDCHF=X`)

DOT_USD <- data.frame(date = as.Date(rownames(DOT_USD)), DOT_USD)
USD_CHF <- data.frame(date = as.Date(rownames(USD_CHF)), USD_CHF)

days <- length(DOT_USD[,1])

for(i in 1:days){

  if(sum(USD_CHF$date %in% DOT_USD$date[i]) == 1){

    DOT_USD$USDCHF[i] <- USD_CHF[USD_CHF$date %in% DOT_USD$date[i],]$USDCHF.X.Adjusted

  } else {

    past <- DOT_USD$date[i] - 5

    sub <- USD_CHF[USD_CHF$date >= past & USD_CHF$date < DOT_USD$date[i],]

    DOT_USD$USDCHF[i] <- sub[sub$date == max(sub$date),]$USDCHF.X.Adjusted

  }

}

DOT_USD$DOT.CHF <- DOT_USD$DOT.USD.Adjusted*DOT_USD$USDCHF
DOT_USD$date <- format(DOT_USD$date, format = "%d.%m.%y")

#r22 <- read.table("Rewards-2022.csv", sep = ",", header = T)
r22$Date <- strptime(r22$Date, "%d.%m.%y %H:%M")
r22$Date <- format(r22$Date, format = "%d.%m.%y")
colnames(r22)[2] <- "date"

mydata <- merge(x = DOT_USD, y = r22, by = "date")
mydata <- mydata[order(mydata$Block),]
mydata$date <- as.Date(mydata$date, format = "%d.%m.%y")
mydata$valueCHF <- mydata$DOT.CHF*mydata$Value


full.sum <- mydata %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(year, month) %>%
  summarise(m.vol = mean(DOT.USD.Volume/10^6),
            m.priceUSD =  mean(DOT.USD.Adjusted),
            m.USDCHF = mean(USDCHF),
            m.priceCHF =  mean(DOT.CHF),
            rewards = sum(Value),
            CHF = sum(valueCHF))
