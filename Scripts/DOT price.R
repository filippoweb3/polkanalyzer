library(quantmod)

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
