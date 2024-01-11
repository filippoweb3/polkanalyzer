library(jsonlite)
library(doParallel)

t1 <- proc.time()
t <- system(command = "python3 get_chain_info.py", intern = TRUE)
t2 <- (proc.time() - t1) #stopping recording time
cat(paste(round(t2[3], 3), " seconds elapsed.")) #printing elapsed time





compute_bytes <- function(x){

  lapply(x$extrinsics_hex, function(x) sum(nchar(x)))

}

extrinsic_number <- function(x){

  lapply(x$extrinsics_hex, function(x) length(x))

}




ncores = 12

blocks <- seq(18987848 - (3600*24)/6, 18987848)

groups <- cut(blocks,breaks = ncores, labels = FALSE)

intervals <- list()

for (i in 1:ncores){

  intervals[[i]] <- blocks[groups == i]

}


start_time <- proc.time()
cl <- doMC::registerDoMC(cores = ncores)

t <- foreach(i = 1:ncores) %dopar% {

  out <- system(paste("python3 test_script_1.py", intervals[[i]][1], intervals[[i]][length(intervals[[i]])]), intern = TRUE)

}

end_time <- (proc.time() - start_time) #stopping recording time
cat(paste(round(end_time[3], 3), " seconds elapsed.")) #printing elapsed time

t.sum <- lapply(t,FUN = function(x) fromJSON(x))

#unlist(lapply(t.sum, function(x) compute_bytes(x)))
#unlist(lapply(t.sum, function(x) extrinsic_number(x)))

t.sum <- do.call(rbind,t.sum)

block_limit <- 5240000

block_mb <- block_limit / (1024*1024)

block_avail_mb <- block_limit*0.75 / (1024*1024)

t.sum$usage_mb <- t.sum$extrinsics_size_bytes / (1024*1024) / 2

t.sum$usage_pct <- t.sum$usage_mb / block_avail_mb * 100

p1 <- ggplot(data = t.sum, aes(x = block_number, y = usage_pct)) +
  geom_line(colour = "black") +
  ylab("Block Usage (%)") + xlab("Polkadot Relay Chain Block") + scale_y_continuous(breaks=c(1,3,5,10, 20, 30, 60))

p2 <- ggplot(data = t.sum, aes(x = t.sum$block_number, y = t.sum$extrinsics_count)) +
  geom_line(colour = "black") +
  ylab("Number of Transactions") + xlab("Polkadot Relay Chain Block") + scale_y_continuous(breaks=c(250, 500, 750, 1000, 1250))

grid.arrange(p1,p2, nrow = 1)


block_avail_mb

t.sum[t.sum$usage_mb == max(t.sum$usage_mb),]

t.sum[t.sum$extrinsics_count == max(t.sum$extrinsics_count),]

sum(t.sum$extrinsics_count)

mean(t.sum$extrinsics_count)

mean(t.sum$usage_pct)

(max(t.sum$block_number) - min(t.sum$block_number)) / (3600*24/6)




