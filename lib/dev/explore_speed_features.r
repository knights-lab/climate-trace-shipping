# source("~/Dropbox/research/climate-trace-shipping/lib/load.r")
# map <- load.ship.metadata()

res <- matrix(0,nr=8, nc=4)
colnames(res) <- c('Speed','Speedservice','Speedmax','Sum')
map$Speed[is.na(map$Speed)] <- 0
map$Speedservice[is.na(map$Speedservice)] <- 0
map$Speedmax[is.na(map$Speedmax)] <- 0
res[,1:3] <- c(1,1,1,1,0,0,0,0,1,1,0,0,1,1,0,0,1,0,1,0,1,0,1,0)

res[1,4] <- sum(map$Speed > 0 & map$Speedservice > 0 & map$Speedmax > 0)
res[2,4] <- sum(map$Speed > 0 & map$Speedservice > 0 & map$Speedmax == 0)
res[3,4] <- sum(map$Speed > 0 & map$Speedservice == 0 & map$Speedmax > 0)
res[4,4] <- sum(map$Speed > 0 & map$Speedservice == 0 & map$Speedmax == 0)
res[5,4] <- sum(map$Speed == 0 & map$Speedservice > 0 & map$Speedmax > 0)
res[6,4] <- sum(map$Speed == 0 & map$Speedservice > 0 & map$Speedmax == 0)
res[7,4] <- sum(map$Speed == 0 & map$Speedservice == 0 & map$Speedmax > 0)
res[8,4] <- sum(map$Speed == 0 & map$Speedservice == 0 & map$Speedmax == 0)

# results: Speed always present when others present; conclusion: use Speed
#      Speed Speedservice Speedmax   Sum
# [1,]     1            1        1 26558
# [2,]     1            1        0 57603
# [3,]     1            0        1  6081
# [4,]     1            0        0     0
# [5,]     0            1        1     0
# [6,]     0            1        0     0
# [7,]     0            0        1     0
# [8,]     0            0        0 37890