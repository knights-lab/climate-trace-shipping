ships.2018 <- x$IMO.Number[x$reporting.period == 2018]
ships.2019 <- x$IMO.Number[x$reporting.period == 2019]
ships.2020 <- x$IMO.Number[x$reporting.period == 2020]

# a matrix of T/F whether a ship has data in a given year
ships.by.year <- matrix(FALSE,nrow=length(unique(x$IMO.Number)),ncol=3)
rownames(ships.by.year) <- sort(unique(x$IMO.Number))
colnames(ships.by.year) <- c('2018','2019','2020')
ships.by.year[,'2018'] <- rownames(ships.by.year) %in% ships.2018
ships.by.year[,'2019'] <- rownames(ships.by.year) %in% ships.2019
ships.by.year[,'2020'] <- rownames(ships.by.year) %in% ships.2020

is.ship.with.prior.year <- ships.by.year[,'2020'] & (ships.by.year[,'2019'] | ships.by.year[,'2018'])
ships.with.prior.year <- rownames(ships.by.year)[is.ship.with.prior.year]
y2020 <- x[x$reporting.period == 2020 & (x$IMO.Number %in% ships.with.prior.year),'kg.CO2.per.nm']
names(y2020) <- x$IMO.Number[x$reporting.period == 2020 & (x$IMO.Number %in% ships.with.prior.year)]
yhat <- y2020
for(i in 1:length(y2020)){
    ship.id <- names(y2020)[i]
    if((ship.id %in% ships.2018) && (ship.id %in% ships.2019)){
      yhat[i] <- mean(x[x$IMO.Number == ship.id & x$reporting.period != 2020,'kg.CO2.per.nm'])
    } else if(ship.id %in% ships.2018) {
      yhat[i] <- x[x$IMO.Number == ship.id & x$reporting.period == 2018,'kg.CO2.per.nm']
    } else if(ship.id %in% ships.2019) {
      yhat[i] <- x[x$IMO.Number == ship.id & x$reporting.period == 2019,'kg.CO2.per.nm']
    }
}