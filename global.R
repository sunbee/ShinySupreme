library(dplyr)
library(data.table)

herbicides <- readRDS("data/herbicides.rds")
compounds <- herbicides[, .(TOT_USAGE = sum(EPEST_HIGH_KG, na.rm=TRUE)), by=c("COMPOUND")][,
                QUANT := quantile(TOT_USAGE, probs=0.8, na.rm=TRUE)][, 
                SEL := TOT_USAGE > QUANT][SEL == TRUE, COMPOUND]

allzips <- readRDS("data/superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college * 100
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

cleantable <- allzips %>%
  select(
    City = city.x,
    State = state.x,
    Zipcode = zipcode,
    Rank = rank,
    Score = centile,
    Superzip = superzip,
    Population = adultpop,
    College = college,
    Income = income,
    Lat = latitude,
    Long = longitude
  )
