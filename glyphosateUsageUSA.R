# """
# County-level data:
# https://water.usgs.gov/nawqa/pnsp/usage/maps/county-level/
# 
# This uses Federal Information Process Standard (FIPS) three-digit code
# for place (~ county).
# See: https://www.census.gov/geographies/reference-files/time-series/geo/name-lookup-tables.2010.html
#
# > library(tigris)
# > data("fips_codes")
# > head(fips_codes)
# 
# See also:
# > lookup_code("Colorado", "Boulder")
#
# YouTube series on leaflet from Abhinav Agarwal
# Here: https://www.youtube.com/watch?v=YlPr3ca1iWw&list=PL6wLL_RojB5y8uL3uuIMnJ6JoTIFywQ-r&index=6
#
# """


# 1. Acquire data from USGS into data.table

# 2. Cross-reference with FIPS reference-data from Tigris
#    Get state/county names, looking up FIPS codes state/county
#    Use the names to make address lines

# 3. Get lat-long pairs from state/county names
#    Geocode using one of the options:
#    1. Use google APIs via ggmap (requires API key, requires credit card)
#    2. Use mapquest APIs and httr (requires API key, no credit card needed)
#    3. Use Data Science Toolkit's RDSK implememtation
#    4. Use Geonames service via eponymous R package 
#    5. Use Here APIs and httr (requires AppID and AppCode, freemium model)
#    Merge tables on address.

# 4. Save glyphosate extract


library(httr)
library(data.table)
library(tigris)
library(geonames)
library(jsonlite)

# 1. Acquire data from USGS into data.table

url_usgs <- "https://water.usgs.gov/nawqa/pnsp/usage/maps/county-level/PesticideUseEstimates/EPest.county.estimates.2012.txt"
tryCatch(
  GET(url_usgs, write_disk("./usgs.txt")),
  error = function(e) {
    print("It looks like you have downloaded the data already.")
  })

if (file.exists("usgs.txt")) {
  DT_usgs <- data.table(read.csv("usgs.txt", sep="\t", 
                                 colClasses=c("factor", 
                                              "integer", 
                                              "character",
                                              "character",
                                              "numeric",
                                              "numeric")))
  
}
head(DT_usgs)

# 2. Execute Pareto analysis to short-list the 20% of compounds that account
#    for 80% of usage.
DT_pareto <- DT_usgs[, .(TOT_USAGE = sum(EPEST_HIGH_KG)), by=c("COMPOUND")][, 
              QUANT := quantile(TOT_USAGE, probs=0.8, na.rm=TRUE)][, 
              SEL := TOT_USAGE > QUANT][SEL == TRUE, COMPOUND]

# 3. Cross-reference with FIPS reference-data from Tigris
#    Get state/county names, looking up FIPS codes state/county
#    Use the names to make address lines

data("fips_codes")
DT_codes <- data.table(fips_codes)
head(DT_codes)

setkeyv(DT_usgs, c("STATE_FIPS_CODE", "COUNTY_FIPS_CODE"))
setkeyv(DT_codes, c("state_code", "county_code"))
DT <- DT_codes[DT_usgs]
DT[, Place := paste(county, state, sep=", ")]
head(DT)   # DT will be merged with geo-coded locations later.

# 3. Get lat-long pairs from state/county names
#    Geocode using one of the options
#    Merge tables on address

# This step involves invoking REST API on freemium plan.
# Hence, data are persisted for use.

if (!file.exists("map.csv")) {
  # Create a database to persist geo-coding data if one doesn't exist. 
  Places <- DT[, unique(Place)]
  Places2Search <- gsub(" ", "+", Places)
  DT_coords <- data.table(Places=Places, SearchTerm=Places2Search)
  DT_coords$Latitude <- NA
  DT_coords$Longitude <- NA
  write.csv(DT_coords, "map.csv", row.names=FALSE)
} else {
  # Use database to access/continue geo-coding data, if persisted.
  DT_coords <- read.csv("map.csv")
}
head(DT_coords)

num <- dim(DT_coords)[1]   # Test: # num <-  17
skipped <- 0;
for (i in 1:num) {
  if (i %% 50 == 0) {
    print(paste("Completed:", i))
  }
  thisRow <- DT_coords[i, ]
  searchTerm <- thisRow$SearchTerm
  latitude <- thisRow$Latitude
  longitude <- thisRow$Longitude
  if (!is.na(latitude) & !is.na(longitude)) {
    next
  }
  tryCatch(
    DT_coords[i, c("Latitude", "Longitude")] <- here_now(searchTerm),
    error = function(e) {
      print(paste("Skipped:", searchTerm, "with", e))
      skipped <<- skipped + 1
      print(paste("Skipped", skipped, "so far."))
    }
  )
  if (skipped > 77) {
    break
  }

}
head(DT_coords, 15)
write.csv(DT_coords, "map.csv", row.names=FALSE)
DT_coords <- data.table(DT_coords)

setkeyv(DT, "Place")
setkeyv(DT_coords, "Places")
DT <- DT_coords[DT]

# 4. Save to disk after clean-up
drop.columns <- c("SearchTerm", "state", "state_code", "county_code")
DT[, (drop.columns) := NULL]
setnames(DT, old=c("state_name", "county"), new=c("State", "County"))
setnames(DT, old=c("Places"), new=c("Location"))
write.csv(DT, "herbicides.csv", row.names=FALSE)
saveRDS(DT, "herbicides.rds")

# LIBRARY:
here_now <- function(searchtext) {
# Geocode using the here.com API with registered account.
#   Geocoding refers to finding lat-long coordinates from an address
#   that is full or partially specified.
#   
#   searchText: (string) must be URL-friendly string (e.g. "Boulder+County,+CO").
#     For the web-service used, the address must be a US address.
#     For the use-case served, expected address has the name of a 
#     county and state.
#
#   Returns: lat-long pair
#
#   Sample usage:
#   > here_now("Boulder+County,+CO") # $lat: 40.08791; $lon: -105.3447
    
    AppCode <- getOption("hereAppCode")
    AppID <- getOption("hereAppID")
    
    rootURL <- "https://geocoder.api.here.com/6.2/geocode.json?"
    app_id = paste("app_id", AppID, sep="=")
    app_code = paste("app_code", AppCode, sep="=")
    searchtext = paste("searchtext", searchtext, sep="=")
    
    request <- paste(paste(rootURL, app_id, sep=''), app_code, searchtext, sep="&")
    response = fromJSON(request)

    res <- list()
    res$lat <- response$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Latitude
    res$lon <- response$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Longitude
  
    res
}


#   Use Here APIs and jsonlite (requires AppID and AppCode, freemium model)

#     AppCode <- getOption("hereAppCode")
#     AppID <- getOption("hereAppID")
#     
#     rootURL <- "https://geocoder.api.here.com/6.2/geocode.json?"
#     searchtext="425+W+Randolph+Chicago"
#     searchtext = "Autauga+County,+AL"   # 01001 @lat:32.50771, lon:-86.65101
#     searchtext = "Arapahoe+County,+CO"  # 08005 @lat:39.6521, lon:-104.2832
#     searchtext = "Boulder+County,+CO"   # 08013 @lat:40.08791, lon:-105.3447
#     searchtext = Places2Search[999]     # 21083 @lat:36.72397, lon: -88.65218
#     
#     app_id = paste("app_id", AppID, sep="=")
#     app_code = paste("app_code", AppCode, sep="=")
#     searchtext = paste("searchtext", searchtext, sep="=")
#     
#     request <- paste(paste(rootURL, app_id, sep=''), app_code, searchtext, sep="&")
#     response = fromJSON(request)
#     
#     lat <- response$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Latitude
#     lon <- response$Response$View$Result[[1]]$Location$NavigationPosition[[1]]$Longitude
# 
# #   Use Geonames service via eponymous R package 
#     options(geonamesUsername=getOption("geonamesUsername"))
#     GNsearch(name=Places[1], coutry="USA")
#     
    



# Other references:
# 1. Traveling Salesman Redux: https://github.com/datawookie/travelling-salesman-map/blob/master/travelling-salesman-demo.R
# 2. Here now: https://developer.here.com/
# 3. Best practices for APIs in R: https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html
# 4. FCC API: https://geo.fcc.gov/api/census/
# 5: Absolute panels: https://shiny.rstudio.com/gallery/absolutely-positioned-panels.html

