##################################
## Single Line Geocode Function ##
##################################
# The function takes:
# - one address at a time as one string (SingleLine)
# - token
# - which geocoding service to use: USA_Comp (USA Composite) or USA_Str (USA StreetAddress) (default is USA_Comp)
# - allow to return Postal codes if a full street address match cannot be found (default is TRUE)
#
# The function returns:
# lon, lat -    The primary x/y coordinates of the address returned by the geocoding service in WGS84 
# score -       The accuracy of the address match between 0 and 100.
# status -      Whether a batch geocode request results in a match (M), tie (T), or unmatch (U)
# matchAddr -   Complete address returned for the geocode request.
# side -        The side of the street where an address resides relative to the direction 
#               of feature digitization
# addressType - The match level for a geocode request. "PointAddress" is typically the 
#               most spatially accurate match level. "StreetAddress" differs from PointAddress 
#               because the house number is interpolated from a range of numbers. "StreetName" is similar,
#               but without the house number.

geocodeSL <- function(address, geocoder = "USA_Comp", postal = TRUE){
  require(httr)
  
  # The following is the link that provides one with the token: http://locator.stanford.edu/arcgis/tokens/
  # The token expires very quickly.
  token <- "JmvbKtygmwkerTQhEwcRlEgT9B9TAfO3ZjTmUzgHAyocr4F_XjWwyaadHNNmjeP1"
  
  if (geocoder == "USA_Str"){
    # Stanford geolocator
    gserver <- "http://locator.stanford.edu/arcgis/rest/services/geocode/USA_StreetAddress/GeocodeServer/geocodeAddresses"
    # template for Single Line format
    pref <- "{'records':[{'attributes':{'OBJECTID':1,'Single Line Input':'"
  }
  else if (geocoder == "USA_Comp") {
    gserver <- "http://locator.stanford.edu/arcgis/rest/services/geocode/USA_Composite/GeocodeServer/geocodeAddresses"
    pref <- "{'records':[{'attributes':{'OBJECTID':1,'SingleLine':'"
  }
  else{
    stop("please provide a valid geocoder")
  }
  
  suff <- "'}}]}"
  
  # url
  url <- URLencode(paste0(gserver, "?addresses=", pref, address, suff, "&token=", token, ifelse(postal, "&f=json", "&f=json&category=Address")))
  
  # submit
  rawdata <- GET(url)
  
  # parse JSON and process result
  res <- content(rawdata, "parsed", "application/json")
  resdf <- with(res$locations[[1]], {data.frame(lon = as.numeric(location$x),
                                                lat = as.numeric(location$y),
                                                score = score, 
                                                #locName = attributes$Loc_name,
                                                status = attributes$Status,
                                                matchAddr = attributes$Match_addr,
                                                side = attributes$Side,
                                                addressType = attributes$Addr_type)})
  return(resdf)
}