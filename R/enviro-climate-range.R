#' get the Environment, Climate, and Range of a species.  
#' @param fish.data the fishbase database fish.data or a subset,
#' @param path to cached copy of fishbase (optional, defaults to copy in package).
#' @param return.range (logical) returns the range bounds for the supplied records in fish.data (defaults to False)
#' @return a list of the values in the Enviroment / Climate / Range data. See https://github.com/ropensci/rfishbase/issues/13. Alternately, if return.range=TRUE, a data.frame of min and max lat and lons.
#' @export
#' @examples \dontrun{
#' library(rfishbase)
#' data(fishbase)
#' out <- getEnviroClimateRange(fish.data[1:3])
#' cat(out[[1]]) # cat for pretty printing
#'
#' ranges <- getEnviroClimateRange(fish.data[1:3], return.range = TRUE)
#' ranges
#' } 
getEnviroClimateRange <- function(fish.data=NULL, path=NULL, return.range=FALSE){
  ids <- getIds(fish.data = fish.data, path=path)
  ECR.pages <- lapply(ids, function(id){
    summaryPage <- getSummary(id)
    xpathSApply(summaryPage, "//h1[ contains(., 'Environment / Climate / Range')]/following::node()[2]/span", xmlValue)
  })
  out <- ECR.pages
  
  if(return.range == TRUE) {
    range <- sapply(ECR.pages, FUN = function(x) {
      lat <- data.frame(str_match_all(x, "([0-9]{1,2})°([NS])"), stringsAsFactors=F)
      if(!any(is.na(lat[1, ])) && (nrow(lat)==2 || nrow(lat)==1)) {
        lat[,2] <- as.numeric(lat[,2])
        lat[,2] <- ifelse(lat[,3] == "S", -lat[,2], lat[,2])
        if(nrow(lat)!=2) {
          lat[2,] <- rep(NA, 3)
        }
        lat <- lat[rev(rownames(lat)),]
      }else{
        lat <- matrix(rep(NA, 6), nrow=2, ncol=3)
      }
      lon <- data.frame(str_match_all(x, "([0-9]{1,3})°([EW])"),  stringsAsFactors=F)
      if(!any(is.na(lon[1, ])) && (nrow(lon)==2 || nrow(lon)==1)) {
        lon[,2] <- as.numeric(lon[,2])
        lon[,2] <- ifelse(lon[,3] == "W", -lon[,2], lon[,2])
        if(nrow(lon)!=2) {
          lon[2,] <- rep(NA, 3)
        }
      }else{
        lon <- matrix(rep(NA, 6), nrow=2, ncol=3)
      }
      out <- c(lat[1,2], lat[2,2], lon[1,2], lon[2,2])
    })

    range <- data.frame(t(range))
    names(range) <- c("lat_min", "lat_max", "lon_min", "lon_max")
    out <- range
  }
  return(out)
}
