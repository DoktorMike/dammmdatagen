

generateCampaigns<-function(n=5, l=30, fromDate=Sys.Date()-364, toDate=Sys.Date()){
  dateseq <- seq(fromDate, toDate, by="1 day")
  numPossibleCampaigns<-as.integer(length(dateseq)/l)
  numPossibleCampaigns

  library(HMM)
  # Initialise HMM
  hmm = initHMM(c("Promotion", "Awareness", "NA"),
                c("Offline", "Online", "Combined", "NA"),
                transProbs=matrix(c(.8,.1,.1,
                                    .1,.8,.1,
                                    .1,.1,.8), 3),
                emissionProbs=matrix(c(.1,.6,.0,
                                       .2,.2,.0,
                                       .7,.2,.0,
                                       .0,.0,1.), 4))
  print(hmm)

  # Simulate
  simHMM(hmm, 30)


  # Sequence of observations
  observations = c("L","L","R","R")
  # Calculate posterior probablities of the states
  posterior = posterior(hmm,observations)
  print(posterior)
}


# Weather
generateWeatherData <- function(fromDate = Sys.Date() - 1 * 365,
                                toDate = Sys.Date(),
                                mynames = c('sunshine', 'precipitation', 'temperature')) {
  tmpdf <- tibble::tibble(date = seq(fromDate, toDate, by = "1 day"))
  arf <- function(x) as.vector(stats::arima.sim(list(order = c(1, 0, 0), ar = 0.7), n = nrow(tmpdf)))
  tmpdf <- data.frame(tmpdf, sapply(mynames, arf)) %>% tibble::as_tibble()
  tmpdf
}

# Macro
# ts.sim <- arima.sim(list(order = c(1,1,0), ar = 0.7), n = 365)
# ts.plot(ts.sim)
generateMacroData <- function(fromDate = Sys.Date() - 1 * 365,
                              toDate = Sys.Date(),
                              mynames = c('cpi', 'cci', 'gdp')) {
  tmpdf <- tibble::tibble(date = seq(fromDate, toDate, by = "1 day"))
  arf <- function(x) as.vector(arima.sim(list(order = c(1, 1, 0), ar = 0.7), n = nrow(tmpdf)-1))
  tmpdf <- data.frame(tmpdf, sapply(mynames, arf)) %>% tibble::as_tibble()
  tmpdf
}

#' Generate competitor media spending as list prices data
#'
#' @param fromDate the beginning of the time series
#' @param toDate the end of the time series
#' @param mynames the names to attach to the generated data
#'
#' @return a tibble with the generated data one column for each element in name
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' generateCompetitorData(Sys.Date()-30, Sys.Date()) %>%
#' gather(competitor, spend, -date) %>%
#' ggplot(aes(y=spend, x=date, fill=competitor)) +
#' geom_bar(stat="identity", position = position_stack())
generateCompetitorData <- function(fromDate = Sys.Date() - 1 * 365,
                              toDate = Sys.Date(),
                              mynames = c('competitor_a', 'competitor_b', 'competitor_c')) {
  tmpdf <- tibble::tibble(date = seq(fromDate, toDate, by = "1 day"))
  arf <- function(x) sample(c(0, seq(10000, 100000, 10000)), nrow(tmpdf), replace = TRUE)
  tmpdf <- data.frame(tmpdf, sapply(mynames, arf)) %>% tibble::as_tibble()
  tmpdf
}

# The mother fucker function
generateData <-
  function(fromDate=Sys.Date()-1*365,
           toDate=Sys.Date(),
           kpi='acquisitions',
           sector='telekom',
           onlineInsertionNames=c('display', 'facebook', 'search_branded'),
           offlineInsertionNames=c('tv', 'radio', 'ooh', 'print'),
           priceNames=c('product_a', 'product_b', 'product_c'),
           distributionNames=c('product_a', 'product_b', 'product_c'),
           weatherNames=c('sunshine', 'precipitation', 'temperature'),
           competitorNames=c('competitor_a', 'competitor_b', 'competitor_c'),
           macroNames=c('cpi', 'cci', 'gdp'),
           eventNames=c('event_a', 'event_b')) {

    datedf <- tibble::tibble(date=seq(fromDate, toDate, by="1 day"))

    ondf <- generateOnlineData(fromDate, toDate, onlineInsertionNames)
    ofdf <- generateOfflineData(fromDate, toDate, offlineInsertionNames)
    prdf <- generatePriceData(fromDate, toDate, priceNames)
    didf <- generateDistributionData(fromDate, toDate, distributionNames)
    wedf <- generateWeatherData(fromDate, toDate, weatherNames)
    codf <- generateCompetitorData(fromDate, toDate, competitorNames)
    madf <- generateMacroData(fromDate, toDate, macroNames)
    evdf <- generateEventData(fromDate, toDate, eventNames)


  }