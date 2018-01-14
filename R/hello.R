

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


#' Generate weather data
#'
#' @param fromDate the beginning of the time series
#' @param toDate the end of the time series
#' @param mynames the names to attach to the generated data
#'
#' @return a tibble with the generated data one column for each element in name
#' @importFrom dplyr "%>%"
#' @importFrom tidyr gather
#' @import ggplot2
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(tidyr)
#' generateWeatherData(Sys.Date()-30, Sys.Date()) %>%
#' gather(type, measure, -date) %>%
#' ggplot(aes(y=measure, x=date, color=type)) +
#' geom_line() + theme_minimal()
generateWeatherData <- function(fromDate = Sys.Date() - 1 * 365,
                                toDate = Sys.Date(),
                                mynames = c('sunshine', 'precipitation', 'temperature')) {
  tmpdf <- tibble::tibble(date = seq(fromDate, toDate, by = "1 day"))
  arf <- function(x) as.vector(stats::arima.sim(list(order = c(1, 0, 0), ar = 0.7), n = nrow(tmpdf)))
  tmpdf <- data.frame(tmpdf, sapply(mynames, arf)) %>% tibble::as_tibble()
  tmpdf
}


#' Generate macro economical data
#'
#' @param fromDate the beginning of the time series
#' @param toDate the end of the time series
#' @param mynames the names to attach to the generated data
#'
#' @return a tibble with the generated data one column for each element in name
#' @importFrom dplyr "%>%"
#' @importFrom tidyr gather
#' @import ggplot2
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(tidyr)
#' generateMacroData(Sys.Date()-30, Sys.Date()) %>%
#' gather(type, measure, -date) %>%
#' ggplot(aes(y=measure, x=date, color=type)) +
#' geom_line() + theme_minimal()
#'
#' ts.plot(arima.sim(list(order = c(1,1,0), ar = 0.7), n = 365))
generateMacroData <- function(fromDate = Sys.Date() - 1 * 365,
                              toDate = Sys.Date(),
                              mynames = c('cpi', 'cci', 'gdp')) {
  tmpdf <- tibble::tibble(date = seq(fromDate, toDate, by = "1 day"))
  arf <- function(x) as.vector(stats::arima.sim(list(order = c(1, 1, 0), ar = 0.7), n = nrow(tmpdf)-1))
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
#' @importFrom dplyr "%>%"
#' @importFrom tidyr gather
#' @import ggplot2
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(tidyr)
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

#' Generate a marketing mix modeling data set
#'
#' This function generates a marketing mix modeling data set based on the
#' parameters given. It uses many different stochastical processes to accomplish
#' this and the dynamics behind them are not available to the user to manipulate.
#'
#'
#' @param fromDate the beginning of the time series
#' @param toDate the end of the time series
#' @param kpi the name of the kpi (response) to simulate
#' @param sector the name of the sector to simulate
#' @param onlineInsertionNames the names of each online media insertion you wish to use
#' @param offlineInsertionNames  the names of each offline media insertion you wish to use
#' @param priceNames the names of the different product prices
#' @param distributionNames the names of the different product distributions
#' @param weatherNames the names of the weather data measurements to use
#' @param competitorNames the names of each competitor considered
#' @param macroNames the names of the macroeconomical factors to simulate
#' @param eventNames the names of events to add
#'
#' @return a list of tibble containing each data mentioned
#' @export
#'
#' @examples
#' a<-1
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

    mydf <- tibble::tibble(date=seq(fromDate, toDate, by="1 day"))

    # ondf <- generateOnlineData(fromDate, toDate, onlineInsertionNames)
    # ofdf <- generateOfflineData(fromDate, toDate, offlineInsertionNames)
    # prdf <- generatePriceData(fromDate, toDate, priceNames)
    # didf <- generateDistributionData(fromDate, toDate, distributionNames)
    wedf <- generateWeatherData(fromDate, toDate, weatherNames)
    codf <- generateCompetitorData(fromDate, toDate, competitorNames)
    madf <- generateMacroData(fromDate, toDate, macroNames)
    # evdf <- generateEventData(fromDate, toDate, eventNames)

    mydf <- Reduce(function(x, y) dplyr::inner_join(x,y, by = "date"), list(mydf, wedf, codf, madf))
    mydf
  }
