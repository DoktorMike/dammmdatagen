

generateCampaigns <- function(n = 5, l = 30, fromDate = Sys.Date() - 364, toDate = Sys.Date()) {
  dateseq <- seq(fromDate, toDate, by = "1 day")
  numPossibleCampaigns <- as.integer(length(dateseq) / l)
  numPossibleCampaigns

  # library(HMM)
  # Initialise HMM
  # hmm = initHMM(c("Promotion", "Awareness", "NA"),
  #               c("Offline", "Online", "Combined", "NA"),
  #               transProbs=matrix(c(.8,.1,.1,
  #                                   .1,.8,.1,
  #                                   .1,.1,.8), 3),
  #               emissionProbs=matrix(c(.1,.6,.0,
  #                                      .2,.2,.0,
  #                                      .7,.2,.0,
  #                                      .0,.0,1.), 4))
  # print(hmm)
  #
  # # Simulate
  # simHMM(hmm, 30)
  #
  #
  # # Sequence of observations
  # observations = c("L","L","R","R")
  # # Calculate posterior probablities of the states
  # posterior = posterior(hmm,observations)
  # print(posterior)
}

generateFromFunction <- function(f, fromDate = Sys.Date() - 1 * 365,
                                 toDate = Sys.Date(),
                                 mynames = c("something", "something_else")) {
  tmpdf <- tibble::tibble(date = seq(fromDate, toDate, by = "1 day"))
  tmpdf <- data.frame(tmpdf, sapply(mynames, f)) %>% tibble::as_tibble()
  tmpdf
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
#' generateWeatherData(Sys.Date() - 30, Sys.Date()) %>%
#'   gather(type, measure, -date) %>%
#'   ggplot(aes(y = measure, x = date, color = type)) +
#'   geom_line() +
#'   theme_minimal()
generateWeatherData <- function(fromDate = Sys.Date() - 1 * 365,
                                toDate = Sys.Date(),
                                mynames = c("sunshine", "precipitation", "temperature")) {
  arf <- function(x) {
    as.vector(stats::arima.sim(list(order = c(1, 0, 0), ar = 0.7), n = as.integer(toDate - fromDate) + 1))
  }
  generateFromFunction(arf, fromDate = fromDate, toDate = toDate, mynames = mynames)
}

#' Generate price data
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
#' generatePriceData(Sys.Date() - 30, Sys.Date()) %>%
#'   gather(type, price, -date) %>%
#'   ggplot(aes(y = price, x = date, color = type)) +
#'   geom_line() +
#'   theme_minimal()
generatePriceData <- function(fromDate = Sys.Date() - 1 * 365,
                              toDate = Sys.Date(),
                              mynames = c("product_a", "product_b", "product_c")) {
  arf <- function(x) {
    # Initialise HMM
    hmm <- HMM::initHMM(
      c("PriceWar", "Normal"),
      c("PriceA", "PriceB", "PriceC", "PriceD"),
      transProbs = matrix(c(
        .8, .2,
        .2, .8
      ), 2),
      emissionProbs = matrix(c(
        .3, .6,
        .2, .2,
        .3, .1,
        .2, .1
      ), 4)
    )
    tmptypedf <-
      tibble::tibble(type = HMM::simHMM(hmm, as.integer(toDate - fromDate) +
        1)$observation)
    tmppricedf <-
      tibble::tibble(
        type = c("PriceA", "PriceB", "PriceC", "PriceD"),
        price = c(199, 149, 129, 99)
      )
    tmpdf <- dplyr::left_join(tmptypedf, tmppricedf, by = "type")
    as.vector(tmpdf$price)
  }
  generateFromFunction(arf,
    fromDate = fromDate,
    toDate = toDate,
    mynames = mynames
  )
}

#' Generate distribution data as absolute levels
#'
#' @param fromDate the beginning of the time series
#' @param toDate the end of the time series
#' @param mynames the names to attach to the generated data
#' @param initDist the vector of initial distributions in total numbers
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
#' generateDistributionData(
#'   Sys.Date() - 30, Sys.Date(),
#'   c("product_a", "product_b", "product_c"), c(20, 50, 10)
#' ) %>%
#'   gather(type, distribution, -date) %>%
#'   ggplot(aes(y = distribution, x = date, color = type)) +
#'   geom_line() +
#'   theme_minimal()
generateDistributionData <- function(fromDate = Sys.Date() - 1 * 365,
                                     toDate = Sys.Date(),
                                     mynames = c("product_a", "product_b", "product_c"),
                                     initDist = c(1000, 500, 800)) {
  arf <- function(x) (rpois(as.integer(toDate - fromDate) + 1, 2) / 10 + 1) / 2
  arf <- function(x) {
    # Initialise HMM
    hmm <- HMM::initHMM(
      c("Normal", "Turbulent"),
      c("Increase", "Decrease", "Stay"),
      transProbs = matrix(c(
        .95, .95,
        .05, .05
      ), 2),
      emissionProbs = matrix(c(
        .05, .4,
        .05, .2,
        .90, .4
      ), 3)
    )
    tmptypedf <-
      tibble::tibble(type = HMM::simHMM(hmm, as.integer(toDate - fromDate) +
        1)$observation)
    tmppricedf <-
      tibble::tibble(
        type = c("Increase", "Decrease", "Stay"),
        price = c(1, -1, 0)
      )
    tmpdf <- dplyr::left_join(tmptypedf, tmppricedf, by = "type")
    cumsum(as.vector(tmpdf$price))
  }
  # dautility::qplotez((rpois(10, 2)/10+1)/2)
  a <- generateFromFunction(arf,
    fromDate = fromDate,
    toDate = toDate,
    mynames = mynames
  )
  a[, -1] <- t(t(a[, -1]) + initDist)
  a
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
#' generateMacroData(Sys.Date() - 30, Sys.Date()) %>%
#'   gather(type, measure, -date) %>%
#'   ggplot(aes(y = measure, x = date, color = type)) +
#'   geom_line() +
#'   theme_minimal()
#'
#' ts.plot(arima.sim(list(order = c(1, 1, 0), ar = 0.7), n = 365))
generateMacroData <- function(fromDate = Sys.Date() - 1 * 365,
                              toDate = Sys.Date(),
                              mynames = c("cpi", "cci", "gdp")) {
  arf <- function(x) as.vector(stats::arima.sim(list(order = c(1, 1, 0), ar = 0.7), n = as.integer(toDate - fromDate)))
  generateFromFunction(arf, fromDate = fromDate, toDate = toDate, mynames = mynames)
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
#' generateCompetitorData(Sys.Date() - 30, Sys.Date()) %>%
#'   gather(competitor, spend, -date) %>%
#'   ggplot(aes(y = spend, x = date, fill = competitor)) +
#'   geom_bar(stat = "identity", position = position_stack())
generateCompetitorData <- function(fromDate = Sys.Date() - 1 * 365,
                                   toDate = Sys.Date(),
                                   mynames = c("competitor_a", "competitor_b", "competitor_c")) {
  arf <- function(x) sample(c(0, seq(10000, 100000, 10000)), as.integer(toDate - fromDate) + 1, replace = TRUE)
  generateFromFunction(arf, fromDate = fromDate, toDate = toDate, mynames = mynames)
}

#' Generate online media data
#'
#' @param fromDate the beginning of the time series
#' @param toDate the end of the time series
#' @param mynames the names to attach to the generated data
#' @param avgcpm the average Cost Per 1000 Impressions
#' @param avgnet the average net investment per insertion and day
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
#' mydflist <- generateOnlineData(Sys.Date() - 30, Sys.Date())
#' mydflist[["impression"]] %>%
#'   gather(type, impression, -date) %>%
#'   ggplot(aes(y = impression, x = date, color = type)) +
#'   geom_line() +
#'   theme_minimal()
generateOnlineData <- function(fromDate = Sys.Date() - 1 * 365,
                               toDate = Sys.Date(),
                               mynames = c("display", "facebook", "search_branded"),
                               avgcpm = 0.5, avgnet = 10000) {
  genCampaignStructure <- function(n) {
    hmm <- HMM::initHMM(
      c("Burst", "Normal", "Off"),
      c("PriceA", "PriceB"),
      transProbs = matrix(c(
        .7, .05, .1,
        .1, .9, .1,
        .2, .05, .8
      ), 3),
      emissionProbs = matrix(c(
        .3, .7, .5,
        .7, .3, .5
      ), 2)
    )
    HMM::simHMM(hmm, n)$states
  }

  arf <- function(x) {
    cpms <- c(seq(0.1 * avgcpm, 2.5 * avgcpm, length.out = 11))
    pricenames <- paste0("Price", LETTERS[1:length(cpms)])
    tmppricedf <- tibble::tibble(cpm = cpms, type = pricenames)
    # Initialise HMM
    hmm <- HMM::initHMM(
      c("High", "Low"),
      pricenames,
      transProbs = matrix(c(
        .7, .3,
        .3, .7
      ), 2),
      emissionProbs = matrix(c(
        .0, .3,
        .0, .1,
        .0, .2,
        .2, .2,
        .2, .2,
        .2, .0,
        .1, .0,
        .1, .0,
        .1, .0,
        .05, .0,
        .05, 0
      ), 11)
    )
    tmptypedf <-
      tibble::tibble(type = HMM::simHMM(hmm, as.integer(toDate - fromDate) +
        1)$observation)
    tmpdf <- dplyr::left_join(tmptypedf, tmppricedf, by = "type")
    as.vector(tmpdf$cpm)
  }
  cpmdf <- generateFromFunction(arf, fromDate = fromDate, toDate = toDate, mynames = mynames)
  campdf <- dplyr::left_join(tibble::tibble(strategy = genCampaignStructure(as.integer(toDate - fromDate) + 1)),
    tibble::tibble(strategy = c("Burst", "Normal", "Off"), net = c(2.5 * avgnet, avgnet, 0)),
    by = "strategy"
  )
  stopifnot(nrow(cpmdf) == nrow(campdf))
  # genspend <- function(x) rnorm(length(x), 1, 1/5)*x
  genimp <- function(x) rnorm(length(x), 1, 1 / 5) * campdf$net / x * 1000
  impdf <- tibble::as_tibble(data.frame(date = cpmdf$date, sapply(dplyr::select(cpmdf, -date), genimp)))
  netdf <- data.frame(
    date = impdf$date,
    dplyr::select(impdf, -date) / 1000 * dplyr::select(cpmdf, -date)
  ) %>%
    tibble::as_tibble()
  list(net = netdf, impression = impdf, cpm = cpmdf)
}

#' Generate offline media data
#'
#' @param fromDate the beginning of the time series
#' @param toDate the end of the time series
#' @param mynames the names to attach to the generated data
#' @param avgcpm the average Cost Per 1000 Impressions
#' @param avgnet the average net investment per insertion and day
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
#' mydflist <- generateOfflineData(Sys.Date() - 30, Sys.Date())
#' mydflist[["impression"]] %>%
#'   gather(type, impression, -date) %>%
#'   ggplot(aes(y = impression, x = date, color = type)) +
#'   geom_line() +
#'   theme_minimal()
generateOfflineData <- function(fromDate = Sys.Date() - 1 * 365,
                                toDate = Sys.Date(),
                                mynames = c("tv", "radio", "ooh"),
                                avgcpm = 0.5, avgnet = 10000) {
  genCampaignStructure <- function(n) {
    hmm <- HMM::initHMM(
      c("Burst", "Normal", "Off"),
      c("PriceA", "PriceB"),
      transProbs = matrix(c(
        .7, .05, .1,
        .1, .9, .1,
        .2, .05, .8
      ), 3),
      emissionProbs = matrix(c(
        .3, .7, .5,
        .7, .3, .5
      ), 2)
    )
    HMM::simHMM(hmm, n)$states
  }

  arf <- function(x) {
    cpms <- c(seq(0.1 * avgcpm, 2.5 * avgcpm, length.out = 11))
    pricenames <- paste0("Price", LETTERS[1:length(cpms)])
    tmppricedf <- tibble::tibble(cpm = cpms, type = pricenames)
    # Initialise HMM
    hmm <- HMM::initHMM(
      c("High", "Low"),
      pricenames,
      transProbs = matrix(c(
        .7, .3,
        .3, .7
      ), 2),
      emissionProbs = matrix(c(
        .0, .3,
        .0, .1,
        .0, .2,
        .2, .2,
        .2, .2,
        .2, .0,
        .1, .0,
        .1, .0,
        .1, .0,
        .05, .0,
        .05, 0
      ), 11)
    )
    tmptypedf <-
      tibble::tibble(type = HMM::simHMM(hmm, as.integer(toDate - fromDate) +
        1)$observation)
    tmpdf <- dplyr::left_join(tmptypedf, tmppricedf, by = "type")
    as.vector(tmpdf$cpm)
  }
  cpmdf <- generateFromFunction(arf, fromDate = fromDate, toDate = toDate, mynames = mynames)
  campdf <- dplyr::left_join(tibble::tibble(strategy = genCampaignStructure(as.integer(toDate - fromDate) + 1)),
    tibble::tibble(strategy = c("Burst", "Normal", "Off"), net = c(2.5 * avgnet, avgnet, 0)),
    by = "strategy"
  )
  stopifnot(nrow(cpmdf) == nrow(campdf))
  # genspend <- function(x) rnorm(length(x), 1, 1/5)*x
  genimp <- function(x) rnorm(length(x), 1, 1 / 5) * campdf$net / x * 1000
  impdf <- tibble::as_tibble(data.frame(date = cpmdf$date, sapply(dplyr::select(cpmdf, -date), genimp)))
  netdf <- data.frame(date = impdf$date, dplyr::select(impdf, -date) / 1000 * dplyr::select(cpmdf, -date)) %>%
    tibble::as_tibble()
  list(net = netdf, impression = impdf, cpm = cpmdf)
}

#' Generate offline media data
#'
#' @param fromDate the beginning of the time series
#' @param toDate the end of the time series
#' @param mynames the names to attach to the generated data
#' @param freq the rate of incidence of events
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
#' generateEventData(Sys.Date() - 30, Sys.Date()) %>%
#'   gather(type, value, -date) %>%
#'   ggplot(aes(y = value, x = date, color = type)) +
#'   geom_line() +
#'   theme_minimal()
generateEventData <- function(fromDate = Sys.Date() - 1 * 365,
                              toDate = Sys.Date(),
                              mynames = c("event_a", "event_b"),
                              freq = 0.01) {
  arf <- function(x) rpois(as.integer(toDate - fromDate) + 1, freq)
  generateFromFunction(arf, fromDate = fromDate, toDate = toDate, mynames = mynames)
}

#' Generate a marketing mix modeling covariates data set
#'
#' This function generates a marketing mix modeling covariates data set based on
#' the parameters given. It uses many different stochastical processes to
#' accomplish this and the dynamics behind them are not available to the user to
#' manipulate. This is supposed to generate all tha variables you use in a
#' marketing mix model except for the response variable i.e. the dependent
#' variable.
#'
#'
#' @param fromDate the beginning of the time series
#' @param toDate the end of the time series
#' @param sector the name of the sector to simulate (currently not used)
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
#' a <- 1
generateCovariatesData <-
  function(fromDate = Sys.Date() - 1 * 365,
           toDate = Sys.Date(),
           sector = "retail",
           onlineInsertionNames = c("display", "facebook", "search_branded"),
           offlineInsertionNames = c("tv", "radio", "ooh", "print"),
           priceNames = c("price_product_a", "price_product_b", "price_product_c"),
           distributionNames = c("dist_product_a", "dist_product_b", "dist_product_c"),
           weatherNames = c("sunshine", "precipitation", "temperature"),
           competitorNames = c("competitor_a", "competitor_b", "competitor_c"),
           macroNames = c("cpi", "cci", "gdp"),
           eventNames = c("event_a", "event_b")) {
    mydf <- tibble::tibble(date = seq(fromDate, toDate, by = "1 day"))

    # These come as list of three tibbles.
    ondf <- generateOnlineData(fromDate, toDate, onlineInsertionNames)
    ofdf <- generateOfflineData(fromDate, toDate, offlineInsertionNames)
    # Fix them into usable flat tables
    netdf <- dplyr::inner_join(
      ondf$net %>% setNames(c("date", paste0("net_", names(.)[-1]))),
      ofdf$net %>% setNames(c("date", paste0("net_", names(.)[-1])))
    )
    cpmdf <- dplyr::inner_join(
      ondf$cpm %>% setNames(c("date", paste0("cpm_", names(.)[-1]))),
      ofdf$cpm %>% setNames(c("date", paste0("cpm_", names(.)[-1])))
    )
    impdf <- dplyr::inner_join(
      ondf$impression %>% setNames(c("date", paste0("imp_", names(.)[-1]))),
      ofdf$impression %>% setNames(c("date", paste0("imp_", names(.)[-1])))
    )

    # These come as pure tibbles
    prdf <- generatePriceData(fromDate, toDate, priceNames)
    didf <- generateDistributionData(fromDate, toDate, distributionNames)
    wedf <- generateWeatherData(fromDate, toDate, weatherNames)
    codf <- generateCompetitorData(fromDate, toDate, competitorNames)
    madf <- generateMacroData(fromDate, toDate, macroNames)
    # evdf <- generateEventData(fromDate, toDate, eventNames)

    mydf <- Reduce(
      function(x, y) dplyr::inner_join(x, y, by = "date"),
      list(mydf, wedf, codf, madf, didf, prdf, netdf, impdf, cpmdf)
    )
    mydf
  }
