#' Generate a marketing mix modeling data set
#'
#' This function generates a marketing mix modeling data set based on the
#' parameters given for a Retail case with Revenue as it's response.
#' It uses many different stochastical processes to accomplish
#' this and the dynamics behind them are not available to the user to manipulate.
#'
#'
#' @param fromDate the beginning of the time series
#' @param toDate the end of the time series
#' @param kpi the name of the kpi (response) to simulate
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
#' a<-1
generateRetailData <-
  function(fromDate = Sys.Date() - 1 * 365,
           toDate = Sys.Date(),
           kpi = 'revenue',
           sector = 'retail',
           onlineInsertionNames = c('display', 'facebook', 'search_branded'),
           offlineInsertionNames = c('tv', 'radio', 'ooh', 'print'),
           priceNames=c('price_product_a', 'price_product_b', 'price_product_c'),
           distributionNames=c('dist_product_a', 'dist_product_b', 'dist_product_c'),
           weatherNames = c('sunshine', 'precipitation', 'temperature'),
           competitorNames = c('competitor_a', 'competitor_b', 'competitor_c'),
           macroNames = c('cpi', 'cci', 'gdp'),
           eventNames = c('event_a', 'event_b')) {
    mydf <- tibble::tibble(date=seq(fromDate, toDate, by="1 day"))

    # These come as list of three tibbles.
    ondf <- generateOnlineData(fromDate, toDate, onlineInsertionNames)
    ofdf <- generateOfflineData(fromDate, toDate, offlineInsertionNames)
    # Fix them into usable flat tables
    netdf <- dplyr::inner_join(
      ondf$net %>% setNames(c('date', paste0('net_', names(.)[-1]))),
      ofdf$net %>% setNames(c('date', paste0('net_', names(.)[-1])))
    )
    cpmdf <- dplyr::inner_join(
      ondf$cpm %>% setNames(c('date', paste0('cpm_', names(.)[-1]))),
      ofdf$cpm %>% setNames(c('date', paste0('cpm_', names(.)[-1])))
    )
    impdf <- dplyr::inner_join(
      ondf$impression %>% setNames(c('date', paste0('imp_', names(.)[-1]))),
      ofdf$impression %>% setNames(c('date', paste0('imp_', names(.)[-1])))
    )
    # These come as pure tibbles
    prdf <- generatePriceData(fromDate, toDate, priceNames)
    didf <- generateDistributionData(fromDate, toDate, distributionNames)
    wedf <- generateWeatherData(fromDate, toDate, weatherNames)
    codf <- generateCompetitorData(fromDate, toDate, competitorNames)
    madf <- generateMacroData(fromDate, toDate, macroNames)
    # evdf <- generateEventData(fromDate, toDate, eventNames)

    mydf <- Reduce(function(x, y) dplyr::inner_join(x,y, by = "date"),
                   list(mydf, wedf, codf, madf, didf, prdf, netdf, impdf, cpmdf))

    # This only covers the retail revenue case!!!
    e_prdf <- t(t(prdf[,-1]^2) * runif(length(priceNames), 0.5, 1.5))
    # e_didf <-
  }
