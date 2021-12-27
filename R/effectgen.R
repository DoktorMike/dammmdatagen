#' Generate a marketing mix modeling data set
#'
#' This function generates a marketing mix modeling data set based on the
#' parameters given for a Retail case with Revenue as it's response.
#' It uses many different stochastical processes to accomplish
#' this and the dynamics behind them are not available to the user to manipulate.
#'
#' @importFrom magrittr "%>%"
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
#' library(ggplot2)
#' library(nord)
#' ret <- generateRetailData()
#' dates <- ret[["covariates"]][["Macro"]][["date"]]
#' qplot(dates, ret[["response"]]) + geom_line() + ylim(0, NA)
#' # entrytocolname <- function(x) a <- ret[["effects"]][[x]] %>% setNames(c(tolower(paste0(x, "_", names(.)))))
#' entrytocolname <- function(x) tibble::tibble(rowSums(ret[["effects"]][[x]])) %>% setNames(x)
#' Reduce(dplyr::bind_cols, lapply(names(ret[["effects"]]), entrytocolname)) %>%
#'         dplyr::mutate(date = dates) %>%
#'         tidyr::pivot_longer(-date, names_to = "variable", values_to = "value") %>%
#'         ggplot2::ggplot(ggplot2::aes(x = date, y = value, fill = variable)) +
#'         ggplot2::geom_bar(stat = "identity")
generateRetailData <-
        function(fromDate = Sys.Date() - 3 * 365,
                 toDate = Sys.Date(),
                 kpi = "units",
                 sector = "retail",
                 onlineInsertionNames = c("display", "facebook", "search_branded"),
                 offlineInsertionNames = c("tv", "radio", "ooh", "print"),
                 priceNames = c("price_product_a", "price_product_b", "price_product_c"),
                 distributionNames = c("dist_product_a", "dist_product_b", "dist_product_c"),
                 weatherNames = c("sunshine", "precipitation", "temperature"),
                 competitorNames = c("competitor_a", "competitor_b", "competitor_c"),
                 macroNames = c("cpi", "cci", "gdp"),
                 eventNames = c("event_a", "event_b")) {
                # GENERATE DATA
                # fromDate <- as.Date("2019-01-01")
                # toDate <- as.Date("2021-12-31")
                datedf <- tibble::tibble(date = seq(fromDate, toDate, by = "1 day"))
                ondf <- generateOnlineData(fromDate, toDate)
                ofdf <- generateOfflineData(fromDate, toDate)
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
                prdf <- generatePriceData(fromDate, toDate)
                didf <- generateDistributionData(fromDate, toDate)
                wedf <- generateWeatherData(fromDate, toDate)
                codf <- generateCompetitorData(fromDate, toDate)
                madf <- generateMacroData(fromDate, toDate)
                evdf <- generateEventData(fromDate, toDate)

                # GENERATE EFFECT
                margin <- 400
                roi <- 1.0
                adsf <- function(x, b = 1, a = max(x), l = stats::rbeta(1, 1, 3), p = 1.0) {
                        b * tanh(as.vector(stats::filter(x, l, method = "recursive")) / a) * stats::rbinom(1, 1, p)
                }
                linf <- function(x, b = 0, p = 1.0) b * x * stats::rbinom(1, 1, p)
                invf <- function(x, b = 1, a = 1, p = 1.0) b / ((x)^a) * stats::rbinom(1, 1, p)

                ebadf <- tibble::tibble(base = stats::rnorm(nrow(prdf), 500, 10) + 500 * (sin(1:nrow(prdf) / (15 * 2 * pi))))
                enetdf <- plyr::numcolwise(adsf, b = 100, p = 0.7)(netdf)
                eprdf <- plyr::numcolwise(invf, b = 20000, p = 1.0)(prdf)
                edidf <- plyr::numcolwise(linf, b = 0.22, p = 1.0)(didf)
                ewedf <- plyr::numcolwise(linf, b = 100, p = 1.0)(wedf)
                ecodf <- plyr::numcolwise(linf, b = -1e-3, p = 1.0)(codf)
                emadf <- plyr::numcolwise(linf, b = 1, p = 1.0)(madf)
                y <- rowSums(Reduce(dplyr::bind_cols, list(enetdf, eprdf, edidf, ewedf, ecodf, emadf))) + ebadf$base
                # qplot(madf$date, y) + geom_line()
                ret <- list(
                        covariates = list(
                                Price = prdf, Distribution = didf, Weather = wedf,
                                Media = netdf, Competitor = codf, Macro = madf, Events = evdf
                        ),
                        effects = list(
                                Base = ebadf, Price = eprdf, Distribution = edidf, Weather = ewedf,
                                Media = enetdf, Competitor = ecodf, Macro = emadf
                        ),
                        response = y
                )
                ret
        }
