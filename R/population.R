#' Estimate units sold per day based on industry and population size of country.
#'
#' This utilizes heuristics to create a realistic estimation of how many units
#' a company can seel within an industry given the population size of that
#' country.
#'
#' @param industry the industry to simulate which defaults to telecom
#' @param population the population size which defaults to 5 million people
#'
#' @return the number of expected sold units per average day
#' @importFrom stats rnorm rpois runif setNames
#' @importFrom MASS rnegbin
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(MASS)
#' avgsalesperday <- salesUnitsPerDay("telecom")
#' avgsales <- MASS::rnegbin(100, avgsalesperday[["UnitsPerDay"]], 1)
#' qplot(1:length(avgsales), avgsales) + geom_line()
salesUnitsPerDay <- function(industry = "telecom", population = 5e6) {
  industries <- c("telecom", "betting")
  if (!(industry %in% industries)) {
    stop(paste0(
      "The only industries supported now are ",
      paste0(industries, collapse = ", ")
    ))
  }
  if (industry == "telecom") { # sell to 1% of population in 365 days
    ret <- list(Unit = "Acquisitions", UnitsPerDay = 0.01 * population / 365)
  } else if (industry == "betting") { # sell to 1% of population in 365 days
    ret <- list(Unit = "New Depositing Players", UnitsPerDay = 0.01 * population / 365)
  }
  ret
}
