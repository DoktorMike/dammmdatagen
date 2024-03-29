% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datagen.R
\name{generateWeatherData}
\alias{generateWeatherData}
\title{Generate weather data}
\usage{
generateWeatherData(
  fromDate = Sys.Date() - 1 * 365,
  toDate = Sys.Date(),
  mynames = c("sunshine", "precipitation", "temperature")
)
}
\arguments{
\item{fromDate}{the beginning of the time series}

\item{toDate}{the end of the time series}

\item{mynames}{the names to attach to the generated data}
}
\value{
a tibble with the generated data one column for each element in name
}
\description{
Generate weather data
}
\examples{
library(ggplot2)
library(dplyr)
library(tidyr)
generateWeatherData(Sys.Date() - 30, Sys.Date()) \%>\%
  gather(type, measure, -date) \%>\%
  ggplot(aes(y = measure, x = date, color = type)) +
  geom_line() +
  theme_minimal()
}
