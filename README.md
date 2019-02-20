
<!-- README.md is generated from README.Rmd. Please edit that file -->
dammmdatagen
============

The goal of dammmdatagen is to make it easy for marketing mix modeling professionals to get access to realistic data sets where the ground truth is known. This fascilitates our development and provides in the end more value for all stakeholders of MMM.

Build status etc
----------------

[![Travis-CI Build Status](https://travis-ci.org/DoktorMike/dammmdatagen.svg?branch=master)](https://travis-ci.org/DoktorMike/dammmdatagen) [![Coverage Status](https://img.shields.io/codecov/c/github/DoktorMike/dammmdatagen/master.svg)](https://codecov.io/github/DoktorMike/dammmdatagen?branch=master)

Installation
------------

You can install dammmdatagen from github with:

``` r
# install.packages("devtools")
devtools::install_github("DoktorMike/dammmdatagen")
```

Quick start
-----------

This is a basic example which shows you how to generate a small 1 year data set.

``` r
# load useful libraries
library(dammmdatagen)

# generate a basic data set
mydf <- generateCovariatesData()
#> Joining, by = "date"
#> Joining, by = "date"
#> Joining, by = "date"
head(mydf)
#> # A tibble: 6 x 37
#>   date       sunshine precipitation temperature competitor_a competitor_b
#>   <date>        <dbl>         <dbl>       <dbl>        <dbl>        <dbl>
#> 1 2018-02-19    -2.26         -2.98      -0.811        80000        50000
#> 2 2018-02-20    -2.96         -3.60       0.434        80000        80000
#> 3 2018-02-21    -2.35         -3.50      -0.685        80000        80000
#> 4 2018-02-22    -2.58         -2.40       0.500       100000        20000
#> 5 2018-02-23    -1.65         -1.62       0.120        70000       100000
#> 6 2018-02-24     1.39         -2.81       0.582        40000        20000
#> # … with 31 more variables: competitor_c <dbl>, cpi <dbl>, cci <dbl>,
#> #   gdp <dbl>, dist_product_a <dbl>, dist_product_b <dbl>,
#> #   dist_product_c <dbl>, price_product_a <dbl>, price_product_b <dbl>,
#> #   price_product_c <dbl>, net_display <dbl>, net_facebook <dbl>,
#> #   net_search_branded <dbl>, net_tv <dbl>, net_radio <dbl>,
#> #   net_ooh <dbl>, net_print <dbl>, imp_display <dbl>, imp_facebook <dbl>,
#> #   imp_search_branded <dbl>, imp_tv <dbl>, imp_radio <dbl>,
#> #   imp_ooh <dbl>, imp_print <dbl>, cpm_display <dbl>, cpm_facebook <dbl>,
#> #   cpm_search_branded <dbl>, cpm_tv <dbl>, cpm_radio <dbl>,
#> #   cpm_ooh <dbl>, cpm_print <dbl>
```

We can do a lot more of course! In this small snippet we'll generate 1 month worth of competitor media spendings data and plot that out.

``` r
library(dammmdatagen)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales) 

generateCompetitorData(fromDate = Sys.Date()-30, toDate = Sys.Date()) %>% 
  gather("competitor", "spend", -"date") %>% 
  ggplot(aes(y=spend, x=date, fill=competitor)) + 
  geom_bar(stat="identity", position = position_stack()) + 
  theme_minimal() + scale_y_continuous(labels = dollar_format(prefix = "kr. "))
```

![](figs/README-competitorspendplot-1.png)

Just as we can generate competitor spending data we can also generate macroeconomical data. These types of indicators are typically slow moving over time with minor temporal differences.

``` r
generateMacroData(fromDate = Sys.Date()-30, toDate = Sys.Date()) %>% 
  gather("indicator", "value", -"date") %>% 
  ggplot(aes(y=value, x=date, color=indicator)) + 
  geom_line(size = 1.5) + theme_minimal()
```

![](figs/README-macroecondataplot-1.png)

Event type data
---------------

Event data are modeled as a poisson distribution with a low incidence.

``` r
generateEventData(Sys.Date()-265, Sys.Date()) %>%
  gather(type, value, -date) %>%
  ggplot(aes(y=value, x=date, fill=type)) +
  geom_bar(stat="identity") + theme_minimal()
```

![](figs/README-eventdata1-1.png)

The incidence can of course be controlled. This is done via the freq parameter.

``` r
generateEventData(Sys.Date()-265, Sys.Date(), freq = 0.1) %>%
  gather(type, value, -date) %>%
  ggplot(aes(y=value, x=date, fill=type)) +
  geom_bar(stat="identity") + theme_minimal()
```

![](figs/README-eventdata2-1.png)

Media generation
----------------

Generating media is in general a bit more complicated as we need more information since in MMM models that's what we primarily care about. So we need three data.frames; the net, the impressions and the cpms. We also differentiate between offline and online media. This difference is rather artificial right now but it's to futureproof the package.

``` r
mydflist <- generateOnlineData(Sys.Date()-30, Sys.Date())
mydflist[["impression"]] %>%
  gather(type, impression, -date) %>%
  ggplot(aes(y=impression, x=date, fill=type)) +
  geom_bar(stat="identity") + theme_minimal()
```

![](figs/README-onlineimpdata-1.png)
