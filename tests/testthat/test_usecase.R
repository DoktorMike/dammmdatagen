# GENERATE DATA
ondf <- generateOnlineData()
ofdf <- generateOfflineData()
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
prdf <- generatePriceData()
didf <- generateDistributionData()
wedf <- generateWeatherData()
codf <- generateCompetitorData()
madf <- generateMacroData()
evdf <- generateEventData()

# GENERATE EFFECT
margin <- 400
roi <- 1.0
adsf <- function(x, b = 1, a = max(x), l = rbeta(1, 1, 3), p = 1.0) {
        b * tanh(as.vector(stats::filter(x, l, method = "recursive")) / a) * rbinom(1, 1, p)
}
linf <- function(x, b = 0, p = 1.0) b * x * rbinom(1, 1, p)
invf <- function(x, b = 1, a = 1, p = 1.0) b / ((x)^a) * rbinom(1, 1, p)

#x<-1:nrow(prdf); qplot(x, rnorm(length(x), 1500, 20)+1000*(sin(x/(15*2*pi))))

ebadf <-  tibble::tibble(base = rnorm(nrow(prdf), 1000, 10) + 1000 * (sin(x / (15 * 2 * pi))))
enetdf <- numcolwise(adsf, b = 100, p = 0.7)(netdf); sum(netdf[, -1]) / sum(enetdf)
eprdf <-  numcolwise(invf, b = 20000, p = 1.0)(prdf); sum(eprdf)
edidf <-  numcolwise(linf, b = 0.22, p = 1.0)(didf); sum(edidf)
ewedf <-  numcolwise(linf, b = 170, p = 1.0)(wedf); sum(ewedf)
ecodf <-  numcolwise(linf, b = -1e-4, p = 1.0)(codf); sum(ecodf)
emadf <-  numcolwise(linf, b = 1, p = 1.0)(madf); sum(emadf)
y <- rowSums(Reduce(bind_cols, list(enetdf, eprdf, edidf, ewedf, ecodf, emadf))) + ebadf$base
#qplot(madf$date, y) + geom_line()
ret <- list(covariates = list(Price = prdf, Distribution = didf, Weather = wedf,
			      Media = netdf, Competitor = codf, Macro = madf, Events = evdf),
	    effects = list(Base = ebadf, Price = eprdf, Distribution = edidf, Weather = ewedf,
			   Media = enetdf, Competitor = ecodf, Macro = emadf),
	    response = y
)
qplot(madf$date, y) + geom_line() + ylim(0, NA)
entrytocolname <- function(x) a <- ret[["effects"]][[x]] %>% setNames(c(tolower(paste0(x, "_", names(.)))))
Reduce(dplyr::bind_cols, lapply(names(ret[["effects"]]), entrytocolname)) %>%
	dplyr::mutate(date=prdf$date) %>%
	tidyr::pivot_longer(-date, names_to="variable", values_to="value") %>%
	ggplot(aes(x=date, y=value, fill=variable)) + geom_bar(stat="identity")
