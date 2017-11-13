

generateCampaigns<-function(n=5, l=30, fromDate=Sys.Date()-364, toDate=Sys.Date()){
  dateseq <- seq(fromDate, toDate, by="1 day")
  numPossibleCampaigns<-as.integer(length(dateseq)/l)
}

