# For some unknown reason the 50th percentile (but not the rest) in the OnlyP 
# file Woody originally used for the ExposurePredict package disagrees with 
# the published table (Ring et al. 2017)

# So let's use the published values:

load("OnlyPests.RData")
library(readxl)
Ring2017 <- read_excel("SupplementalTable-NHANESInferrence.xlsx",skip=6)
Ring2017 <- as.data.frame(Ring2017)
save(Ring2017,file="Ring2017.RData",version=2)

for (this.demo in names(OnlyP))
{
  ring.colname <- this.demo
  if (regexpr("66",ring.colname)!=-1)
  {
    ring.colname <- "Age65."    
  } else if (regexpr("years",ring.colname)!=-1)
  {
    ring.colname <- gsub("_years","",ring.colname)
    ring.colname <- gsub("-",".",ring.colname)
    ring.colname <- paste("Age",ring.colname,sep="")
  } else if (regexpr("BMI",ring.colname)!=-1)
  {
    ring.colname <- gsub("_","",ring.colname)
    ring.colname <- gsub("LE","LT",ring.colname)
  }
  ring.colname <-paste("Median",ring.colname,"Median",sep=".")
  
  for (this.DTXSID in names(OnlyP[[this.demo]][,"50%"]))
  {
    OnlyP[[this.demo]][this.DTXSID,"50%"] <- Ring2017[Ring2017$DTXSID==this.DTXSID,ring.colname]
  }
}

save(OnlyP,file="OnlyPests.RData")