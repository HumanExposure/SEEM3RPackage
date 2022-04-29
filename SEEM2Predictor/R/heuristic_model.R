#Pulls all five descriptors needed to run HeurisitcPredict
heuristic_model <- function(chem.cas,MolWt,
                            DSSToxID=NULL,
                            UseWambaugh2014=T,
                            UseDirectNHANES=T,
                            SEED=42)
{
  these.props <- subset(chemprop.table,CASRN==this.cas)
  if (is.null(DSSToxID)) DSSToxID <- paste("DTXSID",these.props[1,"DSSTox_GSID"],sep="")
  # See if it's in there already:
  if (UseWambaugh2014|UseDirectNHANES)
  {
    out <- HTExppredict(DSSToxID=DSSToxID,UseWambaugh2014=UseWambaugh2014,UseDirectNHANES=UseDirectNHANES,SEED=SEED)
    if (this.cas %in% HPV[,"CAS"])
    {
      PrVol <- IUR.to.geometric.mean(HPV[HPV[,"CAS"]==this.cas,"CHEM_AGG_P"])
    } else {
      PrVol <- 25000/2.204/365.25/2
    }
    these.paths <- subset(NewUseTable2,CASRN==this.cas)
    if (dim(these.paths)[1]==0)
    {
      ConsumerAndIndustrial <- 0
      IndustrialNoConsumer <- 0
      PesticideActive <- 0
      PesticideInert <- 0
    } else {
      ConsumerAndIndustrial <- these.paths[1,"CONSUMER.AND.INDUSTRIAL.PROCESS"]
      IndustrialNoConsumer <- these.paths[1,"INDUSTRIAL.PROCESS.NO.CONSUMER"]
# Recreate the pesticide active column:
      PesticideActive <- these.paths[1,"PESTICIDE.ACTIVE.NO.CONSUMER"]+
        these.paths[1,"PESTICIDE.ACTIVE.AND.CONSUMER"]
      PesticideInert <- these.paths[1,"PESTICIDE.INERT"]
    }
  }
  if (class(out)=="character") #Should be a matrix if it was in the table
  {
    out <- HTExppredict(
      ConsumerAndIndustrial = ConsumerAndIndustrial,
      IndustrialNoConsumer = IndustrialNoConsumer,
      PrVol = PrVol,
      PesticideActive = PesticideActive,
      PesticideInert = PesticideInert,
      MolWt = MolWt)
  }
  new.row <- data.frame(Substance_CASRN=this.cas,
               DSSTox_Substance_Id=DSSToxID,
               Structure_MolWt=MolWt,
               PrVol = PrVol,
               ConsumerAndIndustrial = ConsumerAndIndustrial,
               IndustrialNoConsumer = IndustrialNoConsumer,
               PesticideActive = PesticideActive,
               PesticideInert = PesticideInert,stringsAsFactors=F)

  for (group in rownames(out))
  {
    new.row <- cbind(new.row,out[group,3])
    colnames(new.row)[length(new.row)] <- paste(group,"Median",sep=".")
    new.row <- cbind(new.row,out[group,4])
    colnames(new.row)[length(new.row)] <- paste(group,"Upper95",sep=".")
  }
  return(new.row)
}
