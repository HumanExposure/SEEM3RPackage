HTExppredict <-
    function(subgroup=c("All","Total","ReproAgeFemale","Male","Female",
                        "6-11_years","12-19_years","20-65_years","66+years",
                        "BMI_GT_30", "BMI_LE_30"),
             DSSToxID="",
             ConsumerAndIndustrial=NULL, IndustrialNoConsumer=NULL, PrVol=NULL,
             PesticideActive=NULL, PesticideInert=NULL, MolWt=NULL,
             UseWambaugh2014=T,UseDirectNHANES=T,SEED=42) 
{
  AllSubgroups <- c("All","Total","ReproAgeFemale","Male","Female",
                    "6-11_years","12-19_years","20-65_years","66+years",
                    "BMI_GT_30", "BMI_LE_30")
  badgroups <- setdiff(subgroup, AllSubgroups)
  if (length(badgroups) > 0) {
      stop(paste("Subgroups",paste(badgroups,collapse=", "),
                 "are not available."))
  }
  if ("All" %in% subgroup)
      subgroup <- c("Total","ReproAgeFemale","Male","Female",
                    "6-11_years","12-19_years","20-65_years",
                    "66+years","BMI_GT_30","BMI_LE_30")
  if (DSSToxID == "") {
      if (is.null(ConsumerAndIndustrial))
          stop("ConsumerAndIndustrial is missing with no default")
      if (is.null(IndustrialNoConsumer))
          stop("IndustrialNoConsumer is missing with no default")
      if (is.null(PrVol))
          stop("PrVol is missing with no default")
      if (is.null(PesticideActive))
          stop("PesticideActive is missing with no default")
      if (is.null(PesticideInert))
          stop("PesticideInert is missing with no default")
      if (is.null(MolWt)) {
          MolWt <- 1
          warning("MolWt is missing. Answer will have units nm/kg/day.")
      }
      HeuristicPredict(subgroup, ConsumerAndIndustrial,
                       IndustrialNoConsumer, PrVol,
                       PesticideActive, PesticideInert, MolWt,SEED=SEED)
  } else {
      if (UseDirectNHANES & DSSToxID %in% rownames(OnlyP[[1]])) {
          tmp <- NULL
          for (this.group in subgroup)
           if (DSSToxID %in% rownames(OnlyP[[this.group]]))
           {
             tmp <- rbind(tmp,OnlyP[[this.group]][DSSToxID,])
           } else {
             tmp <- rbind(tmp,HPreds[[this.group]][DSSToxID,])
           }
          print(DSSToxID)
          print(class(DSSToxID))
          print(tmp)
          print(class(tmp))
          print(OnlyP[[1]][DSSToxID,])
          print(class(OnlyP[[1]][DSSToxID,]))
          rownames(tmp) <- subgroup
          return(tmp)
      } else if (UseWambaugh2014 & DSSToxID %in% rownames(HPreds[[1]])) {
          tmp <- do.call("rbind",
                         lapply(subgroup, function(z) HPreds[[z]][DSSToxID,]))
          rownames(tmp) <- subgroup
          return(tmp)
      } else {
          paste("DSSToxID",DSSToxID,"not in Expocast database.")
      }
  }
}
