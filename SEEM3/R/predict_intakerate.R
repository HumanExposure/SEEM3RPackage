#'Calculate probababilistithe probabality of exposure via various pathways.
#'
#'@param 
#'  
#'@return 
#'
#'@details 
#'
#'@examples 

#'
#'@author John Wambaugh and Caroline Ring
#'
#' "Consensus modeling of median chemical intake for the US population based on
#' predictions of exposure pathways." 
#' Environmental Science & Technology 53.2 (2018): 719-732.
#'
#'@export predict_intakerate
#'@import parallel
predict_intakerate<- function(
  chem.props,
  file.stub="new-seem",
  rand.seed=123456,
  num.draws=500,
  num.cores=2,
  model.preds=NULL)
{
# set the random number generator seed for reproductibility:
  set.seed(rand.seed)

# Make chem.props a data.table
  chem.props <- as.data.table(chem.props)
  
# Get rid of NA deltas:
  chem.props <- subset(chem.props,
    !is.na(delta.Diet.pred) &
    !is.na(delta.Res.pred) &
    !is.na(delta.Pest.pred) &
    !is.na(delta.Indust.pred))


#MODEL.NAMES <- c(
#  "SHEDS.Direct", 
#  "SHEDS.Indirect",
#  "FINE",
#  "Food.Contact",
#  "REDS",
#  "RAIDAR",
#  "RAIDAR.ICE",
#  "USETox.Pest",
#  "USETox.Indust",
#  "USETox.Res",
#  "USETox.Diet",
#  "Production.Volume",
#  "Stockholm")
#NUM.MODELS <- length(MODEL.NAMES)
# 
#PATHWAY.NAMES <- c(
#  "Diet",
#  "Res",
#  "Pest",
#  "Indust")
#NUM.PATHWAYS <- length(PATHWAY.NAMES)
#deltas <- paste("delta",PATHWAY.NAMES,"pred",sep=".")
#model.preds <- paste("Pred",MODEL.NAMES,"log.scale",sep=".")
#save(MODEL.NAMES,NUM.MODELS,PATHWAY.NAMES,NUM.PATHWAYS,deltas,model.preds,
#  file="../data/globals.RData")

# Initialize model predictions (all NA's except Stockholm and production volume)
  for (this.model in MODEL.NAMES) 
  {
    chem.props[,eval(this.model):=as.numeric(NA)]
  #  new.seem.unpredicted[,eval(this.model):=as.numeric(eval(this.model))]
  }
  chem.props[,Stockholm:=0]
  
# Check to see if there are any relevant model predictions provided:
  if (!is.null(model.preds))
  {
    print("Model predictions for these models found:")
    print(intersect(MODEL.NAMES,colnames(model.preds)))
    print(paste("Model predictions for",length(
      intersect(model.preds$DSSTox_Substance_Id,
      chem.props$DSSTox_Substance_Id)),"chemicals found,"))
    for (this.chem in 
      intersect(model.preds$DSSTox_Substance_Id,chem.props$DSSTox_Substance_Id)) 
    {
      for (this.model in intersect(MODEL.NAMES,colnames(model.preds))) 
      {
        chem.props[,eval(this.model):=as.numeric(model.preds[
          DSSTox_Substance_Id==this.chem,this.model])]
      }
    }
  } else print("No model predictions used (set model.preds if desired)")

# library(gtools)
# stockholm <- read.xls("StockholmConvention.xlsx",stringsAsFactors=F)
# save(stockholm,file="../data/StockholmConvention.RData")

# Add Stockolm convention chemicals:
  for (this.chem in stockholm$DSSTox_Substance_Id) 
  {
    if (this.chem %in% chem.props$DSSTox_Substance_Id) 
    {
      chem.props[DSSTox_Substance_Id==this.chem,
        Stockholm := 1]
    }
  }

# CDR.HPV <- read.xls("CDR-HPV-2016.xlsx",stringsAsFactors=FALSE)
# CDR.HPV <- subset(CDR.HPV,DSSTox_Substance_Id!="")
# CDR.HPV$CAS <- CDR.HPV$Substance_CASRN
# CDR.HPV$Compound <- CDR.HPV$Substance_Name
# CDR.HPV <- cbind(CDR.HPV,
#   unlist(lapply(as.list(CDR.HPV$X2015.Aggregate.PV..Lbs..),
#   CDR.to.geometric.mean)))
# colnames(CDR.HPV)[length(colnames(CDR.HPV))] <- "kgs.day"
# save(CDR.HPV,file="../data/CDR-HPV.RData")

# Add production volume:
  for (this.chem in 
    intersect(CDR.HPV$DSSTox_Substance_Id,chem.props$DSSTox_Substance_Id)) 
  {
    this.subset <- CDR.HPV[CDR.HPV$DSSTox_Substance_Id==this.chem,]
    if (dim(this.subset)[1]>1)
    {
      if (any(this.subset$X2015.Aggregate.PV..Lbs.. != "Withheld")) 
      {
        this.subset <- subset(this.subset,
          this.subset$X2015.Aggregate.PV..Lbs.. != "Withheld")
      } else this.subset <- this.subset[1,]
    }
    chem.props[DSSTox_Substance_Id==this.chem,
      Production.Volume :=
      this.subset[,"kgs.day"]]
  }
# If a chemical is not on the HPV list production must be < 25000 lbs/year
  chem.props[is.na(chem.props$Production.Volume),
    Production.Volume:=25000/2/2.204/365.25]

# Make colnames match what is expected:
  if ("CASRN" %in% 
    colnames(chem.props))
  {
    colnames(chem.props)[colnames(chem.props) ==
      "CASRN"] <-  
      "Substance_CASRN"
  }
  if ("PREFERRED_NAME" %in% 
    colnames(chem.props))
  {
    colnames(chem.props)[colnames(chem.props) ==
      "PREFERRED_NAME"] <-  
      "Substance_Name"
  }
  
  if (!("Substance_CASRN" %in% colnames(chem.props)))
  {
    chem.props$Substance_CASRN <- NA
  }

# Cut the data.table down to just the columns we need:
  chem.props <- chem.props[,c(
    "DSSTox_Substance_Id",
    "Substance_CASRN",
    "Substance_Name",
    MODEL.NAMES,
    paste("delta",PATHWAY.NAMES,"pred",sep=".")),
    with=F]

# Rename columns to match 2018:
  colnames(chem.props)[
    colnames(chem.props)=="DSSTox_Substance_Id"] <-
    "dsstox_substance_id"
  colnames(chem.props)[
    colnames(chem.props)=="Substance_CASRN"] <-
    "CAS"
  setnames(chem.props,
           MODEL.NAMES,
           paste("Pred",
                 MODEL.NAMES,
                 sep="."))

# Choose which iterations from Markov Chain to Use

  these.iter <- sample(1:dim(samp.dt)[1],num.draws)
  sub.samp <- samp.dt[these.iter]
  sub.samp[,iteration:=NULL]
  sub.samp <- as.matrix(sub.samp)
  
  
# model.mins <- readRDS("../data/model.mins-2018-06-05.RData")
# model.means <- readRDS("../data/model.means-2018-06-05.RData")
# model.sds <- readRDS("../data/model.sds-2018-06-05.RData")
# save(model.mins,model.means,model.sds,file="../data/modelstats.RData")

# Scale the model predictions:
  chem.preds <- chem.props
  for (this.model in MODEL.NAMES)
  {
    scalenm <- paste("Pred",this.model,"log",sep=".")
    colnm <- paste("Pred",this.model,"log.scale",sep=".")
    chem.preds[,eval(colnm) :=
      (log(as.numeric(unlist(chem.preds[,
      paste("Pred",this.model,sep="."),with=F]))) -
      model.means[scalenm]) / model.sds[scalenm]]
    chem.preds[is.na(unlist(chem.preds[,colnm,with=F])),colnm] <- 0
    if (any(unlist(chem.preds[,colnm,with=F])==-Inf))
    {
      chem.preds[unlist(chem.preds[,colnm,with=F])==-Inf,colnm] <- 
        model.mins[colnm]
    }
  }
  chem.preds[,Pred.Stockholm.log.scale:=Pred.Stockholm]

# known.delta.matrix <- readRDS("../data/known-deltas-2018-06-05.RData")
# save(known.delta.matrix,file="../data/knowndeltas.RData")

# Overwrite predicted deltas with any known deltas:
  for (this.chem in rownames(known.delta.matrix))
    for (this.path in colnames(known.delta.matrix))
      chem.preds[dsstox_substance_id==this.chem,
        paste("delta",this.path,"pred",sep=".") :=
        known.delta.matrix[this.chem,this.path]]

# Set up a cluster for making predictions:
  cl <- makeCluster(num.cores)
  clusterSetRNGStream(cl, rand.seed)
# Export global variables/functions:
  clusterExport(cl,c(
    "fast_seem",
    "PATHWAY.NAMES",
    "MODEL.NAMES",
    "NUM.MODELS",
    "NUM.PATHWAYS",
    "deltas",
    "model.preds",
    "wmdl",
    "wpath",
    "NUM.NONZERO.WEIGHTS"))
# Export local variables:
  clusterExport(cl, c(
    "chem.preds",
    "sub.samp",
    "num.draws"),
    envir=environment())
  clusterEvalQ(cl,library(data.table))

# Make the predictions:
  seem.preds <-  matrix(unlist(parLapply(cl,
    as.list(chem.preds$dsstox_substance_id), 
    function(x) quantile(unlist(fast_seem(x)),c(0.5,0.025,0.975)))),
    ncol=3,
    byrow=T)

# Close the cluster:
  stopCluster(cl)
  
#Convert back to mg/kg/day units:
  seem.preds <- seem.preds*nhanes.sd+nhanes.mean

# Convert back to aritmetic scale:
  chem.preds[,seem3:=exp(seem.preds[,1])]
  chem.preds[,seem3.l95:=exp(seem.preds[,2])]
  chem.preds[,seem3.u95:=exp(seem.preds[,3])]

# Annotate pathways:
  chem.preds[,Pathway:=""]
  chem.preds[delta.Diet.pred>0.5,Pathway:=paste(Pathway,"Dietary",sep=", ")]
  chem.preds[delta.Res.pred>0.5,Pathway:=paste(Pathway,"Consumer",sep=", ")]
  chem.preds[delta.Pest.pred>0.5,Pathway:=paste(Pathway,"Pesticide",sep=", ")]
  chem.preds[delta.Indust.pred>0.5,Pathway:=paste(Pathway,"Industrial",sep=", ")]
  chem.preds[,Pathway:=substr(Pathway,3,nchar(Pathway))]
  chem.preds[Pathway=="",Pathway:="Unknown"]
  chem.preds[Pathway=="Dietary, Consumer, Pesticide, Industrial",
    Pathway:="All Four"]
  chem.preds[regexpr(",",Pathway)!=-1,
    Pathway:=gsub("Consumer","Cons.",
    gsub("Industrial","Ind.",
    gsub("Dietary","Diet.",
    gsub("Pesticide","Pest.",Pathway))))]
  
# Calculate applicability domain:
  chem.preds[,AD:=1]
  chem.preds[delta.Diet.pred<=0.5 &
             delta.Res.pred<=0.5 &
             delta.Pest.pred<=0.5 &
            delta.Indust.pred<=0.5,AD:=0]
  chem.preds[AD==0, seem3:=NA]
  chem.preds[AD==0, seem3.l95:=NA]
  chem.preds[AD==0, seem3.u95:=NA]

# clean up memory:
  gc()
# Write out the chemical descriptors with intake rates added:
  save(chem.preds, paste(file.stub,"-SEEM3-",Sys.Date(),".RData",sep=""))  

  return(chem.preds)
}