#'Calculate the probabality of exposure via various pathways.
#'
#' @description
#'This function calculates uses a precomputed random forest model (Ring et al., 2018)
#'to computer the probabality that chemical exposure occurs via four diferent source-based
#'pathways: industrial, pesticidal, consumer use, and diet. These pathway probabilities
#'are used by the SEEM consensus exposure rate model. 
#'
#'If a chemical is unlikely to have exposure via any of these four pathways, it is considered
#'to be outside the domain of applicability of the Ring et al. (2018) consensus model.
#'
#'@param chem.props a N x ?? table of chemical properties to be predicted.
#'@param file.stub The text prefix added onto the output files (default "new-seem")
#'@param rand.seed The random number generator seed allowing for pseudo-random 
#'number seuqneces to be replicated.
#'  
#'@return SChemical-specific pathway probabilities
#'
#'@author John Wambaugh and Caroline Ring
#'
#'@references Ring, Caroline L., et al. 
#' "Consensus modeling of median chemical intake for the US population based on predictions of exposure pathways." 
#' Environmental science & technology 53.2 (2018): 719-732.
#'
#'@export predict_pathways
#'@import data.table
#'@import randomForest
predict_pathways<- function(
  chem.props,
  file.stub="new-seem",
  rand.seed=123456)
{
  # Git (rightly) balks at files >100MB so we need to rebuild this list:
  pathway.RF <- list(
    Diet=pathway.diet,
    Indust=pathway.indust,
    Pest=pathway.pest,
    Res=pathway.res 
  )
# set the random number generator seed for reproductibility:
  set.seed(rand.seed)

# Make chem.props a data.table
  chem.props <- as.data.table(chem.props)

# These are fixed to match Ring et al. (2018):
  NUM.TREES <- 5000
#  RF.DATE.USED <- "2018-06-04"
  
# Remove this property:
  physchem.props<-physchem.props[physchem.props!="preferred_name"]

# Make sue that we have useable numbers:

  count <- dim(chem.props)[1]
  chem.props <- subset(chem.props,!is.na(as.numeric(AVERAGE_MASS)))
  print(paste(count-dim(chem.props)[1],"chemicals omittted due to no structure."))
  count <- dim(chem.props)[1]
  chem.props <- subset(chem.props,!is.na(as.numeric(OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED)))
  print(paste(count-dim(chem.props)[1],"chemicals omitted due to no OPERA predictions."))
  count <- dim(chem.props)[1]
  chem.props <- subset(chem.props,!is.na(as.numeric(atom.element_main_group)))
  print(paste(count-dim(chem.props)[1],"chemicals omitted due to no ToxPrint chemotypes."))
  
# Rename columns back to older names:
  if ("DTXSID" %in% 
    colnames(chem.props))
  {
    colnames(chem.props)[colnames(chem.props) ==
      "DTXSID"] <- 
      "DSSTox_Substance_Id"
  }                           
  if ("ATMOSPHERIC_HYDROXYLATION_RATE_.AOH._CM3.MOLECULE.SEC_OPERA_PRED" %in% 
    colnames(chem.props))
  {
    colnames(chem.props)[colnames(chem.props) ==
      "ATMOSPHERIC_HYDROXYLATION_RATE_.AOH._CM3.MOLECULE.SEC_OPERA_PRED"] <- 
      "NCCT_AOH"
    chem.props[,NCCT_AOH:=log10(as.numeric(NCCT_AOH))]  
  }                           
  if ("BIOCONCENTRATION_FACTOR_OPERA_PRED" %in% 
    colnames(chem.props))
  {
    colnames(chem.props)[colnames(chem.props) ==
      "BIOCONCENTRATION_FACTOR_OPERA_PRED"] <-  
      "NCCT_BCF"
    chem.props[,NCCT_BCF:=log10(as.numeric(NCCT_BCF))]
  } 
  if ("BIODEGRADATION_HALF_LIFE_DAYS_DAYS_OPERA_PRED" %in% 
    colnames(chem.props))
  {
    colnames(chem.props)[colnames(chem.props) ==
      "BIODEGRADATION_HALF_LIFE_DAYS_DAYS_OPERA_PRED"] <- 
      "NCCT_BIODEG"
    chem.props[,NCCT_BIODEG:=log10(as.numeric(NCCT_BIODEG))]
    chem.props[,NCCT_RBiodeg:=NA]
  } 
  if ("BOILING_POINT_DEGC_OPERA_PRED" %in% 
    colnames(chem.props))
  {
    colnames(chem.props)[colnames(chem.props) ==
      "BOILING_POINT_DEGC_OPERA_PRED"] <- 
      "NCCT_BP"
  } 
  if ("HENRYS_LAW_ATM.M3.MOLE_OPERA_PRED" %in% 
    colnames(chem.props))
  {
    colnames(chem.props)[colnames(chem.props) ==
      "HENRYS_LAW_ATM.M3.MOLE_OPERA_PRED"] <- 
      "NCCT_HL"
    chem.props[,NCCT_HL:=log10(as.numeric(NCCT_HL))]
  } 
  if ("OPERA_KM_DAYS_OPERA_PRED" %in% 
    colnames(chem.props))
  {
    colnames(chem.props)[colnames(chem.props) ==
      "OPERA_KM_DAYS_OPERA_PRED"] <- 
      "NCCT_KM"
    chem.props[,NCCT_KM:=log10(as.numeric(NCCT_KM))]  
  } 
  if ("OCTANOL_AIR_PARTITION_COEFF_LOGKOA_OPERA_PRED" %in% 
    colnames(chem.props))
  {
    colnames(chem.props)[colnames(chem.props) ==
      "OCTANOL_AIR_PARTITION_COEFF_LOGKOA_OPERA_PRED"] <- 
      "NCCT_LogKOA"
  }                     
  if ("SOIL_ADSORPTION_COEFFICIENT_KOC_L.KG_OPERA_PRED" %in% 
    colnames(chem.props))
  {
    colnames(chem.props)[colnames(chem.props) ==
      "SOIL_ADSORPTION_COEFFICIENT_KOC_L.KG_OPERA_PRED"] <- 
      "NCCT_KOC"
    chem.props[,NCCT_KOC:=log10(as.numeric(NCCT_KOC))]  
  }    
  if ("OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED" %in% 
    colnames(chem.props))
  {
    colnames(chem.props)[colnames(chem.props) ==
      "OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED"] <- 
      "NCCT_LogP"
  }       
  if ("MELTING_POINT_DEGC_OPERA_PRED" %in% 
    colnames(chem.props))
  {
    colnames(chem.props)[colnames(chem.props) ==
      "MELTING_POINT_DEGC_OPERA_PRED"] <- 
      "NCCT_MP"
  }  
  if ("VAPOR_PRESSURE_MMHG_OPERA_PRED" %in% 
    colnames(chem.props))
  {
    colnames(chem.props)[colnames(chem.props) ==
      "VAPOR_PRESSURE_MMHG_OPERA_PRED"] <- 
      "NCCT_VP"
    chem.props[,NCCT_VP:=log10(as.numeric(NCCT_VP))]  
  }    
  if ("WATER_SOLUBILITY_MOL.L_OPERA_PRED" %in% 
    colnames(chem.props))
  {
    colnames(chem.props)[colnames(chem.props) ==
      "WATER_SOLUBILITY_MOL.L_OPERA_PRED"] <- 
      "NCCT_WS"
    chem.props[,NCCT_WS:=log10(as.numeric(NCCT_WS))]  
  }   
  if ("AVERAGE_MASS" %in% 
    colnames(chem.props))
  {
    colnames(chem.props)[colnames(chem.props) ==
      "AVERAGE_MASS"] <- 
      "Structure_MolWt"

  }   

  these.cols <- physchem.props[physchem.props %in% colnames(chem.props)]
  chem.props[, (these.cols) := 
    lapply(.SD, as.numeric), 
    .SDcols = these.cols]    
        
  #Calculate Csat,w in mg/L:
  if (all(c("NCCT_WS","Structure_MolWt") %in% colnames(chem.props)))
  {
    chem.props[,NCCT_Csatw := 10^(NCCT_WS)*Structure_MolWt*1000]
  }
  
  #Calculate Kaw:
  if (all(c("NCCT_LogP","NCCT_LogKOA") %in% colnames(chem.props)))
  {
    chem.props[,NCCT_LogKAW := log10((10^NCCT_LogP)/(10^NCCT_LogKOA))]
  }
  
  missing.cols <- physchem.props[!(physchem.props %in% colnames(chem.props))]
  if (length(missing.cols)>0)
  {
    stop(paste("Argument chem.props is missing columns named:",
      paste(missing.cols,collapse=", ")))
  }
  
# Check that properties are on right scale and units by comparing means:
print("Ratio of mean parameters in new set to Ring 2018:")
mean.discrepancy <- apply(chem.props[,.SD,.SDcols=names(Ring2018.means)],
  2,
  function(x) mean(as.numeric(x),na.rm=T)) / 
  Ring2018.means
print(signif(mean.discrepancy,2))
  
  
# Drop chemicals without structures:
  num.before <- dim(chem.props)[1]
  chem.props <- subset(chem.props,!is.na(as.numeric(Structure_MolWt)))
  num.after <- dim(chem.props)[1]
  if (num.before > num.after) warning(paste(num.before-num.after,
    "chemicals dropped for not having structures"))
  
# Scale and Center properties based on 2018 ensemble:

#b <- readRDS("L:/Lab/NCCT_ExpoCast/ExpoCast2018/SEEM3/SEEM3_bayes/all-chems-2018-06-04.Rdata")
#Ring2018.means <- apply(b[,c(3:15,22,765:766)],2,function(x) mean(x,na.rm=T))
#Ring2018.sds <- apply(b[,c(3:15,22,765:766)],2,function(x) sd(x,na.rm=T))
#logicnames <- apply(b[1:10000,],2,function(x) length(unique(x)))
#logic.names <- names(logicnames[logicnames<=2])[1:729]
#save(Ring2018.means,Ring2018.sds,logic.names,file="../data/Ring2018scalecenter.RData")
  for (this.col in physchem.props)
  {
    m <- Ring2018.means[this.col]
    s <- Ring2018.sds[this.col]
    #Use means where there is an NA:
    chem.props[is.na(unlist(chem.props[,this.col,with=F])),this.col] <- m
    chem.props[is.nan(unlist(chem.props[,this.col,with=F])),this.col] <- m
    # Scale and center the phys-chem properties:
    chem.props[,this.col] <- (unlist(chem.props[,this.col,with=F]) - m)/s
  }

# Convert all the logical variables to factors, for random forest.
  
#  chem.props[, (logic.names):=lapply(.SD,
#    function(x) factor(x==T,
#    levels=c(TRUE, FALSE))),
#    .SDcols=logic.names]
#  for (this.col in logic.names) 
#    levels(chem.props[,this.col]) <- levels(chem.props[,this.col])

  rf_vars <- c(structure.desc, physchem.props)
  rf_vars <- rf_vars[!(rf_vars %in% c("COMMON.NAME","preferred_name"))]
  
#pathway.RF <- list()
#pathway.RF[["Diet"]] <- readRDS("../data/delta_Diet_forest_noimpute-_2018-06-04_.Rdata")
#pathway.RF[["Indust"]] <- readRDS("../data/delta_Indust_forest_noimpute-_2018-06-04_.Rdata")
#pathway.RF[["Pest"]] <- readRDS("../data/delta_Pest_forest_noimpute-_2018-06-04_.Rdata")
#pathway.RF[["Res"]] <- readRDS("../data/delta_Res_forest_noimpute-_2018-06-04_.Rdata")
#save(pathway.RF,file="../data/Ring2018PathwayRFModels.RData")
  
  delta.preds<-NULL
  for (this.pathway in PATHWAY.NAMES)
  {
    # clean up memory:
    gc()
    print(paste("Random forest for", this.pathway))
    deltaname <- paste("delta",
      this.pathway,
      sep=".")
    
    # Load each pathway's random forest model:
    this.RF <-   pathway.RF[[this.pathway]]
    
    # Make the preeictions:
    delta.pred <- predict(object=this.RF,
      newdata=chem.props[,
      .SD, 
      .SDcols=rf_vars],
      type="prob")
    rownames(delta.pred) <- chem.props[, DSSTox_Substance_Id]
    
    delta.pred <- data.table(delta.pred, keep.rownames=TRUE)
    delta.pred[,"FALSE":=NULL]
    setnames(delta.pred, 
      c("rn", "TRUE"),
      c("DSSTox_Substance_Id", paste("delta",
        this.pathway,
        "pred",
        sep=".")))
    
    #Add OOB error rate
    delta.pred[, (paste("delta",
                        this.pathway,
                        "OOB.err",
                        sep=".")):=this.RF$err.rate[NUM.TREES, "OOB"]]
    if (!is.null(delta.preds)) 
      delta.preds<-merge(
        delta.preds,
        delta.pred,
        by="DSSTox_Substance_Id",
        allow.cartesian=TRUE)
    else delta.preds <- delta.pred
  }
  chem.props  <-merge(chem.props,
    delta.preds,by="DSSTox_Substance_Id",allow.cartesian=TRUE)

# clean up memory:
  gc()
# Write out the chemical descriptors wtih pathway predictions added:
  save(chem.props, file=paste(file.stub,"-PathwayDeltas-",Sys.Date(),".RData",sep=""))  

  return(chem.props)
}