#'Retrieve intake rates from Ring et al. (2018)
#'
#'This function find chemical intake rate predictions that provided as
#' supplemental information in Ring et al. (2018). These predictions are from 
#' the SEEM consensus exposure rate model. 
#'
#'@param chem.ids is a vector of DTXSIDs from the CompTox Chemicals Dashboard
#'  
#'@return a list with tow entries: matched and unmatched
#'
#'@details 
#' Matched is a list of intake rates that were in the supplemental information
#' Unmatched contaisn all the id's that were not in the supplemental information
#' of Ring et al. (2018)
#'
#'@examples 
#'
#'@author John Wambaugh
#'
#'@references Ring, Caroline L., et al. 
#' "Consensus modeling of median chemical intake for the US population based on predictions of exposure pathways." 
#' Environmental science & technology 53.2 (2018): 719-732.
#'
#'@export predict_pathways
#'@import data.table, randomForest
lit_intakerate <- function(
  chem.ids
  )
{
#  Ring2018.preds[,AD:=1]
#  Ring2018.preds[delta.Diet.pred<=0.5 &
#             delta.Res.pred<=0.5 &
#             delta.Pest.pred<=0.5 &
#            delta.Indust.pred<=0.5,AD:=0]
#  Ring2018.preds[delta.Diet.pred<=0.5 &
#             delta.Res.pred<=0.5 &
#             delta.Pest.pred<=0.5 &
#            delta.Indust.pred<=0.5,seem3.u95:=NA]
            
  matched <- subset(Ring2018.preds,dsstox_substance_id%in% chem.ids)
  matched <- matched[,c(
    "dsstox_substance_id",
    "CAS",
    "Substance_Name",
    "Pred.SHEDS.Direct",
    "Pred.SHEDS.Indirect",
    "Pred.FINE",
    "Pred.Food.Contact",
    "Pred.REDS",
    "Pred.RAIDAR",
    "Pred.RAIDAR.ICE",
    "Pred.USETox.Pest",
    "Pred.USETox.Indust",
    "Pred.USETox.Res",
    "Pred.USETox.Diet",
    "Pred.Production.Volume",
    "Pred.Stockholm",
    "delta.Diet.pred",
    "delta.Res.pred",
    "delta.Pest.pred",
    "delta.Indust.pred",
    "seem3",
    "seem3.l95",
    "seem3.u95",
    "Rank",
    "Pathway",
    "AD")]  

  unmatched <- sort(unique(
    chem.ids[!(chem.ids %in% matched$dsstox_substance_id)]))

  return(list(
    matched=matched,
    unmatched=unmatched))
}