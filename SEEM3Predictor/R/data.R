#This file is used by roxygen2 to generate man files (documentation) for data
#sets included in the package.

#' randomForest Model for Dietary Pathway
#'
#' This model estimates the probability that a chemical has human exposure 
#' from a dietary source. To train the random forest algorithm for each pathway, 
#' we chose sets of chemicals that might reasonably serve as "positive"
#' or "negative" examples of that pathway, as summarized in the table below. 
#' The number of positive and negative chemicals was balanced
#' by randomly sampling a subset from the larger of the two sets.
#' Training Sets and Performance of Random Forest Models for Exposure Pathways
#'
#' \tabular{lllllllll}{
#' NHANES \tab Chemicals \tab Positives \tab Negatives \tab OOB Error Rate \tab 
#' Positives error rate \tab Balanced Accuracy \tab 
#' Sources of Positive Example Chemicals \tab
#' Sources of Negative Example Chemicals \cr
#' dietary \tab 24 \tab 2523 \tab 886 \tab5 27 \tab 32 \tab 73 \tab 
#' FDA CEDI, ExpoCast, CPDat (Food, Food Additive, Food Contact), 
#' NHANES Curation \tab
#' Pharmapendium, CPDat (nonfood), NHANES Curation \cr
#' consumer \tab 49 \tab 1622 \tab 567 \tab 26 \tab 24 \tab 74 \tab
#'  CPDat (consumer_use, building_material), ExpoCast, NHANES Curation \tab 
#'  CPDat (Agricultural, Industrial), FDA CEDI, NHANES Curation \cr
#' far-field pesticide sources \tab 
#' 94 \tab 1480 \tab 6522 \tab 21 \tab 36 80 \tab
#'  REDs, Swiss Pesticides, Stockholm Convention, CPDat (Pesticide), NHANES
#' Curation \tab
#' Pharmapendium, Industrial Positives, NHANES Curation \cr
#' far field industrial sources \tab 
#' 42 \tab 5089 \tab 2913 \tab 19 \tab 16 \tab 81 \tab 
#' CDR HPV, USGS Water Occurrence, NORMAN PFAS, Stockholm Convention,
#' CPDat (Industrial, Industrial_Fluid), NHANES Curation \tab
#' Pharmapendium, Pesticide Positives, NHANES Curation \cr
#' }
#'
#' @docType data
#' @author John Wambaugh
#' @references Ring, Caroline L., et al. 
#' "Consensus modeling of median chemical intake for the US population based on predictions of exposure pathways." 
#' Environmental science & technology 53.2 (2018): 719-732.
"pathway.diet"

#' randomForest Model for Far-Field Industrial Pathway
#'
#' This model estimates the probability that a chemical has human exposure 
#' from a far-field industrial source. To train the random forest algorithm for each pathway, 
#' we chose sets of chemicals that might reasonably serve as "positive"
#' or "negative" examples of that pathway, as summarized in the table below. 
#' The number of positive and negative chemicals was balanced
#' by randomly sampling a subset from the larger of the two sets.
#'
#' Training Sets and Performance of Random Forest Models for Exposure Pathways
#' \tabular{lllllllll}{
#' NHANES \tab Chemicals \tab Positives \tab Negatives \tab OOB Error Rate \tab 
#' Positives error rate \tab Balanced Accuracy \tab 
#' Sources of Positive Example Chemicals \tab
#' Sources of Negative Example Chemicals \cr
#' dietary \tab 24 \tab 2523 \tab 886 \tab5 27 \tab 32 \tab 73 \tab 
#' FDA CEDI, ExpoCast, CPDat (Food, Food Additive, Food Contact), 
#' NHANES Curation \tab
#' Pharmapendium, CPDat (nonfood), NHANES Curation \cr
#' consumer \tab 49 \tab 1622 \tab 567 \tab 26 \tab 24 \tab 74 \tab
#'  CPDat (consumer_use, building_material), ExpoCast, NHANES Curation \tab 
#'  CPDat (Agricultural, Industrial), FDA CEDI, NHANES Curation \cr
#' far-field pesticide sources \tab 
#' 94 \tab 1480 \tab 6522 \tab 21 \tab 36 80 \tab
#'  REDs, Swiss Pesticides, Stockholm Convention, CPDat (Pesticide), NHANES
#' Curation \tab
#' Pharmapendium, Industrial Positives, NHANES Curation \cr
#' far field industrial sources \tab 
#' 42 \tab 5089 \tab 2913 \tab 19 \tab 16 \tab 81 \tab 
#' CDR HPV, USGS Water Occurrence, NORMAN PFAS, Stockholm Convention,
#' CPDat (Industrial, Industrial_Fluid), NHANES Curation \tab
#' Pharmapendium, Pesticide Positives, NHANES Curation \cr
#' }
#'
#' @docType data
#' @author John Wambaugh
#' @references Ring, Caroline L., et al. 
#' "Consensus modeling of median chemical intake for the US population based on predictions of exposure pathways." 
#' Environmental science & technology 53.2 (2018): 719-732.
"pathway.indust"

#' randomForest Model for Far-Field Pesticidal Pathway
#'
#' This model estimates the probability that a chemical has human exposure 
#' from a far-field pesticidal source. To train the random forest algorithm for each pathway, 
#' we chose sets of chemicals that might reasonably serve as "positive"
#' or "negative" examples of that pathway, as summarized in the table below. 
#' The number of positive and negative chemicals was balanced
#' by randomly sampling a subset from the larger of the two sets.
#'
#' Training Sets and Performance of Random Forest Models for Exposure Pathways
#' \tabular{lllllllll}{
#' NHANES \tab Chemicals \tab Positives \tab Negatives \tab OOB Error Rate \tab 
#' Positives error rate \tab Balanced Accuracy \tab 
#' Sources of Positive Example Chemicals \tab
#' Sources of Negative Example Chemicals \cr
#' dietary \tab 24 \tab 2523 \tab 886 \tab5 27 \tab 32 \tab 73 \tab 
#' FDA CEDI, ExpoCast, CPDat (Food, Food Additive, Food Contact), 
#' NHANES Curation \tab
#' Pharmapendium, CPDat (nonfood), NHANES Curation \cr
#' consumer \tab 49 \tab 1622 \tab 567 \tab 26 \tab 24 \tab 74 \tab
#'  CPDat (consumer_use, building_material), ExpoCast, NHANES Curation \tab 
#'  CPDat (Agricultural, Industrial), FDA CEDI, NHANES Curation \cr
#' far-field pesticide sources \tab 
#' 94 \tab 1480 \tab 6522 \tab 21 \tab 36 80 \tab
#'  REDs, Swiss Pesticides, Stockholm Convention, CPDat (Pesticide), NHANES
#' Curation \tab
#' Pharmapendium, Industrial Positives, NHANES Curation \cr
#' far field industrial sources \tab 
#' 42 \tab 5089 \tab 2913 \tab 19 \tab 16 \tab 81 \tab 
#' CDR HPV, USGS Water Occurrence, NORMAN PFAS, Stockholm Convention,
#' CPDat (Industrial, Industrial_Fluid), NHANES Curation \tab
#' Pharmapendium, Pesticide Positives, NHANES Curation \cr
#' }
#'
#' @docType data
#' @author John Wambaugh
#' @references Ring, Caroline L., et al. 
#' "Consensus modeling of median chemical intake for the US population based on predictions of exposure pathways." 
#' Environmental science & technology 53.2 (2018): 719-732.
"pathway.pest"

#' randomForest Model for Nar-Field (Residential) Pathway
#'
#' This model estimates the probability that a chemical has human exposure 
#' from a near-field household product source. To train the random forest algorithm for each pathway, 
#' we chose sets of chemicals that might reasonably serve as "positive"
#' or "negative" examples of that pathway, as summarized in the table below. 
#' The number of positive and negative chemicals was balanced
#' by randomly sampling a subset from the larger of the two sets.
#'
#' Training Sets and Performance of Random Forest Models for Exposure Pathways
#' \tabular{lllllllll}{
#' NHANES \tab Chemicals \tab Positives \tab Negatives \tab OOB Error Rate \tab 
#' Positives error rate \tab Balanced Accuracy \tab 
#' Sources of Positive Example Chemicals \tab
#' Sources of Negative Example Chemicals \cr
#' dietary \tab 24 \tab 2523 \tab 886 \tab5 27 \tab 32 \tab 73 \tab 
#' FDA CEDI, ExpoCast, CPDat (Food, Food Additive, Food Contact), 
#' NHANES Curation \tab
#' Pharmapendium, CPDat (nonfood), NHANES Curation \cr
#' consumer \tab 49 \tab 1622 \tab 567 \tab 26 \tab 24 \tab 74 \tab
#'  CPDat (consumer_use, building_material), ExpoCast, NHANES Curation \tab 
#'  CPDat (Agricultural, Industrial), FDA CEDI, NHANES Curation \cr
#' far-field pesticide sources \tab 
#' 94 \tab 1480 \tab 6522 \tab 21 \tab 36 80 \tab
#'  REDs, Swiss Pesticides, Stockholm Convention, CPDat (Pesticide), NHANES
#' Curation \tab
#' Pharmapendium, Industrial Positives, NHANES Curation \cr
#' far field industrial sources \tab 
#' 42 \tab 5089 \tab 2913 \tab 19 \tab 16 \tab 81 \tab 
#' CDR HPV, USGS Water Occurrence, NORMAN PFAS, Stockholm Convention,
#' CPDat (Industrial, Industrial_Fluid), NHANES Curation \tab
#' Pharmapendium, Pesticide Positives, NHANES Curation \cr
#' }
#'
#' @docType data
#' @author John Wambaugh
#' @references Ring, Caroline L., et al. 
#' "Consensus modeling of median chemical intake for the US population based on predictions of exposure pathways." 
#' Environmental science & technology 53.2 (2018): 719-732.
"pathway.res"