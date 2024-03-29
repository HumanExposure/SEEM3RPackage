% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{pathway.indust}
\alias{pathway.indust}
\title{randomForest Model for Far-Field Industrial Pathway}
\format{
An object of class \code{randomForest} of length 18.
}
\usage{
pathway.indust
}
\description{
This model estimates the probability that a chemical has human exposure 
from a far-field industrial source. To train the random forest algorithm for each pathway, 
we chose sets of chemicals that might reasonably serve as "positive"
or "negative" examples of that pathway, as summarized in the table below. 
The number of positive and negative chemicals was balanced
by randomly sampling a subset from the larger of the two sets.
}
\details{
Training Sets and Performance of Random Forest Models for Exposure Pathways
\tabular{lllllllll}{
NHANES \tab Chemicals \tab Positives \tab Negatives \tab OOB Error Rate \tab 
Positives error rate \tab Balanced Accuracy \tab 
Sources of Positive Example Chemicals \tab
Sources of Negative Example Chemicals \cr
dietary \tab 24 \tab 2523 \tab 886 \tab5 27 \tab 32 \tab 73 \tab 
FDA CEDI, ExpoCast, CPDat (Food, Food Additive, Food Contact), 
NHANES Curation \tab
Pharmapendium, CPDat (nonfood), NHANES Curation \cr
consumer \tab 49 \tab 1622 \tab 567 \tab 26 \tab 24 \tab 74 \tab
 CPDat (consumer_use, building_material), ExpoCast, NHANES Curation \tab 
 CPDat (Agricultural, Industrial), FDA CEDI, NHANES Curation \cr
far-field pesticide sources \tab 
94 \tab 1480 \tab 6522 \tab 21 \tab 36 80 \tab
 REDs, Swiss Pesticides, Stockholm Convention, CPDat (Pesticide), NHANES
Curation \tab
Pharmapendium, Industrial Positives, NHANES Curation \cr
far field industrial sources \tab 
42 \tab 5089 \tab 2913 \tab 19 \tab 16 \tab 81 \tab 
CDR HPV, USGS Water Occurrence, NORMAN PFAS, Stockholm Convention,
CPDat (Industrial, Industrial_Fluid), NHANES Curation \tab
Pharmapendium, Pesticide Positives, NHANES Curation \cr
}
}
\references{
Ring, Caroline L., et al. 
"Consensus modeling of median chemical intake for the US population based on predictions of exposure pathways." 
Environmental science & technology 53.2 (2018): 719-732.
}
\author{
John Wambaugh
}
\keyword{datasets}
