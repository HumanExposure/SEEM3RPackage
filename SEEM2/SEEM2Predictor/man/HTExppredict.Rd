\name{HTExppredict}
\alias{HTExppredict}
\title{
  Function to Calculate High Throughput Exposure Estimates
}
\description{
Wambaugh et al. (2014) describe a simple model to coarsely predict population
median exposures to chemicals, based on four characteristics of chemical
use and production volume. This function returns exposure estimates
developed in that paper. It can be used in two modes. In the first, if
a DSSTox Substance ID is provided for which estimates are available,
precomputed estimates are retrieved from internal data. These estimates
correspond to values supplied in supplemental materials for Wambaugh et
al. (2014). If the DSS Tox Substance ID is not provided, and production
volume and the four use indicators are provided, the exposure is
computed from posterior regression coefficient estimates. If molecular
weight is supplied, then exposure is returned in units of mg/kg body
weight/day, otherwise, exposire is returned in units of nm/kg body
weight/day.
}
\usage{
HTExppredict(subgroup = c("All", "Total", "ReproAgeFemale", "Male",
"Female", "6-11_years", "12-19_years", "20-65_years", "66+years",
"BMI_GT_30", "BMI_LE_30"), DSSToxID = "", ConsumerAndIndustrial = NULL,
IndustrialNoConsumer = NULL, PrVol = NULL, PesticideActive = NULL,
PesticideInert = NULL, MolWt = NULL, SEED = 42)
}

\arguments{
  \item{subgroup}{
    Population subgroup for which estimates are desired. "Total" is for
    the whole population, "ReproAgeFemale" is for women between the ages
    of 16 and 49 years old, "Male" and "Female" are self-evident, as are
    the age categories "6-11_years", "12-19_years", "20-65_years", and
    "66+years". "BMI_GT_30" and "BMI_LE_30" indicates people with body
    mass index greater than 30 and less than or equal to 30, respectively
    (a frequently used cutoff for obesity). Finally, including "All"
    returns estimates for all these subgroups.
}
  \item{DSSToxID}{
    DSSTox Substance ID for the chemical, if precomputed values are
    desired. If the desired chemical is one of 106 NHANES chemicals for
    which exposures have been more directly estimated, those values are
    returned. Otherwise, values from Wambaugh et al, 2014 are
    returned. If this value is "", then exposure predictions are derived
    de novo using information from the other arguments, which must not
    be missing in this case.
}
  \item{ConsumerAndIndustrial}{
    chemical substances in consumer products (e.g. toys, personal care
    products, clothes, furniture, and home-care products) that are also
    used in industrial manufacturing processes. Does not include food or
    pharmaceuticals. 
}
  \item{IndustrialNoConsumer}{
    chemical substances and products in industrial manufacturing
    processes that are not used in consumer products. Does not include
    food or pharmaceuticals.
}
  \item{PrVol}{
    Mass of the chemical produced, in kilograms / day.
}
  \item{PesticideActive}{
    active ingredients in products designed to prevent, destroy, repel,
    or reduce pests (e.g., insect repellants, weed killers, and
    disinfectants).
}
  \item{PesticideInert}{
    secondary (i.e., nonactive) ingredients in a pesticide which serve a
    purpose other than repelling pests. Pesticide use of these
    ingredients is known due to more stringent reporting standards for
    pesticide ingredients, but many of these chemicals appear to be also
    used in consumer products.
}
  \item{MolWt}{
    Molecular weight of the chemical. If missing, results are returned
    as dosages of nm/kg body weight/day. If supplied, results are
    returned as dosages of mg/kg body weight/day.
}
  \item{SEED}{
    seed for random number generator. 
}
}
\details{
  The returned quantiles depend on some random number generation when
  DSS Tox Substance ID is not provided. To ensure replicability, set and
  record the random number seed (argument SEED). When computing
  exposures, this functions sets R's random number seed. If this 
}
\value{
  a matrix with columns for 2.5\%, 5\%, 50\%, 95\%, and 97.5\%-iles of
  the posterior prediction for population median dose, and a row for
  each population subgroup requested.
}
\references{
  Wambaugh JF, Wang A, Dionisio KL, Frame A, Egeghy P, Judson R, and
  Setzer RW. 2014. High Throughput Heuristics for Prioritizing Human
  Exposure to Environmental Chemicals. Environmental Science and
  Technology 48: 12760-12767.
}
\author{
  R. Woodrow Setzer <setzer.woodrow@epa.gov>
}
