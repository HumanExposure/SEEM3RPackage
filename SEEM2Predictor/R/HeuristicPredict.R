HeuristicPredict <-
    function(subgroup=c("All","Total","ReproAgeFemale","Male","Female",
                        "6-11_years","12-19_years","20-65_years","66+years",
                        "BMI_GT_30", "BMI_LE_30"),
             ConsumerAndIndustrial=NULL, IndustrialNoConsumer=NULL, PrVol=NULL,
             PesticideActive=NULL, PesticideInert=NULL, MolWt=NULL,SEED=42) 
    {
        set.seed(SEED)
        x <- as.numeric(c(ConsumerAndIndustrial, IndustrialNoConsumer,
                          log(PrVol), PesticideActive, PesticideInert))
        if (!all(x[-3] %in% c(0,1))) stop("Non 0-1 variable present")
        x <- matrix(c(1,x), ncol=1)
        ## Multiply final result by 1e-6 * MolWt
        ## The final result for group gr is exp(B[[gr]] %*% x) * 1e-6 * MolWt
        tmp <- do.call("rbind",lapply(subgroup, function(z) {
            eps <- rnorm(length(sd_V[[z]]), mean=0, sd=sd_V[[z]])
            quantile(exp(B[[z]] %*% x + eps) * 1e-6 * MolWt,
                     pr=c(0.025, 0.05, 0.5, 0.95, 0.975))
        }))
        rownames(tmp) <- subgroup
        tmp
    }
