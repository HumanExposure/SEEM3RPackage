library(reshape2)

setwd("C:/Users/jwambaug/git/SEEM3/scripts")
load("chem.preds-2018-11-28.RData")
ring2018 <- chem.preds
load("new.chem.preds-2018-12-19.RData")
new.preds <- chem.preds

chem.preds <- rbind(ring2018,new.preds[,colnames(ring2018)])

chem.preds <- subset(chem.preds,!duplicated(dsstox_substance_id))

chem.preds[,"seem3.AD"] <- apply(chem.preds, 1, function(x)
  ifelse(any(x["delta.Diet.pred"] > 0.5 |
    x["delta.Res.pred"] > 0.5 |
    x["delta.Pest.pred"] > 0.5 |
    x["delta.Indust.pred"] > 0.5), 1, 0))
colnames(chem.preds)[1] <- "DTXSID"    
   
# Total population predictions from SEEM3 consensus model: 
seem3.total.preds <- chem.preds[,c(
  "DTXSID",
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
  "Pred.USETox.Diet"               
  )
  ]
  
# Convert into tall data frame:
seem3.total.melt <- melt(seem3.total.preds)
colnames(seem3.total.melt)[2] <- "Predictor"
colnames(seem3.total.melt)[3] <- "Median"
seem3.total.melt <- subset(seem3.total.melt,!is.na(Median))
seem3.total.melt$Median <- signif(seem3.total.melt$Median,4)
seem3.total.melt$L95 <- NA
seem3.total.melt$U95 <- NA
seem3.total.melt$AD <- 1
seem3.total.melt$Demographic <- "Total"

for (this.pred in unique(seem3.total.melt$Predictor))
{

  seem3.total.melt[seem3.total.melt$Predictor==this.pred,"Units"] <- 
    this.unit
  seem3.total.melt[seem3.total.melt$Predictor==this.pred,"Reference"] <- 
    this.ref
}


seem3.total.consensus <- chem.preds[,c(
  "DTXSID",
  "seem3",                           
  "seem3.l95",
  "seem3.u95",
  "seem3.AD"
  )
  ]
seem3.total.consensus[,2:5] <-
  apply(seem3.total.consensus[,2:5],2,function(x) signif(x,4))
seem3.total.consensus$Demographic <- "Total"
seem3.total.consensus$Predictor <- "SEEM3 Consensus" 
seem3.total.consensus$Reference <- "Ring 2018" 
colnames(seem3.total.consensus)[2:4] <- c("Median","L95","U95","AD")
 
  
  
# General exposure predictors from SEEM3 consensus model: 
seem3.general <- chem.preds[,c(
  "DTXSID",
  "Pred.Production.Volume",           
  "Pred.Stockholm",                  
  "delta.Diet.pred",                  
  "delta.Res.pred",                  
  "delta.Pest.pred",                  
  "delta.Indust.pred"
  )
  ]  
 
  


