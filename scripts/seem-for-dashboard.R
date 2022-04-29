library(reshape2)
library(readxl)

setwd("C:/Users/jwambaug/git/SEEM3/scripts")
load("chem.preds-2018-11-28.RData")
ring2018 <- chem.preds
load("new.chem.preds-2018-12-19.RData")
new.preds <- chem.preds

chem.preds <- rbind(ring2018,new.preds[,colnames(ring2018)])

chem.preds <- subset(chem.preds,!duplicated(dsstox_substance_id))

# any chemical that does not have >50% predicted probability of exposure by 
# SEEM3 pathways is outside the domain of applicability
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

# Convert the Predictor labels to text:
seem3.total.melt$Predictor <- as.character(seem3.total.melt$Predictor)
seem3.total.melt$Predictor <- gsub("Pred.","",seem3.total.melt$Predictor)

seem3.predictors <- as.data.frame(read_excel("seem3-predictors.xlsx"))

for (this.pred in seem3.predictors$Label)
{
  this.row <- which(seem3.predictors$Label == this.pred)
  this.unit <- seem3.predictors[this.row,"Units"]
  this.ref <- seem3.predictors[this.row,"Reference"]

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
colnames(seem3.total.consensus)[2:5] <- c("Median","L95","U95","AD")
seem3.total.consensus$Units <- "mg/kg/day"  
  
load("HPreds.RData")

seem2.demos <- NULL
for (this.demo in names(HPreds))
 if (this.demo != "Total")
{
  this.preds <- as.data.frame(HPreds[[this.demo]][,c("50%", "2.5%", "97.5%")])
  colnames(this.preds) <- c("Median","L95","U95")
  this.preds$DTXSID <- rownames(this.preds)
  if (this.demo == "ReproAgeFemale") {
    demo.text <- "Repro. Age Females"
  } else if (this.demo == "Male") {
    demo.text <- "Males"
  } else if (this.demo == "Female") {
    demo.text <- "Females"
  } else if (this.demo == "6-11_years") {
    demo.text <- "Age 6-11"
  } else if (this.demo == "12-19_years") {
    demo.text <- "Age 12-19"
  } else if (this.demo == "20-65_years") {
    demo.text <- "Age 20-65"
  } else if (this.demo == "66+years") {
    demo.text <- "Age 66+"
  } else if (this.demo == "BMI_GT_30") {
    demo.text <- "BMI > 30"
  } else if (this.demo == "BMI_LE_30") {
    demo.text <- "BMI <= 30"
  }
  this.preds$Demographic <- demo.text
  seem2.demos <- rbind(seem2.demos, this.preds)
} 
seem2.demos$Predictor <- "SEEM2 Heuristic"
seem2.demos$Reference <- "Wambaugh 2014"
seem2.demos$AD <- 1
seem2.demos$Units <- "mg/kg/day"

# Standardize the order for the demographic-specific predictions:
col.order <- c(
  "DTXSID",
  "Demographic",
  "Predictor",
  "Median",
  "L95",
  "U95",
  "Units",
  "AD",
  "Reference")

# Make master table:  
seem.demos <- rbind(
  seem3.total.consensus[,col.order],
  seem2.demos[,col.order],
  seem3.total.melt[,col.order])

# Create a tab separated text file:
write.table(seem.demos,
  file=paste("Demographic-Exposure-Predictions-",Sys.Date(),".txt",sep=""),
  row.names=FALSE,
  sep="\t")
  
    
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

colnames(seem3.general) <- c(
  "DTXSID",
  "Production Volume",           
  "Stockholm Convention",                  
  "Probability Dietary",                  
  "Probability Residential",                  
  "Probability Far-Field Pesticde",                  
  "Probability Industrial"
  )

for (this.col in c(2,4:7))
  seem3.general[,this.col] <- signif(seem3.general[,this.col],3)
   
seem3.general$Units <- "kg/day"

# Create a tab separated text file:
write.table(seem3.general[,c("DTXSID",
  "Production Volume",
  "Units",
  "Stockholm Convention",
  "Probability Dietary",
  "Probability Residential",
  "Probability Far-Field Pesticde",
  "Probability Industrial")],
  file=paste("General-Exposure-Predictions-",Sys.Date(),".txt",sep=""),
  row.names=FALSE,
  sep="\t")

