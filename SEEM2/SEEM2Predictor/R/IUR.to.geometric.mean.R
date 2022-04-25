#Convert IUR categories (lbs/year) into kg/day
#John Wambaugh
IUR.to.geometric.mean <- function(x)
{
  if (x == "1 to < 10 million lbs")
    return(exp((log(1*1000000/2.204/365.25)+log(10*1000000/2.204/365.25))/2))
  else if (x == "500,000 to < 1 million lbs")
    return(exp((log(0.5*1000000/2.204/365.25)+log(1*1000000/2.204/365.25))/2))
  else if (x == "< 500,000 lbs")
    return(exp((log(25000/2.204/365.25)+log(0.5*1000000/2.204/365.25))/2))
  else if (x == "10 to < 50 million lbs")
    return(exp((log(10*1000000/2.204/365.25)+log(50*1000000/2.204/365.25))/2))
  else if (x == "50 to < 100 million lbs")
    return(exp((log(50*1000000/2.204/365.25)+log(100*1000000/2.204/365.25))/2))
  else if (x == "100 to < 500 million lbs")
    return(exp((log(100*1000000/2.204/365.25)+log(500*1000000/2.204/365.25))/2))
  else if (x == "500 million to < 1 billion lbs")
    return(exp((log(500*1000000/2.204/365.25)+log(1000*1000000/2.204/365.25))/2))
  else if (x == "1 billion lbs and greater")
    return(5000*1000000/2.204/365.25)
  else print(IUR.HPV[this.row,"CHEM_AGG_P"])
}
