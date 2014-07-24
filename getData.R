getData <- function(directory)
{
  CustomerSet <- read.csv2(directory, colClasses = "character", header = TRUE,strip.white=TRUE)
  colnames(CustomerSet) <- c('cif','total','sector','amount')  
  CustomerSet <- subset(CustomerSet,sector!='Internal (no class.)')
  CustomerSet <- subset(CustomerSet,sector!='Non-classified institutions')  
  CustomerSet$cif <- as.numeric(CustomerSet$cif)
  CustomerSet$amount <- as.numeric(CustomerSet$amount)
  CustomerSet$total <- as.numeric(CustomerSet$total)
  CustomerSet$pct <- CustomerSet$amount/CustomerSet$total
   
  return (data.frame(CustomerSet))
  
  #sectors <- sort(unique(CustomerSet$sector))
}
