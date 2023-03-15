# Scope: only useful for Midterm
# Assumptions:
#   inputData is a data table, with columns named "pixelX"
#   whichDigits is a vector of digits for which sample images are to be printed
# Depends on:
#   plotDigit - function already saved in _Functions (download if not)
# Output:
#   a copy of the inputData with all "pixel" columns mapped to 0 or 1
#   depending on their value compared to BWthreshold

plotSampleDigits <- function(inputData, whichDigits = c(0:9)){
  #==================== begin testing area ===============
  # whichDigits <- c(0:9) # black-and-white-threshold...
  # inputData <- copy(train)
  #==================== end testing area =================
  
  par(mfrow=c(3,3))
  for(whichLabel in whichDigits){
    srows <- head(inputData[label==whichLabel,],9)
    tlist <- lapply(c(1:9), function(x) plotDigit(srows,x))
    do.call("grid.arrange",tlist)
  }
}
