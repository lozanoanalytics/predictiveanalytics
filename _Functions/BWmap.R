# Scope: only useful for Midterm
# Assumptions:
#   inputData is a data table, with columns named "pixelX"
#   BWthreshold is an integer between 0 and 255
# Output:
#   a copy of the inputData with all "pixel" columns mapped to 0 or 1
#   depending on their value compared to BWthreshold

BWmap <- function(inputData, BWthreshold = 20){

  #==================== begin testing area ===============
  # BWthreshold <- 20 # black-and-white-threshold...
  # inputData <- copy(train)
  #==================== end testing area =================
  
  outputData <- copy(inputData) # copy inputData so it's not affected by transformations here
  
  # take all columns that start with "pixel"
  pcols <- names(outputData)
  pcols <- pcols[substr(pcols,1,5)=="pixel"]
  
  # this will take a bit to run (1') - so print progress to console...
  for(pcol in pcols){
    print(pcol)
    outputData[get(pcol) <= BWthreshold, eval(pcol):=as.integer(0)]
    outputData[get(pcol) > BWthreshold, eval(pcol):=as.integer(1)]
  }
  return(outputData)
}
