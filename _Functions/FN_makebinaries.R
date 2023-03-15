
# start with some data with 10 digits
whichData <- copy(rawTrain)

# also, I need a targetDigit - this will become 1, all the rest will be 0
str(whichData[,label])

# ASSUMPTION: the variable encoding the digits is an integer
targetVariable <- "label"
targetDigit <- 3

# indicate a name for the new variable that will have only 1/0 values
outputVar <- "newlabel"

FN_makebinaries <- function(whichData, targetVariable, targetDigit, outputVar){
  
  # ================  NO NEED TO TOUCH ANYTHING BELOW =======================
  
  
  # identify the records on which the old variable has the "targetDigit" value
  
  # attempt # 1
  # whichData[label == 3, newlabel := 1]
  # whichData[label != 3, newlabel := 0]
  
  # attempt # 2 - replace 3 with a general value (targetDigit)
  # whichData[label == targetDigit, newlabel := 1]
  # whichData[label != targetDigit, newlabel := 0]
  
  # attempt # 3 - replace newlabel with "outputVar" for flexibility
  # whichData[label == targetDigit, (outputVar) := 1]
  # whichData[label != targetDigit, (outputVar) := 0]
  
  # attempt # 4 - replace label with "targetVariable" for flexibility
  x <- copy(whichData) # otherwise this function changes the original dataset, which we may not want...
  x[get(targetVariable) == targetDigit, (outputVar) := 1]
  x[get(targetVariable) != targetDigit, (outputVar) := 0]
  return(x)
}

# test this
tdata <- copy(rawTrain)[1:100,list(label, pixel0)]
# x <- FN_makebinaries(tdata, targetVariable = 'label', targetDigit = 3, outputVar = 'RVw')
# x

# tt <- copy(tdata)
# for(i in c(0:9)){
#   tt <- FN_makebinaries(tt, targetVariable = 'label', targetDigit = i, outputVar = paste0('RV',i))
# }


