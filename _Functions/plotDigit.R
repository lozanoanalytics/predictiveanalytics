plotDigit <- function(dtin, rowNo){
  # select the row number indicated from the Train data
  x1 <- dtin[rowNo,]
  # transform data in "long" format
  # detect id vars - all except "pixelX"
  
  idvars <- names(x1)
  idvars <- idvars[!grepl('pixel', idvars)]
  # print(idvars)
  
  # wide to long format transformation
  x2 <- melt(x1,id.vars = idvars)
  
  # transform "pixelXYZ" into XYZ (integer) - prep extract position
  x2[,position:=as.integer(gsub('pixel','',variable))]
  
  # extract position coordinates
  x2[,xvar:=floor(position/28)]
  x2[,yvar:=position-28*xvar]
  x2[,xvar:=28-xvar]
  # x2[,yvar:=28-yvar]
  # print label information - what SHOULD be plotted
  print(paste0('This should look like a... ',unique(x2[,label])))
  # finally, plot digit - https://stackoverflow.com/questions/39859438/getting-geom-tile-to-draw-square-rather-than-rectangular-cells/39859565
  x3 <- ggplot(x2, aes(x=yvar,y=xvar,fill=value)) + geom_tile(color='black')
  ggplotGrob(x3)
}
