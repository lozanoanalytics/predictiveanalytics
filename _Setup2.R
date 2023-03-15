
#==============================================================================
# sources and idea credits: https://grouplens.org/datasets/movielens/
#==============================================================================

projFld <- "C:/Users/ANONYMOUS/Documents/BC/BC_Spring_2019/Predictive_Analytics/Midterm"
RCode <- paste0(projFld, "/RCode")
CleanData <- paste0(projFld, "/CleanData")
RData <- paste0(projFld, "/RData")
RawData <- paste0(projFld, "/RawData")
Output <- paste0(projFld, "/Output")

#==============================================================================
# libraries
#==============================================================================

library(data.table)
library(glmnet)
library(randomForest)
library(readr)
library(pixmap)
library(nnet)
library(e1071)
library(caret)
library(tree)
library(reshape2)
library(gridExtra)
library(raster)
library(stringr)
library(quantmod)
library(Hmisc)
library(fpp2)
library(psych)
library(scales)
library(grid)
library(corrplot)

#==============================================================================
# source some functions
#==============================================================================
fns_list <- c(
  "FN_trim")
for(tfn in fns_list){
  source(paste0(RCode,"/_Functions/",tfn, ".R"))
}
