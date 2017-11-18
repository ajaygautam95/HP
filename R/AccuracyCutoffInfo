#' AccuracyCutoffInfo function: 
#'Obtain the accuracy on the trainining and testing dataset.
# for cutoff value ranging from .4 to .8 ( with a .05 increase )
#' @keywords AccuracyCutoffInfo
#' @export
#' @examples 
#' AccuracyCutoffInfo()
#' 
#' train   : your data.table or data.frame type training data ( assumes you have the predicted score in it ).
#' test    : your data.table or data.frame type testing data
#' predict : prediction's column name (assumes the same for training and testing set)
#' actual  : actual results' column name
#' returns  : 1. data : a data.table with three columns.
#'            		   each row indicates the cutoff value and the accuracy for the 
#'            		   train and test set respectively.
#' 			 2. plot : plot that visualizes the data.table

library(sqldf)
library(ROCR)
library(grid)
library(caret)
library(dplyr)
library(scales)
library(ggplot2)
library(gridExtra)
library(data.table)
library(tidyr)
AccuracyCutoffInfo <- function( train, test, predict, actual )
{
  # change the cutoff value's range as you please 
  cutoff <- seq( .4, .8, by = .05 )
  
  accuracy <- lapply( cutoff, function(c)
  {
    # use the confusionMatrix from the caret package
    cm_train <- confusionMatrix( as.numeric( train[[predict]] > c ), train[[actual]] )
    cm_test  <- confusionMatrix( as.numeric( test[[predict]]  > c ), test[[actual]]  )
    
    dt <- data.table( cutoff = c,
                      train  = cm_train$overall[["Accuracy"]],
                      test   = cm_test$overall[["Accuracy"]] )
    return(dt)
  })%>% rbindlist()
  
  # visualize the accuracy of the train and test set for different cutoff value 
  # accuracy in percentage.
  accuracy_long <- gather( accuracy, "data", "accuracy", -1 )
  
  plot <- ggplot( accuracy_long, aes( cutoff, accuracy, group = data, color = data ) ) + 
    geom_line( size = 1 ) + geom_point( size = 3 ) +
    scale_y_continuous( label = percent ) +
    ggtitle( "Train/Test Accuracy for Different Cutoff" )
  
  return( list( data = accuracy, plot = plot ) )
}
