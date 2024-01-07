#----------------------------------------------------------------------------------------#
#- Function to create RF model error matrix and accuracy metrics in a formatted table   -#
#----------------------------------------------------------------------------------------#
#    A. Shrestha - UIdaho - 2023 - NOTE only use for objects of class 'randomForest'    -#
#----------------------------------------------------------------------------------------#

#' Creates RF model error matrix and accuracy metrics in a formatted table
#'
#' This function takes object of class 'randomForest'.
#'
#' @param randomForestObj An object of class 'randomForest'. For 'train' objects from `caret` package, use '...$finalModel' attribute to call the 'randomForest' final model. 
#' @return Message with the estimate OOB rate and matrix table with confusion matrix and accuracy metrics
#'
#'
#' @export


errorAccuracyOutputs <- function(randomForestObj){
  confusionMatrixTable <- randomForestObj$confusion
  
  # Remove class error column (will calculate and add later)
  confusionMatrixTable <- confusionMatrixTable[,-5]
  
  # transpose confusion matrix 
  
  confusionMatrixTable <- t(confusionMatrixTable)
  
  
  # Calculate user accuracy
  userAcc <- round(diag(confusionMatrixTable) / rowSums(confusionMatrixTable) * 100)
  userAcc <- signif(userAcc, digits = 4)
  
  # Calculate producer accuracy
  prodAcc <- diag(confusionMatrixTable) / colSums(confusionMatrixTable) * 100
  prodAcc <- signif(prodAcc, digits = 4)
  
  # Calculate comission error
  comErr <- 100 - userAcc
  
  # Calculate omission error
  omErr <- 100 - prodAcc
  
  # Calculate overall accuracy
  overallAcc <- sum(diag(confusionMatrixTable)) / sum(confusionMatrixTable) * 100
  overallAcc <- signif(overallAcc, digits = 4)
  
  # Combine accuracy measures into a data frame
  accuracyDF <- data.frame(userAcc, prodAcc, comErr, omErr)
  
  userAcc_comErr <- accuracyDF[,c(1,3)]
  prodAcc_omErr <- accuracyDF[,c(2,4)]
  
  prodAcc_omErr_transpose <- t(prodAcc_omErr)
  userAcc <- c("-", "Overall Accuracy") 
  comErr <- c("-", overallAcc)
  addCols <- cbind(userAcc, comErr)
  prodAcc_omErr_transpose <- cbind(prodAcc_omErr_transpose, addCols)
  
  
  confusionMatrixTable <- cbind(confusionMatrixTable, userAcc_comErr)
  
  confusionMatrixTable <- rbind(confusionMatrixTable, prodAcc_omErr_transpose)
  
  # Assign the new row and column names to the confusion matrix
  rownames(confusionMatrixTable) <- c("green", "gray", "red", "shadow", "Producers Accuracy", "Omission Error")
  colnames(confusionMatrixTable) <- c("green", "gray", "red", "shadow", "Users Accuracy", "Commission Error")
  
  # Add message reporting the OOB estimate of error rate 
  oobErrRate <- round(((randomForestObj$err.rate[nrow(randomForestObj$err.rate), "OOB"])*100), 2)
  message((paste("OOB estimate of  error rate:", oobErrRate, "%")))
  
  return(confusionMatrixTable)
  
}


