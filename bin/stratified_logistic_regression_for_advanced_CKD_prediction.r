setwd(".")
options(stringsAsFactors = FALSE)
thisSeed <- 10
set.seed(thisSeed)
cat("set.seed(", thisSeed, ")\n", sep="")

# Survival analysis and the stratified sample:
# https://towardsdatascience.com/survival-analysis-and-the-stratified-sample-2c2582aa9805

list.of.packages <- c("easypackages", "survival",  "dplyr", "survminer", "stats", "PRROC", "formula.tools", "dplyr", "pastecs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

library("easypackages")
libraries(list.of.packages)

source("./confusion_matrix_rates.r")
source("./utils.r")

NUM_METRICS <- 9
confMatDataFrame <- matrix(ncol=NUM_METRICS, nrow=1)
colnames(confMatDataFrame) <- c("MCC", "F1 score", "accuracy", "TP_rate", "TN_rate", "PPV", "NPV", "PR_AUC", "ROC_AUC")

fileNameData <- "../data/journal.pone.0199920.s002_EDITED.csv"
targetName <- "EventCKD35"
patients_data <- read.csv(fileNameData, header = TRUE, sep =",")
cat("Read data from file ", fileNameData, "\n", sep="")


# let's remove the features with missing data
patients_data$"TriglyceridesBaseline" <- NULL
patients_data$"HgbA1C" <- NULL
patients_data$"Age.3.categories" <- NULL
patients_data$"TimeToEventMonths" <- NULL

patients_data$"TIME_YEAR" <-  factor(patients_data$"TIME_YEAR")

# let's put the target label last on the right 
patients_data <- patients_data%>%select(-targetName,targetName)

target_index <- dim(patients_data)[2]


execution_number <- 100
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{

  cat("exe = ", exe_i, "\n", sep="")

  # shuffle the rows
  patients_data <- patients_data[sample(nrow(patients_data)),] 

  num_to_return <- 1
  upper_num_limit <- 10000000
  exe_num <- sample(1:upper_num_limit, num_to_return)


  cat("nrow(patients_data) = ", nrow(patients_data), "\n", sep="")
  cat("ncol(patients_data) = ", ncol(patients_data), "\n", sep="")


  # split training set and test set
  training_set_perce <- 80
  cat("training_set_perce = ", training_set_perce, "%\n", sep="")

  # the training set is the first training_set_perce% of the whole dataset
  training_set_first_index <- 1 # NEW
  training_set_last_index <- round(dim(patients_data)[1]*training_set_perce/100) # NEW

  # the test set is the last 20% of the whole dataset
  test_set_first_index <- round(dim(patients_data)[1]*training_set_perce/100)+1 # NEW
  test_set_last_index <- dim(patients_data)[1] # NEW

  cat("[Creating the subsets for the values]\n")
  prc_data_train <- patients_data[training_set_first_index:training_set_last_index, ] 
  prc_data_test <- patients_data[test_set_first_index:test_set_last_index, ] 

  prc_data_train_labels <- prc_data_train[, target_index] 
  prc_data_test_labels <- prc_data_test[, target_index] 

  glm_model <- glm(EventCKD35 ~ Sex + AgeBaseline + HistoryDiabetes + HistoryCHD + HistoryVascular + HistorySmoking + HistoryHTN + HistoryDLD + HistoryObesity + DLDmeds + DMmeds + HTNmeds + ACEIARB + CholesterolBaseline   + CreatinineBaseline + eGFRBaseline + sBPBaseline + dBPBaseline + BMIBaseline + (TIME_YEAR), data = prc_data_train, family = "binomial")

  prc_data_test_pred <- predict(glm_model, prc_data_test, type = "response")
  threshold <- 0.5
  prc_data_test_pred_bin <- as.numeric(prc_data_test_pred)

  prc_data_test_pred_bin[prc_data_test_pred_bin>=threshold]<-1
  prc_data_test_pred_bin[prc_data_test_pred_bin<threshold]<-0
  
  thisConfMat <- confusion_matrix_rates(prc_data_test_pred_bin, prc_data_test_labels, "@@@ Test set @@@")
    
  if (exe_i == 1)  confMatDataFrame <-  thisConfMat
  else  confMatDataFrame <- rbind(confMatDataFrame, thisConfMat)

}


cat("\n\n\n=== final results ===\n")
 
cat("Number of executions = ", execution_number, "\n", sep="")
 # statistics on the dataframe of confusion matrices
statDescConfMatr <- stat.desc(confMatDataFrame)

cat("\n\n=== === === ===\n")
print(dec_three(statDescConfMatr))
meanSdRowResults <- (statDescConfMatr)[c("mean", "std.dev"),]
cat("\n\n=== === === ===\n")
print(dec_three(meanSdRowResults))
cat("\n\n=== === === ===\n")