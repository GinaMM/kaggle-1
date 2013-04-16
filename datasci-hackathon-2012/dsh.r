
#Read the data:
TrainingData = read.csv("TrainingData.csv")
SubmissionZeros = read.csv("SubmissionZerosExceptNAs.csv")

names(TrainingData)

#Convenient to have a way to refer to target variables:
target_names = names(SubmissionZeros)[6:44]
target_names

#function for putting -1000000's wherever they appear in the sample submission (I'm sorry this is necessary)
replaceNAs = function(submission, sample) {
  submission = submission[,names(sample)]
  stopifnot(mean(names(submission) == names(sample))==1)
  stopifnot(dim(submission) == dim(sample))
  submission = submission[order(submission$rowID),]
  submission[sample == -1000000] = -1000000
  submission
}


######### CAPONE 01 ##############
#1: time series for each IV
for (iv in TrainingData) {
 ##Generate TS model
 iv.ts <- zoo(iv, ?, format=?))
 ## Compute Predictions
 #pred <- predict(model_3,n.ahead=5,se.fit=T)
}

#2: {IV, Index} --> DV Prediction
## Fill in missing IV values in training data using above generated models
for (iv in TrainingData) {
  if is.na(iv) {
    #pred <- predict(model_3,n.ahead=5,se.fit=T)
  }
}
## Generate model over Training data (over 80% of it say?)
### Some options (lm, nnet, svm?)
# model <- lm(...)


#3: test offline
## given pred. data rows [a:b]
evaluate <- function(trainingSet, predictedSet, rowIndex) {
  #calc MAE over chunks
  error = abs(trainingSet[(rowIndex:),] - predictedSet)
  avgErrors = apply(error, 1, mean, na.rm=FALSE)	
  mae = mean(avgErrors, na.rm=FALSE)
 }

#4: create final upload file -- get predictedSet over Test-Data
##Create slim submission skeleton for merging onto
submission_skeleton = SubmissionZeros[,1:5]
capone_submission = merge(submission_skeleton, predictedSet)
capone_submission_NAs = replaceNAs(capone_submission, SubmissionZeros)
write.csv(capone_submission_NAs, "./upload/capone_submission_01.csv", row.names = FALSE)
