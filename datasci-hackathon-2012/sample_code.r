setwd('~/dsh/')

library(reshape)
library(plyr)

#Read the data:
TrainingData = read.csv("TrainingData.csv")
SubmissionZeros = read.csv("SubmissionZerosExceptNAs.csv")

names(TrainingData)

#Convenient to have a way to refer to target variables:
target_names = names(SubmissionZeros)[6:44]
target_names



######### 1st EXAMPLE ##############
#Average over hours
target_means_by_hour_of_day = ddply(TrainingData, "hour", function(df) colMeans(df[,target_names], na.rm = TRUE))
names(SubmissionZeros)

#Create slim submission skeleton for merging onto
submission_hour_means = SubmissionZeros[,1:5]

#Create full set of submissions via merge
submission_hour_means = merge(submission_hour_means, target_means_by_hour_of_day)

#function for putting -1000000's wherever they appear in the sample submission (I'm sorry this is necessary)
replaceNAs = function(submission, sample) {
  submission = submission[,names(sample)]
  stopifnot(mean(names(submission) == names(sample))==1)
  stopifnot(dim(submission) == dim(sample))
  submission = submission[order(submission$rowID),]
  submission[sample == -1000000] = -1000000
  submission
}


submission_hour_means_NAs = replaceNAs(submission_hour_means, SubmissionZeros)
write.csv(submission_hour_means_NAs, "./upload/satpreet_submission_hour_means.csv", row.names = FALSE)



######### 2nd EXAMPLE ##############
#Average by hour and chunk:
target_means_by_hour_and_chunkID = ddply(TrainingData, c("chunkID", "hour"), function(df) colMeans(df[,target_names], na.rm = TRUE))
names(SubmissionZeros)
submission_hour_chunk_means = SubmissionZeros[,1:5]

#The first time I did this I didn't use "all.x=TRUE" and my data frame was too short. Good thing I have error checking in "replaceNAs"!
submission_hour_chunk_means = merge(submission_hour_chunk_means, target_means_by_hour_and_chunkID, all.x=TRUE)
submission_hour_chunk_means_NAs = replaceNAs(submission_hour_chunk_means, SubmissionZeros)

#A few hour/chunk combinations seem to have given us NA's (always missing in training data).
#So we'll replace anywhere we have NA's with whatever we would have predicted in the last submission:
submission_hour_chunk_means_NAs[is.na(submission_hour_chunk_means_NAs)] = submission_hour_means_NAs[is.na(submission_hour_chunk_means_NAs)]
summary(submission_hour_chunk_means_NAs)
write.csv(submission_hour_chunk_means_NAs, "./upload/satpreet_submission_hour_chunk_means.csv", row.names = FALSE)




######### 3rd EXAMPLE ##############
satpreet_submission_n1 = submission_hour_chunk_means_NAs
#Create slim submission skeleton for merging onto
submission_hour_means = SubmissionZeros[,1:5]

#Create full set of submissions via merge
submission_hour_means = merge(submission_hour_means, target_means_by_hour_of_day)


for (col in 