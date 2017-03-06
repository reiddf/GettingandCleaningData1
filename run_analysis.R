rm(list = ls())   # resets the environment
# install.packages("reshape2")
# install.packages("data.table")
# install.packages("stringr")

# -----------------------------------------------------------
# getfile -- download data files (zip file)
# -----------------------------------------------------------
getfile <- function ()
{
  # library(jpeg)
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  f <- file.path(getwd(), "20Dataset.zip")
  # download.file(url, f, mode="wb")
  download.file(url, f)
  
  list.files(f, recursive=TRUE)
  
  
}

# -----------------------------------------------------------
# getfile -- download data files (zip file)
# -----------------------------------------------------------
unzip_file <- function ()
{
  datadir1 <- file.path(getwd(), "UCI HAR Dataset")
  f1 <- file.path(getwd(), "20Dataset.zip")
  
  dir.create(datadir1)
  unzip(f1,exdir=getwd()) 
}


# -----------------------------------------------------------
# main program
# -----------------------------------------------------------
main <- function ()
{
  null_obj <- "";
  library("data.table")
  
  # -----------------------------------------------------------------------------
  # 0. Get the input data.
  # -----------------------------------------------------------------------------
  f <- file.path(getwd(), "20Dataset.zip")
  if (!file.exists(f)) {  getfile();}
  else {  cat("---------------------bypassing download.", null_obj,"\n");  }
  
  datadir <- file.path(getwd(), "UCI HAR Dataset")
  if (!file.exists(datadir)) { unzip_file(); }
  else {  cat("---------------------bypassing unzip.", null_obj,"\n");  }
  # list.files(pathIn, recursive = TRUE)

  activity_xref  <- fread(file.path(datadir, "activity_labels.txt"))
  setnames(activity_xref, names(activity_xref), c("activity", "activity_name"))
  
  features_xref  <- fread(file.path(datadir, "features.txt"))

  act_train      <- fread(file.path(datadir, "train", "Y_train.txt"))
  act_test       <- fread(file.path(datadir, "test",  "Y_test.txt"))
  subject_train  <- fread(file.path(datadir, "train", "subject_train.txt"))
  subject_test   <- fread(file.path(datadir, "test",  "subject_test.txt"))
  x_test         <- fread(file.path(datadir, "test",  "X_test.txt"))
  x_train        <- fread(file.path(datadir, "train", "X_train.txt"))
  # -- features_xref
  # -- activity_xref
  # -- act_test 
  # -- act_train

  # -----------------------------------------------------------------------------
  # 1. Merges the training and the test sets to create one data set. 
  # -----------------------------------------------------------------------------
  activity <- rbind(act_train, act_test)
  setnames(activity, "V1", "activity")
  # -- activity
  subject <- rbind(subject_train, subject_test)
  setnames(subject, "V1", "subject")
  # -- subject 
  x <- rbind(x_train, x_test)
  # -- x
  
  subject <- cbind(subject, activity)
  x       <- cbind(subject, x)
  

  # -----------------------------------------------------------------------------
  # 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
  # -----------------------------------------------------------------------------
  features_xref_mean_stddev <- features_xref[grepl("mean|std", V2)]
  features_xref_mean_stddev$V3 <- paste0("V", features_xref_mean_stddev$V1)
  features_xref_mean_stddev
  
  keepcols <- c("subject", "activity", features_xref_mean_stddev$V3)
  # -- keepcols
  
  x <- x[ , keepcols, with=FALSE]
  # -- x

  # -----------------------------------------------------------------------------
  # 3. Uses descriptive activity names to name the activities in the data set
  # -----------------------------------------------------------------------------
  x_pretty <- merge (x, activity_xref, by = "activity")
  # -- x_pretty

  # -----------------------------------------------------------------------------
  # 4. Appropriately labels the data set with descriptive variable names.
  # -----------------------------------------------------------------------------
  setnames(features_xref_mean_stddev, names(features_xref_mean_stddev), c("feature_no_v", "feature_name","feature"))
  # -- features_xref_mean_stddev
  x_pretty <- data.table(melt(x_pretty, c("subject", "activity", "activity_name"), variable.name="feature"))
  x_pretty <- merge (x_pretty, features_xref_mean_stddev, by = "feature")
  #dropcols <- c("feature_no_v")
  #x_pretty <- x_pretty[ , !(names(x_pretty) %in% dropcols)]
  x_pretty <- subset(x_pretty, select = -c(feature_no_v) )
  
  time<- grep("^t" , x_pretty$feature_name)
  fft <- grep("^f" , x_pretty$feature_name)
  
  x_pretty$signal_domain <- NA
  x_pretty$signal_domain[time] <- "time"
  x_pretty$signal_domain[fft]  <- "frequency"
  
  # head(x_pretty)
  # x_pretty[x_pretty$signal_domain =="frequency", ]
  
  val_mean <- grep("mean()", x_pretty$feature_name)
  val_std  <- grep("std()", x_pretty$feature_name)
  x_pretty$calc_type <- NA
  x_pretty$calc_type[val_mean] <- "mean"
  x_pretty$calc_type[val_std]  <- "std"
  
  # head(x_pretty)
  # x_pretty[x_pretty$calc =="std", ]
  # x_pretty[x_pretty$calc =="mean", ]
  
  axis_x <- grep("-X", x_pretty$feature_name)
  axis_y <- grep("-Y", x_pretty$feature_name)
  axis_z <- grep("-Z", x_pretty$feature_name)
  x_pretty$axis <- NA
  x_pretty$axis[axis_x] <- "X"
  x_pretty$axis[axis_y] <- "Y"
  x_pretty$axis[axis_z] <- "Z"
  
  # x_pretty[x_pretty$axis =="X", ]
  # x_pretty[x_pretty$axis =="Y", ]
  # x_pretty[x_pretty$axis =="Z", ]
  
  x_pretty$magnitude <- FALSE # "N"
  x_pretty$jerk <- FALSE # "N"
  magnitude <- grep("Mag", x_pretty$feature_name)
  jerk <- grep("Jerk", x_pretty$feature_name)
  x_pretty$magnitude[magnitude] <- TRUE # "Y"
  x_pretty$jerk[jerk] <- TRUE # "Y"
  
  # x_pretty[x_pretty$magnitude =="Y", ]
  # x_pretty[x_pretty$jerk =="Y", ]
  
  gyro <- grep("Gyro", x_pretty$feature_name)
  acc <- grep("Acc", x_pretty$feature_name)
  x_pretty$gyro_acc <- NA
  x_pretty$gyro_acc[gyro] <- "gyro"
  x_pretty$gyro_acc[acc] <- "acc"
  
  # x_pretty[x_pretty$gyro_acc =="gyro", ]
  # x_pretty[x_pretty$gyro_acc =="acc", ]
  
  bodysig<- grep("Body", x_pretty$feature_name)
  gravity<- grep("Gravity", x_pretty$feature_name)
  x_pretty$body_gravity <- NA
  x_pretty$body_gravity[bodysig] <- "body"
  x_pretty$body_gravity[gravity] <- "gravity"
  
  # x_pretty[x_pretty$body_gravity =="body", ]
  # x_pretty[x_pretty$body_gravity =="gravity", ]
  
  x_pretty <- x_pretty[ , c(2,3,4,1,6,7,8,9,10,11,12,13,5)]
  head(x_pretty)
  
  
  fout <- file.path(getwd(), "data_cleansing_project.csv")
  if (file.exists(fout)) {  file.remove(fout)}
  write.csv(x_pretty, file = "data_cleansing_project.csv")

#}
#main2 <- function () 
#{
#  library("data.table")
#  
#  f <- file.path(getwd(), "data_cleansing_project.csv")

#  x_pretty  <- fread(file.path(getwd(), "data_cleansing_project.csv"))
#  x_pretty <- subset(x_pretty, select = -c(V1) )
#  # x_pretty$value_num   <- as.numeric(x_pretty$value)
#  head(x_pretty)
  # -----------------------------------------------------------------------------
  # 5. From the data set in step 4, creates a second, independent tidy data set 
  #    with the average of each variable for each activity and each subject.
  # -----------------------------------------------------------------------------
  subjmeans<- aggregate(x_pretty[, 13], list(x_pretty$subject, x_pretty$activity, x_pretty$activity_name, x_pretty$feature_name, x_pretty$signal_domain, x_pretty$calc_type, x_pretty$axis, x_pretty$magnitude, x_pretty$jerk, x_pretty$gyro_acc, x_pretty$body_gravity ), mean)
  setnames(subjmeans, names(subjmeans), c("subject", "activity","activity_name","feature_name","signal_domain", "calc_type", "axis", "magnitude", "jerk","gyro_acc","body_gravity", "value_num"))
  head(subjmeans)
  #library("reshape2")
  #subjmeans_pretty <- dcast(subjmeans, subject + activity + activity_name ~ feature_name, value.var="value_num")
  #head(subjmeans_pretty)

  fout2 <- file.path(getwd(), "data_cleansing_project_summary.csv")
  if (file.exists(fout2)) {  file.remove(fout2)}
  write.csv(subjmeans, file = "data_cleansing_project_summary.csv")
  
  
}



