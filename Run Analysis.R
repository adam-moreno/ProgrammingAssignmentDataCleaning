#####################################################                      Read in all data

###SETTING WORKING DIRECTORIES
 test_wd <- "C:\\Users\\morenoa\\Documents\\Coursera\\Johns Hopkins\\Data Cleaning Quiz 4 + assignment\\UCI HAR Dataset\\test"
 train_wd <- "C:\\Users\\morenoa\\Documents\\Coursera\\Johns Hopkins\\Data Cleaning Quiz 4 + assignment\\UCI HAR Dataset\\train"
 overall_wd <- "C:\\Users\\morenoa\\Documents\\Coursera\\Johns Hopkins\\Data Cleaning Quiz 4 + assignment\\UCI HAR Dataset"

###FEATURES WILL BE COLNAMES
 setwd(overall_wd)
 features <- read.table("features.txt")


###READ IN TEST DATA
 setwd(test_wd)
 subject_test <- read.table("subject_test.txt")
 X_test <- read.table("X_test.txt")
 Y_test <- read.table("Y_test.txt")


###READ IN TRAIN DATA
 setwd(train_wd)
 subject_train <- read.table("subject_train.txt")
 X_train <- read.table("X_train.txt")
 Y_train <- read.table("Y_train.txt")


####################################################                   ORGANIZE AND COMBINE DATA
####FEATURES TXT WILL BE COLNAMES FOR X_"FILE"
 colnames(X_train) <- features$V2
 colnames(X_test) <- features$V2

####FIND MEAN AND STD DATA ONLY IN X DATA
 mean_data_only <- features[grep("mean()", features$V2),]
 std_data_only <- features[grep("std()", features$V2),]
 mean_cols <- mean_data_only$V1
 std_cols <- std_data_only$V1

###X MEAN/STD DATA ONLY
 X_test_clean <- subset(X_test, select = c(mean_cols, std_cols))
 X_train_clean <- subset(X_train, select = c(mean_cols, std_cols))

###COMBINE ALL DATA AND RENAME VOLUNTEER/ACTIVITY COLS
 all_test_data <- cbind(subject_test, Y_test, X_test_clean)
 all_train_data <- cbind(subject_train, Y_train, X_train_clean)
 combined_data <- rbind(all_test_data, all_train_data)
 colnames(combined_data)[1:2] <- c("Volunteer #", "Activity Label")
 Descriptors <- c(1, 2)
 combined_data[Descriptors] <- lapply(combined_data[Descriptors], factor)


####################################################                   CREATE NEW DATA SET WITH AVGS FOR EACH SUBJECT & ACTIVITY
Volunteer_1 <- combined_data[combined_data$'Volunteer #' == 1,]
V1_Activity1 <- Volunteer_1[Volunteer_1$'Activity Label' == 1,]
V1_Activity2 <- Volunteer_1[Volunteer_1$'Activity Label' == 2,]
V1_Activity3 <- Volunteer_1[Volunteer_1$'Activity Label' == 3,]
V1_Activity4 <- Volunteer_1[Volunteer_1$'Activity Label' == 4,]
V1_Activity5 <- Volunteer_1[Volunteer_1$'Activity Label' == 5,]

Volunteer_2 <- combined_data[combined_data$'Volunteer #' == 2,]
V2_Activity1 <- Volunteer_2[Volunteer_2$'Activity Label' == 1,]
V2_Activity2 <- Volunteer_2[Volunteer_2$'Activity Label' == 2,]
V2_Activity3 <- Volunteer_2[Volunteer_2$'Activity Label' == 3,]
V2_Activity4 <- Volunteer_2[Volunteer_2$'Activity Label' == 4,]
V2_Activity5 <- Volunteer_2[Volunteer_2$'Activity Label' == 5,]

Volunteer_3 <- combined_data[combined_data$'Volunteer #' == 3,]
V3_Activity1 <- Volunteer_3[Volunteer_3$'Activity Label' == 1,]
V3_Activity2 <- Volunteer_3[Volunteer_3$'Activity Label' == 2,]
V3_Activity3 <- Volunteer_3[Volunteer_3$'Activity Label' == 3,]
V3_Activity4 <- Volunteer_3[Volunteer_3$'Activity Label' == 4,]
V3_Activity5 <- Volunteer_3[Volunteer_3$'Activity Label' == 5,]

Volunteer_4 <- combined_data[combined_data$'Volunteer #' == 4,]
V4_Activity1 <- Volunteer_4[Volunteer_4$'Activity Label' == 1,]
V4_Activity2 <- Volunteer_4[Volunteer_4$'Activity Label' == 2,]
V4_Activity3 <- Volunteer_4[Volunteer_4$'Activity Label' == 3,]
V4_Activity4 <- Volunteer_4[Volunteer_4$'Activity Label' == 4,]
V4_Activity5 <- Volunteer_4[Volunteer_4$'Activity Label' == 5,]

Volunteer_5 <- combined_data[combined_data$'Volunteer #' == 5,]
V5_Activity1 <- Volunteer_5[Volunteer_5$'Activity Label' == 1,]
V5_Activity2 <- Volunteer_5[Volunteer_5$'Activity Label' == 2,]
V5_Activity3 <- Volunteer_5[Volunteer_5$'Activity Label' == 3,]
V5_Activity4 <- Volunteer_5[Volunteer_5$'Activity Label' == 4,]
V5_Activity5 <- Volunteer_5[Volunteer_5$'Activity Label' == 5,]

Volunteer_6 <- combined_data[combined_data$'Volunteer #' == 6,]
V6_Activity1 <- Volunteer_6[Volunteer_6$'Activity Label' == 1,]
V6_Activity2 <- Volunteer_6[Volunteer_6$'Activity Label' == 2,]
V6_Activity3 <- Volunteer_6[Volunteer_6$'Activity Label' == 3,]
V6_Activity4 <- Volunteer_6[Volunteer_6$'Activity Label' == 4,]
V6_Activity5 <- Volunteer_6[Volunteer_6$'Activity Label' == 5,]

Volunteer_7 <- combined_data[combined_data$'Volunteer #' == 7,]
V7_Activity1 <- Volunteer_7[Volunteer_7$'Activity Label' == 1,]
V7_Activity2 <- Volunteer_7[Volunteer_7$'Activity Label' == 2,]
V7_Activity3 <- Volunteer_7[Volunteer_7$'Activity Label' == 3,]
V7_Activity4 <- Volunteer_7[Volunteer_7$'Activity Label' == 4,]
V7_Activity5 <- Volunteer_7[Volunteer_7$'Activity Label' == 5,]

Volunteer_8 <- combined_data[combined_data$'Volunteer #' == 8,]
V8_Activity1 <- Volunteer_8[Volunteer_8$'Activity Label' == 1,]
V8_Activity2 <- Volunteer_8[Volunteer_8$'Activity Label' == 2,]
V8_Activity3 <- Volunteer_8[Volunteer_8$'Activity Label' == 3,]
V8_Activity4 <- Volunteer_8[Volunteer_8$'Activity Label' == 4,]
V8_Activity5 <- Volunteer_8[Volunteer_8$'Activity Label' == 5,]

Volunteer_9 <- combined_data[combined_data$'Volunteer #' == 9,]
V9_Activity1 <- Volunteer_9[Volunteer_9$'Activity Label' == 1,]
V9_Activity2 <- Volunteer_9[Volunteer_9$'Activity Label' == 2,]
V9_Activity3 <- Volunteer_9[Volunteer_9$'Activity Label' == 3,]
V9_Activity4 <- Volunteer_9[Volunteer_9$'Activity Label' == 4,]
V9_Activity5 <- Volunteer_9[Volunteer_9$'Activity Label' == 5,]

Volunteer_10 <- combined_data[combined_data$'Volunteer #' == 10,]
V10_Activity1 <- Volunteer_10[Volunteer_10$'Activity Label' == 1,]
V10_Activity2 <- Volunteer_10[Volunteer_10$'Activity Label' == 2,]
V10_Activity3 <- Volunteer_10[Volunteer_10$'Activity Label' == 3,]
V10_Activity4 <- Volunteer_10[Volunteer_10$'Activity Label' == 4,]
V10_Activity5 <- Volunteer_10[Volunteer_10$'Activity Label' == 5,]

Volunteer_10 <- combined_data[combined_data$'Volunteer #' == 10,]
V10_Activity1 <- Volunteer_10[Volunteer_10$'Activity Label' == 1,]
V10_Activity2 <- Volunteer_10[Volunteer_10$'Activity Label' == 2,]
V10_Activity3 <- Volunteer_10[Volunteer_10$'Activity Label' == 3,]
V10_Activity4 <- Volunteer_10[Volunteer_10$'Activity Label' == 4,]
V10_Activity5 <- Volunteer_10[Volunteer_10$'Activity Label' == 5,]

Volunteer_11 <- combined_data[combined_data$'Volunteer #' == 11,]
V11_Activity1 <- Volunteer_11[Volunteer_11$'Activity Label' == 1,]
V11_Activity2 <- Volunteer_11[Volunteer_11$'Activity Label' == 2,]
V11_Activity3 <- Volunteer_11[Volunteer_11$'Activity Label' == 3,]
V11_Activity4 <- Volunteer_11[Volunteer_11$'Activity Label' == 4,]
V11_Activity5 <- Volunteer_11[Volunteer_11$'Activity Label' == 5,]

Volunteer_12 <- combined_data[combined_data$'Volunteer #' == 12,]
V12_Activity1 <- Volunteer_12[Volunteer_12$'Activity Label' == 1,]
V12_Activity2 <- Volunteer_12[Volunteer_12$'Activity Label' == 2,]
V12_Activity3 <- Volunteer_12[Volunteer_12$'Activity Label' == 3,]
V12_Activity4 <- Volunteer_12[Volunteer_12$'Activity Label' == 4,]
V12_Activity5 <- Volunteer_12[Volunteer_12$'Activity Label' == 5,]

Volunteer_13 <- combined_data[combined_data$'Volunteer #' == 13,]
V13_Activity1 <- Volunteer_13[Volunteer_13$'Activity Label' == 1,]
V13_Activity2 <- Volunteer_13[Volunteer_13$'Activity Label' == 2,]
V13_Activity3 <- Volunteer_13[Volunteer_13$'Activity Label' == 3,]
V13_Activity4 <- Volunteer_13[Volunteer_13$'Activity Label' == 4,]
V13_Activity5 <- Volunteer_13[Volunteer_13$'Activity Label' == 5,]

Volunteer_14 <- combined_data[combined_data$'Volunteer #' == 14,]
V14_Activity1 <- Volunteer_14[Volunteer_14$'Activity Label' == 1,]
V14_Activity2 <- Volunteer_14[Volunteer_14$'Activity Label' == 2,]
V14_Activity3 <- Volunteer_14[Volunteer_14$'Activity Label' == 3,]
V14_Activity4 <- Volunteer_14[Volunteer_14$'Activity Label' == 4,]
V14_Activity5 <- Volunteer_14[Volunteer_14$'Activity Label' == 5,]

Volunteer_15 <- combined_data[combined_data$'Volunteer #' == 15,]
V15_Activity1 <- Volunteer_15[Volunteer_15$'Activity Label' == 1,]
V15_Activity2 <- Volunteer_15[Volunteer_15$'Activity Label' == 2,]
V15_Activity3 <- Volunteer_15[Volunteer_15$'Activity Label' == 3,]
V15_Activity4 <- Volunteer_15[Volunteer_15$'Activity Label' == 4,]
V15_Activity5 <- Volunteer_15[Volunteer_15$'Activity Label' == 5,]

Volunteer_16 <- combined_data[combined_data$'Volunteer #' == 16,]
V16_Activity1 <- Volunteer_16[Volunteer_16$'Activity Label' == 1,]
V16_Activity2 <- Volunteer_16[Volunteer_16$'Activity Label' == 2,]
V16_Activity3 <- Volunteer_16[Volunteer_16$'Activity Label' == 3,]
V16_Activity4 <- Volunteer_16[Volunteer_16$'Activity Label' == 4,]
V16_Activity5 <- Volunteer_16[Volunteer_16$'Activity Label' == 5,]

Volunteer_17 <- combined_data[combined_data$'Volunteer #' == 17,]
V17_Activity1 <- Volunteer_17[Volunteer_17$'Activity Label' == 1,]
V17_Activity2 <- Volunteer_17[Volunteer_17$'Activity Label' == 2,]
V17_Activity3 <- Volunteer_17[Volunteer_17$'Activity Label' == 3,]
V17_Activity4 <- Volunteer_17[Volunteer_17$'Activity Label' == 4,]
V17_Activity5 <- Volunteer_17[Volunteer_17$'Activity Label' == 5,]

Volunteer_18 <- combined_data[combined_data$'Volunteer #' == 18,]
V18_Activity1 <- Volunteer_18[Volunteer_18$'Activity Label' == 1,]
V18_Activity2 <- Volunteer_18[Volunteer_18$'Activity Label' == 2,]
V18_Activity3 <- Volunteer_18[Volunteer_18$'Activity Label' == 3,]
V18_Activity4 <- Volunteer_18[Volunteer_18$'Activity Label' == 4,]
V18_Activity5 <- Volunteer_18[Volunteer_18$'Activity Label' == 5,]

Volunteer_19 <- combined_data[combined_data$'Volunteer #' == 19,]
V19_Activity1 <- Volunteer_19[Volunteer_19$'Activity Label' == 1,]
V19_Activity2 <- Volunteer_19[Volunteer_19$'Activity Label' == 2,]
V19_Activity3 <- Volunteer_19[Volunteer_19$'Activity Label' == 3,]
V19_Activity4 <- Volunteer_19[Volunteer_19$'Activity Label' == 4,]
V19_Activity5 <- Volunteer_19[Volunteer_19$'Activity Label' == 5,]

Volunteer_20 <- combined_data[combined_data$'Volunteer #' == 20,]
V20_Activity1 <- Volunteer_20[Volunteer_20$'Activity Label' == 1,]
V20_Activity2 <- Volunteer_20[Volunteer_20$'Activity Label' == 2,]
V20_Activity3 <- Volunteer_20[Volunteer_20$'Activity Label' == 3,]
V20_Activity4 <- Volunteer_20[Volunteer_20$'Activity Label' == 4,]
V20_Activity5 <- Volunteer_20[Volunteer_20$'Activity Label' == 5,]

Volunteer_21 <- combined_data[combined_data$'Volunteer #' == 21,]
V21_Activity1 <- Volunteer_21[Volunteer_21$'Activity Label' == 1,]
V21_Activity2 <- Volunteer_21[Volunteer_21$'Activity Label' == 2,]
V21_Activity3 <- Volunteer_21[Volunteer_21$'Activity Label' == 3,]
V21_Activity4 <- Volunteer_21[Volunteer_21$'Activity Label' == 4,]
V21_Activity5 <- Volunteer_21[Volunteer_21$'Activity Label' == 5,]

Volunteer_22 <- combined_data[combined_data$'Volunteer #' == 22,]
V22_Activity1 <- Volunteer_22[Volunteer_22$'Activity Label' == 1,]
V22_Activity2 <- Volunteer_22[Volunteer_22$'Activity Label' == 2,]
V22_Activity3 <- Volunteer_22[Volunteer_22$'Activity Label' == 3,]
V22_Activity4 <- Volunteer_22[Volunteer_22$'Activity Label' == 4,]
V22_Activity5 <- Volunteer_22[Volunteer_22$'Activity Label' == 5,]

Volunteer_23 <- combined_data[combined_data$'Volunteer #' == 23,]
V23_Activity1 <- Volunteer_23[Volunteer_23$'Activity Label' == 1,]
V23_Activity2 <- Volunteer_23[Volunteer_23$'Activity Label' == 2,]
V23_Activity3 <- Volunteer_23[Volunteer_23$'Activity Label' == 3,]
V23_Activity4 <- Volunteer_23[Volunteer_23$'Activity Label' == 4,]
V23_Activity5 <- Volunteer_23[Volunteer_23$'Activity Label' == 5,]

Volunteer_24 <- combined_data[combined_data$'Volunteer #' == 24,]
V24_Activity1 <- Volunteer_24[Volunteer_24$'Activity Label' == 1,]
V24_Activity2 <- Volunteer_24[Volunteer_24$'Activity Label' == 2,]
V24_Activity3 <- Volunteer_24[Volunteer_24$'Activity Label' == 3,]
V24_Activity4 <- Volunteer_24[Volunteer_24$'Activity Label' == 4,]
V24_Activity5 <- Volunteer_24[Volunteer_24$'Activity Label' == 5,]

Volunteer_25 <- combined_data[combined_data$'Volunteer #' == 25,]
V25_Activity1 <- Volunteer_25[Volunteer_25$'Activity Label' == 1,]
V25_Activity2 <- Volunteer_25[Volunteer_25$'Activity Label' == 2,]
V25_Activity3 <- Volunteer_25[Volunteer_25$'Activity Label' == 3,]
V25_Activity4 <- Volunteer_25[Volunteer_25$'Activity Label' == 4,]
V25_Activity5 <- Volunteer_25[Volunteer_25$'Activity Label' == 5,]

Volunteer_26 <- combined_data[combined_data$'Volunteer #' == 26,]
V26_Activity1 <- Volunteer_26[Volunteer_26$'Activity Label' == 1,]
V26_Activity2 <- Volunteer_26[Volunteer_26$'Activity Label' == 2,]
V26_Activity3 <- Volunteer_26[Volunteer_26$'Activity Label' == 3,]
V26_Activity4 <- Volunteer_26[Volunteer_26$'Activity Label' == 4,]
V26_Activity5 <- Volunteer_26[Volunteer_26$'Activity Label' == 5,]

Volunteer_27 <- combined_data[combined_data$'Volunteer #' == 27,]
V27_Activity1 <- Volunteer_27[Volunteer_27$'Activity Label' == 1,]
V27_Activity2 <- Volunteer_27[Volunteer_27$'Activity Label' == 2,]
V27_Activity3 <- Volunteer_27[Volunteer_27$'Activity Label' == 3,]
V27_Activity4 <- Volunteer_27[Volunteer_27$'Activity Label' == 4,]
V27_Activity5 <- Volunteer_27[Volunteer_27$'Activity Label' == 5,]

Volunteer_28 <- combined_data[combined_data$'Volunteer #' == 28,]
V28_Activity1 <- Volunteer_28[Volunteer_28$'Activity Label' == 1,]
V28_Activity2 <- Volunteer_28[Volunteer_28$'Activity Label' == 2,]
V28_Activity3 <- Volunteer_28[Volunteer_28$'Activity Label' == 3,]
V28_Activity4 <- Volunteer_28[Volunteer_28$'Activity Label' == 4,]
V28_Activity5 <- Volunteer_28[Volunteer_28$'Activity Label' == 5,]

Volunteer_29 <- combined_data[combined_data$'Volunteer #' == 29,]
V29_Activity1 <- Volunteer_29[Volunteer_29$'Activity Label' == 1,]
V29_Activity2 <- Volunteer_29[Volunteer_29$'Activity Label' == 2,]
V29_Activity3 <- Volunteer_29[Volunteer_29$'Activity Label' == 3,]
V29_Activity4 <- Volunteer_29[Volunteer_29$'Activity Label' == 4,]
V29_Activity5 <- Volunteer_29[Volunteer_29$'Activity Label' == 5,]

Volunteer_30 <- combined_data[combined_data$'Volunteer #' == 30,]
V30_Activity1 <- Volunteer_30[Volunteer_30$'Activity Label' == 1,]
V30_Activity2 <- Volunteer_30[Volunteer_30$'Activity Label' == 2,]
V30_Activity3 <- Volunteer_30[Volunteer_30$'Activity Label' == 3,]
V30_Activity4 <- Volunteer_30[Volunteer_30$'Activity Label' == 4,]
V30_Activity5 <- Volunteer_30[Volunteer_30$'Activity Label' == 5,]


###RBIND ALL VOLUNTEER DATA

V1_Avg5 <- suppressWarnings(rbind(V1_Activity5, colMeans = colMeans(V1_Activity5[, sapply(V1_Activity5, is.numeric)])))
V1_Avg4 <- suppressWarnings(rbind(V1_Activity4, colMeans = colMeans(V1_Activity4[, sapply(V1_Activity4, is.numeric)])))
V1_Avg3 <- suppressWarnings(rbind(V1_Activity3, colMeans = colMeans(V1_Activity3[, sapply(V1_Activity3, is.numeric)])))
V1_Avg2 <- suppressWarnings(rbind(V1_Activity2, colMeans = colMeans(V1_Activity2[, sapply(V1_Activity2, is.numeric)])))
V1_Avg1 <- suppressWarnings(rbind(V1_Activity1, colMeans = colMeans(V1_Activity1[, sapply(V1_Activity1, is.numeric)])))
V1_all <- rbind(V1_Avg5, V1_Avg4, V1_Avg3, V1_Avg2, V1_Avg1)

V2_Avg5 <- suppressWarnings(rbind(V2_Activity5, colMeans = colMeans(V2_Activity5[, sapply(V2_Activity5, is.numeric)])))
V2_Avg4 <- suppressWarnings(rbind(V2_Activity4, colMeans = colMeans(V2_Activity4[, sapply(V2_Activity4, is.numeric)])))
V2_Avg3 <- suppressWarnings(rbind(V2_Activity3, colMeans = colMeans(V2_Activity3[, sapply(V2_Activity3, is.numeric)])))
V2_Avg2 <- suppressWarnings(rbind(V2_Activity2, colMeans = colMeans(V2_Activity2[, sapply(V2_Activity2, is.numeric)])))
V2_Avg1 <- suppressWarnings(rbind(V2_Activity1, colMeans = colMeans(V2_Activity1[, sapply(V2_Activity1, is.numeric)])))
V2_all <- rbind(V2_Avg5, V2_Avg4, V2_Avg3, V2_Avg2, V2_Avg1)

V3_Avg5 <- suppressWarnings(rbind(V3_Activity5, colMeans = colMeans(V3_Activity5[, sapply(V3_Activity5, is.numeric)])))
V3_Avg4 <- suppressWarnings(rbind(V3_Activity4, colMeans = colMeans(V3_Activity4[, sapply(V3_Activity4, is.numeric)])))
V3_Avg3 <- suppressWarnings(rbind(V3_Activity3, colMeans = colMeans(V3_Activity3[, sapply(V3_Activity3, is.numeric)])))
V3_Avg2 <- suppressWarnings(rbind(V3_Activity2, colMeans = colMeans(V3_Activity2[, sapply(V3_Activity2, is.numeric)])))
V3_Avg1 <- suppressWarnings(rbind(V3_Activity1, colMeans = colMeans(V3_Activity1[, sapply(V3_Activity1, is.numeric)])))
V3_all <- rbind(V3_Avg5, V3_Avg4, V3_Avg3, V3_Avg2, V3_Avg1)

V4_Avg5 <- suppressWarnings(rbind(V4_Activity5, colMeans = colMeans(V4_Activity5[, sapply(V4_Activity5, is.numeric)])))
V4_Avg4 <- suppressWarnings(rbind(V4_Activity4, colMeans = colMeans(V4_Activity4[, sapply(V4_Activity4, is.numeric)])))
V4_Avg3 <- suppressWarnings(rbind(V4_Activity3, colMeans = colMeans(V4_Activity3[, sapply(V4_Activity3, is.numeric)])))
V4_Avg2 <- suppressWarnings(rbind(V4_Activity2, colMeans = colMeans(V4_Activity2[, sapply(V4_Activity2, is.numeric)])))
V4_Avg1 <- suppressWarnings(rbind(V4_Activity1, colMeans = colMeans(V4_Activity1[, sapply(V4_Activity1, is.numeric)])))
V4_all <- rbind(V4_Avg5, V4_Avg4, V4_Avg3, V4_Avg2, V4_Avg1)

V5_Avg5 <- suppressWarnings(rbind(V5_Activity5, colMeans = colMeans(V5_Activity5[, sapply(V5_Activity5, is.numeric)])))
V5_Avg4 <- suppressWarnings(rbind(V5_Activity4, colMeans = colMeans(V5_Activity4[, sapply(V5_Activity4, is.numeric)])))
V5_Avg3 <- suppressWarnings(rbind(V5_Activity3, colMeans = colMeans(V5_Activity3[, sapply(V5_Activity3, is.numeric)])))
V5_Avg2 <- suppressWarnings(rbind(V5_Activity2, colMeans = colMeans(V5_Activity2[, sapply(V5_Activity2, is.numeric)])))
V5_Avg1 <- suppressWarnings(rbind(V5_Activity1, colMeans = colMeans(V5_Activity1[, sapply(V5_Activity1, is.numeric)])))
V5_all <- rbind(V5_Avg5, V5_Avg4, V5_Avg3, V5_Avg2, V5_Avg1)

V6_Avg5 <- suppressWarnings(rbind(V6_Activity5, colMeans = colMeans(V6_Activity5[, sapply(V6_Activity5, is.numeric)])))
V6_Avg4 <- suppressWarnings(rbind(V6_Activity4, colMeans = colMeans(V6_Activity4[, sapply(V6_Activity4, is.numeric)])))
V6_Avg3 <- suppressWarnings(rbind(V6_Activity3, colMeans = colMeans(V6_Activity3[, sapply(V6_Activity3, is.numeric)])))
V6_Avg2 <- suppressWarnings(rbind(V6_Activity2, colMeans = colMeans(V6_Activity2[, sapply(V6_Activity2, is.numeric)])))
V6_Avg1 <- suppressWarnings(rbind(V6_Activity1, colMeans = colMeans(V6_Activity1[, sapply(V6_Activity1, is.numeric)])))
V6_all <- rbind(V6_Avg5, V6_Avg4, V6_Avg3, V6_Avg2, V6_Avg1)

V7_Avg5 <- suppressWarnings(rbind(V7_Activity5, colMeans = colMeans(V7_Activity5[, sapply(V7_Activity5, is.numeric)])))
V7_Avg4 <- suppressWarnings(rbind(V7_Activity4, colMeans = colMeans(V7_Activity4[, sapply(V7_Activity4, is.numeric)])))
V7_Avg3 <- suppressWarnings(rbind(V7_Activity3, colMeans = colMeans(V7_Activity3[, sapply(V7_Activity3, is.numeric)])))
V7_Avg2 <- suppressWarnings(rbind(V7_Activity2, colMeans = colMeans(V7_Activity2[, sapply(V7_Activity2, is.numeric)])))
V7_Avg1 <- suppressWarnings(rbind(V7_Activity1, colMeans = colMeans(V7_Activity1[, sapply(V7_Activity1, is.numeric)])))
V7_all <- rbind(V7_Avg5, V7_Avg4, V7_Avg3, V7_Avg2, V7_Avg1)

V8_Avg5 <- suppressWarnings(rbind(V8_Activity5, colMeans = colMeans(V8_Activity5[, sapply(V8_Activity5, is.numeric)])))
V8_Avg4 <- suppressWarnings(rbind(V8_Activity4, colMeans = colMeans(V8_Activity4[, sapply(V8_Activity4, is.numeric)])))
V8_Avg3 <- suppressWarnings(rbind(V8_Activity3, colMeans = colMeans(V8_Activity3[, sapply(V8_Activity3, is.numeric)])))
V8_Avg2 <- suppressWarnings(rbind(V8_Activity2, colMeans = colMeans(V8_Activity2[, sapply(V8_Activity2, is.numeric)])))
V8_Avg1 <- suppressWarnings(rbind(V8_Activity1, colMeans = colMeans(V8_Activity1[, sapply(V8_Activity1, is.numeric)])))
V8_all <- rbind(V8_Avg5, V8_Avg4, V8_Avg3, V8_Avg2, V8_Avg1)

V9_Avg5 <- suppressWarnings(rbind(V9_Activity5, colMeans = colMeans(V9_Activity5[, sapply(V9_Activity5, is.numeric)])))
V9_Avg4 <- suppressWarnings(rbind(V9_Activity4, colMeans = colMeans(V9_Activity4[, sapply(V9_Activity4, is.numeric)])))
V9_Avg3 <- suppressWarnings(rbind(V9_Activity3, colMeans = colMeans(V9_Activity3[, sapply(V9_Activity3, is.numeric)])))
V9_Avg2 <- suppressWarnings(rbind(V9_Activity2, colMeans = colMeans(V9_Activity2[, sapply(V9_Activity2, is.numeric)])))
V9_Avg1 <- suppressWarnings(rbind(V9_Activity1, colMeans = colMeans(V9_Activity1[, sapply(V9_Activity1, is.numeric)])))
V9_all <- rbind(V9_Avg5, V9_Avg4, V9_Avg3, V9_Avg2, V9_Avg1)

V10_Avg5 <- suppressWarnings(rbind(V10_Activity5, colMeans = colMeans(V10_Activity5[, sapply(V10_Activity5, is.numeric)])))
V10_Avg4 <- suppressWarnings(rbind(V10_Activity4, colMeans = colMeans(V10_Activity4[, sapply(V10_Activity4, is.numeric)])))
V10_Avg3 <- suppressWarnings(rbind(V10_Activity3, colMeans = colMeans(V10_Activity3[, sapply(V10_Activity3, is.numeric)])))
V10_Avg2 <- suppressWarnings(rbind(V10_Activity2, colMeans = colMeans(V10_Activity2[, sapply(V10_Activity2, is.numeric)])))
V10_Avg1 <- suppressWarnings(rbind(V10_Activity1, colMeans = colMeans(V10_Activity1[, sapply(V10_Activity1, is.numeric)])))
V10_all <- rbind(V10_Avg5, V10_Avg4, V10_Avg3, V10_Avg2, V10_Avg1)

V11_Avg5 <- suppressWarnings(rbind(V11_Activity5, colMeans = colMeans(V11_Activity5[, sapply(V11_Activity5, is.numeric)])))
V11_Avg4 <- suppressWarnings(rbind(V11_Activity4, colMeans = colMeans(V11_Activity4[, sapply(V11_Activity4, is.numeric)])))
V11_Avg3 <- suppressWarnings(rbind(V11_Activity3, colMeans = colMeans(V11_Activity3[, sapply(V11_Activity3, is.numeric)])))
V11_Avg2 <- suppressWarnings(rbind(V11_Activity2, colMeans = colMeans(V11_Activity2[, sapply(V11_Activity2, is.numeric)])))
V11_Avg1 <- suppressWarnings(rbind(V11_Activity1, colMeans = colMeans(V11_Activity1[, sapply(V11_Activity1, is.numeric)])))
V11_all <- rbind(V11_Avg5, V11_Avg4, V11_Avg3, V11_Avg2, V11_Avg1)

V12_Avg5 <- suppressWarnings(rbind(V12_Activity5, colMeans = colMeans(V12_Activity5[, sapply(V12_Activity5, is.numeric)])))
V12_Avg4 <- suppressWarnings(rbind(V12_Activity4, colMeans = colMeans(V12_Activity4[, sapply(V12_Activity4, is.numeric)])))
V12_Avg3 <- suppressWarnings(rbind(V12_Activity3, colMeans = colMeans(V12_Activity3[, sapply(V12_Activity3, is.numeric)])))
V12_Avg2 <- suppressWarnings(rbind(V12_Activity2, colMeans = colMeans(V12_Activity2[, sapply(V12_Activity2, is.numeric)])))
V12_Avg1 <- suppressWarnings(rbind(V12_Activity1, colMeans = colMeans(V12_Activity1[, sapply(V12_Activity1, is.numeric)])))
V12_all <- rbind(V12_Avg5, V12_Avg4, V12_Avg3, V12_Avg2, V12_Avg1)

V13_Avg5 <- suppressWarnings(rbind(V13_Activity5, colMeans = colMeans(V13_Activity5[, sapply(V13_Activity5, is.numeric)])))
V13_Avg4 <- suppressWarnings(rbind(V13_Activity4, colMeans = colMeans(V13_Activity4[, sapply(V13_Activity4, is.numeric)])))
V13_Avg3 <- suppressWarnings(rbind(V13_Activity3, colMeans = colMeans(V13_Activity3[, sapply(V13_Activity3, is.numeric)])))
V13_Avg2 <- suppressWarnings(rbind(V13_Activity2, colMeans = colMeans(V13_Activity2[, sapply(V13_Activity2, is.numeric)])))
V13_Avg1 <- suppressWarnings(rbind(V13_Activity1, colMeans = colMeans(V13_Activity1[, sapply(V13_Activity1, is.numeric)])))
V13_all <- rbind(V13_Avg5, V13_Avg4, V13_Avg3, V13_Avg2, V13_Avg1)

V14_Avg5 <- suppressWarnings(rbind(V14_Activity5, colMeans = colMeans(V14_Activity5[, sapply(V14_Activity5, is.numeric)])))
V14_Avg4 <- suppressWarnings(rbind(V14_Activity4, colMeans = colMeans(V14_Activity4[, sapply(V14_Activity4, is.numeric)])))
V14_Avg3 <- suppressWarnings(rbind(V14_Activity3, colMeans = colMeans(V14_Activity3[, sapply(V14_Activity3, is.numeric)])))
V14_Avg2 <- suppressWarnings(rbind(V14_Activity2, colMeans = colMeans(V14_Activity2[, sapply(V14_Activity2, is.numeric)])))
V14_Avg1 <- suppressWarnings(rbind(V14_Activity1, colMeans = colMeans(V14_Activity1[, sapply(V14_Activity1, is.numeric)])))
V14_all <- rbind(V14_Avg5, V14_Avg4, V14_Avg3, V14_Avg2, V14_Avg1)

V15_Avg5 <- suppressWarnings(rbind(V15_Activity5, colMeans = colMeans(V15_Activity5[, sapply(V15_Activity5, is.numeric)])))
V15_Avg4 <- suppressWarnings(rbind(V15_Activity4, colMeans = colMeans(V15_Activity4[, sapply(V15_Activity4, is.numeric)])))
V15_Avg3 <- suppressWarnings(rbind(V15_Activity3, colMeans = colMeans(V15_Activity3[, sapply(V15_Activity3, is.numeric)])))
V15_Avg2 <- suppressWarnings(rbind(V15_Activity2, colMeans = colMeans(V15_Activity2[, sapply(V15_Activity2, is.numeric)])))
V15_Avg1 <- suppressWarnings(rbind(V15_Activity1, colMeans = colMeans(V15_Activity1[, sapply(V15_Activity1, is.numeric)])))
V15_all <- rbind(V15_Avg5, V15_Avg4, V15_Avg3, V15_Avg2, V15_Avg1)

V16_Avg5 <- suppressWarnings(rbind(V16_Activity5, colMeans = colMeans(V16_Activity5[, sapply(V16_Activity5, is.numeric)])))
V16_Avg4 <- suppressWarnings(rbind(V16_Activity4, colMeans = colMeans(V16_Activity4[, sapply(V16_Activity4, is.numeric)])))
V16_Avg3 <- suppressWarnings(rbind(V16_Activity3, colMeans = colMeans(V16_Activity3[, sapply(V16_Activity3, is.numeric)])))
V16_Avg2 <- suppressWarnings(rbind(V16_Activity2, colMeans = colMeans(V16_Activity2[, sapply(V16_Activity2, is.numeric)])))
V16_Avg1 <- suppressWarnings(rbind(V16_Activity1, colMeans = colMeans(V16_Activity1[, sapply(V16_Activity1, is.numeric)])))
V16_all <- rbind(V16_Avg5, V16_Avg4, V16_Avg3, V16_Avg2, V16_Avg1)

V17_Avg5 <- suppressWarnings(rbind(V17_Activity5, colMeans = colMeans(V17_Activity5[, sapply(V17_Activity5, is.numeric)])))
V17_Avg4 <- suppressWarnings(rbind(V17_Activity4, colMeans = colMeans(V17_Activity4[, sapply(V17_Activity4, is.numeric)])))
V17_Avg3 <- suppressWarnings(rbind(V17_Activity3, colMeans = colMeans(V17_Activity3[, sapply(V17_Activity3, is.numeric)])))
V17_Avg2 <- suppressWarnings(rbind(V17_Activity2, colMeans = colMeans(V17_Activity2[, sapply(V17_Activity2, is.numeric)])))
V17_Avg1 <- suppressWarnings(rbind(V17_Activity1, colMeans = colMeans(V17_Activity1[, sapply(V17_Activity1, is.numeric)])))
V17_all <- rbind(V17_Avg5, V17_Avg4, V17_Avg3, V17_Avg2, V17_Avg1)

V18_Avg5 <- suppressWarnings(rbind(V18_Activity5, colMeans = colMeans(V18_Activity5[, sapply(V18_Activity5, is.numeric)])))
V18_Avg4 <- suppressWarnings(rbind(V18_Activity4, colMeans = colMeans(V18_Activity4[, sapply(V18_Activity4, is.numeric)])))
V18_Avg3 <- suppressWarnings(rbind(V18_Activity3, colMeans = colMeans(V18_Activity3[, sapply(V18_Activity3, is.numeric)])))
V18_Avg2 <- suppressWarnings(rbind(V18_Activity2, colMeans = colMeans(V18_Activity2[, sapply(V18_Activity2, is.numeric)])))
V18_Avg1 <- suppressWarnings(rbind(V18_Activity1, colMeans = colMeans(V18_Activity1[, sapply(V18_Activity1, is.numeric)])))
V18_all <- rbind(V18_Avg5, V18_Avg4, V18_Avg3, V18_Avg2, V18_Avg1)

V19_Avg5 <- suppressWarnings(rbind(V19_Activity5, colMeans = colMeans(V19_Activity5[, sapply(V19_Activity5, is.numeric)])))
V19_Avg4 <- suppressWarnings(rbind(V19_Activity4, colMeans = colMeans(V19_Activity4[, sapply(V19_Activity4, is.numeric)])))
V19_Avg3 <- suppressWarnings(rbind(V19_Activity3, colMeans = colMeans(V19_Activity3[, sapply(V19_Activity3, is.numeric)])))
V19_Avg2 <- suppressWarnings(rbind(V19_Activity2, colMeans = colMeans(V19_Activity2[, sapply(V19_Activity2, is.numeric)])))
V19_Avg1 <- suppressWarnings(rbind(V19_Activity1, colMeans = colMeans(V19_Activity1[, sapply(V19_Activity1, is.numeric)])))
V19_all <- rbind(V19_Avg5, V19_Avg4, V19_Avg3, V19_Avg2, V19_Avg1)

V20_Avg5 <- suppressWarnings(rbind(V20_Activity5, colMeans = colMeans(V20_Activity5[, sapply(V20_Activity5, is.numeric)])))
V20_Avg4 <- suppressWarnings(rbind(V20_Activity4, colMeans = colMeans(V20_Activity4[, sapply(V20_Activity4, is.numeric)])))
V20_Avg3 <- suppressWarnings(rbind(V20_Activity3, colMeans = colMeans(V20_Activity3[, sapply(V20_Activity3, is.numeric)])))
V20_Avg2 <- suppressWarnings(rbind(V20_Activity2, colMeans = colMeans(V20_Activity2[, sapply(V20_Activity2, is.numeric)])))
V20_Avg1 <- suppressWarnings(rbind(V20_Activity1, colMeans = colMeans(V20_Activity1[, sapply(V20_Activity1, is.numeric)])))
V20_all <- rbind(V20_Avg5, V20_Avg4, V20_Avg3, V20_Avg2, V20_Avg1)

V21_Avg5 <- suppressWarnings(rbind(V21_Activity5, colMeans = colMeans(V21_Activity5[, sapply(V21_Activity5, is.numeric)])))
V21_Avg4 <- suppressWarnings(rbind(V21_Activity4, colMeans = colMeans(V21_Activity4[, sapply(V21_Activity4, is.numeric)])))
V21_Avg3 <- suppressWarnings(rbind(V21_Activity3, colMeans = colMeans(V21_Activity3[, sapply(V21_Activity3, is.numeric)])))
V21_Avg2 <- suppressWarnings(rbind(V21_Activity2, colMeans = colMeans(V21_Activity2[, sapply(V21_Activity2, is.numeric)])))
V21_Avg1 <- suppressWarnings(rbind(V21_Activity1, colMeans = colMeans(V21_Activity1[, sapply(V21_Activity1, is.numeric)])))
V21_all <- rbind(V21_Avg5, V21_Avg4, V21_Avg3, V21_Avg2, V21_Avg1)

V22_Avg5 <- suppressWarnings(rbind(V22_Activity5, colMeans = colMeans(V22_Activity5[, sapply(V22_Activity5, is.numeric)])))
V22_Avg4 <- suppressWarnings(rbind(V22_Activity4, colMeans = colMeans(V22_Activity4[, sapply(V22_Activity4, is.numeric)])))
V22_Avg3 <- suppressWarnings(rbind(V22_Activity3, colMeans = colMeans(V22_Activity3[, sapply(V22_Activity3, is.numeric)])))
V22_Avg2 <- suppressWarnings(rbind(V22_Activity2, colMeans = colMeans(V22_Activity2[, sapply(V22_Activity2, is.numeric)])))
V22_Avg1 <- suppressWarnings(rbind(V22_Activity1, colMeans = colMeans(V22_Activity1[, sapply(V22_Activity1, is.numeric)])))
V22_all <- rbind(V22_Avg5, V22_Avg4, V22_Avg3, V22_Avg2, V22_Avg1)

V23_Avg5 <- suppressWarnings(rbind(V23_Activity5, colMeans = colMeans(V23_Activity5[, sapply(V23_Activity5, is.numeric)])))
V23_Avg4 <- suppressWarnings(rbind(V23_Activity4, colMeans = colMeans(V23_Activity4[, sapply(V23_Activity4, is.numeric)])))
V23_Avg3 <- suppressWarnings(rbind(V23_Activity3, colMeans = colMeans(V23_Activity3[, sapply(V23_Activity3, is.numeric)])))
V23_Avg2 <- suppressWarnings(rbind(V23_Activity2, colMeans = colMeans(V23_Activity2[, sapply(V23_Activity2, is.numeric)])))
V23_Avg1 <- suppressWarnings(rbind(V23_Activity1, colMeans = colMeans(V23_Activity1[, sapply(V23_Activity1, is.numeric)])))
V23_all <- rbind(V23_Avg5, V23_Avg4, V23_Avg3, V23_Avg2, V23_Avg1)

V24_Avg5 <- suppressWarnings(rbind(V24_Activity5, colMeans = colMeans(V24_Activity5[, sapply(V24_Activity5, is.numeric)])))
V24_Avg4 <- suppressWarnings(rbind(V24_Activity4, colMeans = colMeans(V24_Activity4[, sapply(V24_Activity4, is.numeric)])))
V24_Avg3 <- suppressWarnings(rbind(V24_Activity3, colMeans = colMeans(V24_Activity3[, sapply(V24_Activity3, is.numeric)])))
V24_Avg2 <- suppressWarnings(rbind(V24_Activity2, colMeans = colMeans(V24_Activity2[, sapply(V24_Activity2, is.numeric)])))
V24_Avg1 <- suppressWarnings(rbind(V24_Activity1, colMeans = colMeans(V24_Activity1[, sapply(V24_Activity1, is.numeric)])))
V24_all <- rbind(V24_Avg5, V24_Avg4, V24_Avg3, V24_Avg2, V24_Avg1)

V25_Avg5 <- suppressWarnings(rbind(V25_Activity5, colMeans = colMeans(V25_Activity5[, sapply(V25_Activity5, is.numeric)])))
V25_Avg4 <- suppressWarnings(rbind(V25_Activity4, colMeans = colMeans(V25_Activity4[, sapply(V25_Activity4, is.numeric)])))
V25_Avg3 <- suppressWarnings(rbind(V25_Activity3, colMeans = colMeans(V25_Activity3[, sapply(V25_Activity3, is.numeric)])))
V25_Avg2 <- suppressWarnings(rbind(V25_Activity2, colMeans = colMeans(V25_Activity2[, sapply(V25_Activity2, is.numeric)])))
V25_Avg1 <- suppressWarnings(rbind(V25_Activity1, colMeans = colMeans(V25_Activity1[, sapply(V25_Activity1, is.numeric)])))
V25_all <- rbind(V25_Avg5, V25_Avg4, V25_Avg3, V25_Avg2, V25_Avg1)

V26_Avg5 <- suppressWarnings(rbind(V26_Activity5, colMeans = colMeans(V26_Activity5[, sapply(V26_Activity5, is.numeric)])))
V26_Avg4 <- suppressWarnings(rbind(V26_Activity4, colMeans = colMeans(V26_Activity4[, sapply(V26_Activity4, is.numeric)])))
V26_Avg3 <- suppressWarnings(rbind(V26_Activity3, colMeans = colMeans(V26_Activity3[, sapply(V26_Activity3, is.numeric)])))
V26_Avg2 <- suppressWarnings(rbind(V26_Activity2, colMeans = colMeans(V26_Activity2[, sapply(V26_Activity2, is.numeric)])))
V26_Avg1 <- suppressWarnings(rbind(V26_Activity1, colMeans = colMeans(V26_Activity1[, sapply(V26_Activity1, is.numeric)])))
V26_all <- rbind(V26_Avg5, V26_Avg4, V26_Avg3, V26_Avg2, V26_Avg1)

V27_Avg5 <- suppressWarnings(rbind(V27_Activity5, colMeans = colMeans(V27_Activity5[, sapply(V27_Activity5, is.numeric)])))
V27_Avg4 <- suppressWarnings(rbind(V27_Activity4, colMeans = colMeans(V27_Activity4[, sapply(V27_Activity4, is.numeric)])))
V27_Avg3 <- suppressWarnings(rbind(V27_Activity3, colMeans = colMeans(V27_Activity3[, sapply(V27_Activity3, is.numeric)])))
V27_Avg2 <- suppressWarnings(rbind(V27_Activity2, colMeans = colMeans(V27_Activity2[, sapply(V27_Activity2, is.numeric)])))
V27_Avg1 <- suppressWarnings(rbind(V27_Activity1, colMeans = colMeans(V27_Activity1[, sapply(V27_Activity1, is.numeric)])))
V27_all <- rbind(V27_Avg5, V27_Avg4, V27_Avg3, V27_Avg2, V27_Avg1)

V28_Avg5 <- suppressWarnings(rbind(V28_Activity5, colMeans = colMeans(V28_Activity5[, sapply(V28_Activity5, is.numeric)])))
V28_Avg4 <- suppressWarnings(rbind(V28_Activity4, colMeans = colMeans(V28_Activity4[, sapply(V28_Activity4, is.numeric)])))
V28_Avg3 <- suppressWarnings(rbind(V28_Activity3, colMeans = colMeans(V28_Activity3[, sapply(V28_Activity3, is.numeric)])))
V28_Avg2 <- suppressWarnings(rbind(V28_Activity2, colMeans = colMeans(V28_Activity2[, sapply(V28_Activity2, is.numeric)])))
V28_Avg1 <- suppressWarnings(rbind(V28_Activity1, colMeans = colMeans(V28_Activity1[, sapply(V28_Activity1, is.numeric)])))
V28_all <- rbind(V28_Avg5, V28_Avg4, V28_Avg3, V28_Avg2, V28_Avg1)

V29_Avg5 <- suppressWarnings(rbind(V29_Activity5, colMeans = colMeans(V29_Activity5[, sapply(V29_Activity5, is.numeric)])))
V29_Avg4 <- suppressWarnings(rbind(V29_Activity4, colMeans = colMeans(V29_Activity4[, sapply(V29_Activity4, is.numeric)])))
V29_Avg3 <- suppressWarnings(rbind(V29_Activity3, colMeans = colMeans(V29_Activity3[, sapply(V29_Activity3, is.numeric)])))
V29_Avg2 <- suppressWarnings(rbind(V29_Activity2, colMeans = colMeans(V29_Activity2[, sapply(V29_Activity2, is.numeric)])))
V29_Avg1 <- suppressWarnings(rbind(V29_Activity1, colMeans = colMeans(V29_Activity1[, sapply(V29_Activity1, is.numeric)])))
V29_all <- rbind(V29_Avg5, V29_Avg4, V29_Avg3, V29_Avg2, V29_Avg1)

V30_Avg5 <- suppressWarnings(rbind(V30_Activity5, colMeans = colMeans(V30_Activity5[, sapply(V30_Activity5, is.numeric)])))
V30_Avg4 <- suppressWarnings(rbind(V30_Activity4, colMeans = colMeans(V30_Activity4[, sapply(V30_Activity4, is.numeric)])))
V30_Avg3 <- suppressWarnings(rbind(V30_Activity3, colMeans = colMeans(V30_Activity3[, sapply(V30_Activity3, is.numeric)])))
V30_Avg2 <- suppressWarnings(rbind(V30_Activity2, colMeans = colMeans(V30_Activity2[, sapply(V30_Activity2, is.numeric)])))
V30_Avg1 <- suppressWarnings(rbind(V30_Activity1, colMeans = colMeans(V30_Activity1[, sapply(V30_Activity1, is.numeric)])))
V30_all <- rbind(V30_Avg5, V30_Avg4, V30_Avg3, V30_Avg2, V30_Avg1)

final_output <- rbind(V1_all, V2_all, V3_all, V4_all, V5_all, V6_all, V7_all, V8_all, V9_all, V10_all, V11_all, V12_all, V13_all, 
			V14_all, V15_all, V16_all, V17_all, V18_all, V19_all, V20_all, V21_all, V22_all, V23_all, V24_all, V25_all,
			V26_all, V27_all, V28_all, V29_all, V30_all)

