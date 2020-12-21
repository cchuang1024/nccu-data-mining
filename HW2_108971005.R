library(ggplot2)
library(DMwR)

library(randomForest)
library(rpart)
library(e1071)

set_col_to_factor <- function(df, ...){
  cols <- unlist(list(...))
  
  for(col in cols){
    df[, col] <- as.factor(df[, col])
  }
  
  return(df)
}

set_col_to_integer <- function(df, ...){
  cols <- unlist(list(...))

  for(col in cols) {
    df[, col] <- as.integer(df[, col])
  }

  return(df)
}

set_col_to_numeric <- function(df, ...){
  cols <- unlist(list(...))
  
  for(col in cols){
    if(is.factor(df[, col])){
      df[, col] <- as.numeric(as.character(df[, col]))
    }else{
      df[, col] <- as.numeric(df[, col])      
    }
  }
  
  return (df)
}

set_col_to_category <- function(df, old_col, new_col, mapper){
  df[new_col] <- as.factor(mapper(df[old_col]))
  
  return(df)
}

set_col_to_numerical_factor <- function(df, old_col, new_col) {
  df[, new_col] <- as.factor(as.numeric(as.factor(df[, old_col])))
  
  return(df)
}

set_col_to_log <- function(df, old_col, new_col){
  df[, new_col] <- df[, old_col]
  df[df[new_col]==0, new_col] <- 0.01
  df[, new_col] <- log10(df[, new_col])
  
  return(df)
}

set_col_to_scale <- function(df, old_col, new_col){
  df[, new_col] <- scale(df[, old_col])
  
  return(df)
}

build_fold_indices <- function(fold) {
  valid_fold <- 1:fold
  fold_indices <- data.frame(valid = valid_fold)
  
  return(fold_indices)
}

pick_out_indices <- function(total_rows, fold, ...) {
  indices <- c()
  
  for (i in list(...)) {
    indices <- append(indices, seq(i, total_rows, fold))
  }
  
  indices <- unique(indices)
  
  return(indices)
}

get_by_exclude <- function(df, fold, ...) {
  total_rows <- nrow(df)
  exclude <- pick_out_indices(total_rows, fold, ...)
  excluded <- df[-exclude,]

  return(excluded)
}

get_by_index <- function(df, fold, ...) {
  total_rows <- nrow(df)
  include <- pick_out_indices(total_rows, fold, ...)
  included <- df[include,]
  
  return(included)
}

subset_by_fold <- function(df, fold, index, indices){
  training_frame <- get_by_exclude(df,
                                   fold,
                                   indices[index, 'valid'])
  
  validation_frame <- get_by_index(df, fold, indices[index, 'valid'])
  
  return(list(training=training_frame,
              validation=validation_frame))
}

shuffle_frame <- function(df){
  set.seed(28657)
  shf_rows <- sample(nrow(df))
  shf_train <- df[shf_rows, ]
  return(shf_train)
}

remove_cols <- function(df, ...){
  cols <- unlist(list(...))
  new_df <- df[, !(names(df) %in% cols)]
  
  return(new_df)
}

gender_mapper <- function(gender){
  return(ifelse(gender == 'Male', 1, 0))
}

age_mapper <- function(age) {
  return(ifelse(age <= 30, 2,
         ifelse((31<= age && age <= 45), 3,
         ifelse(age <= 45, 1, 0))))
}

travel_mapper <- function(travel) {
  return(ifelse(travel=='Travel_Rarely', 3,
         ifelse(travel=='Travel_Frequently', 2,
         ifelse(travel=='Non-Travel', 1, 0))))
}

department_mapper <- function(department){
  return(ifelse(department=='Human Resources', 1,
         ifelse(department=='Sales', 2,
         ifelse(department=='Research & Development', 3, 0))))
}

field_mapper <- function(field) {
  # 'Medical''Other''Marketing''Human Resources''Technical Degree'
  return(ifelse(field=='Medical', 1, 
         ifelse(field=='Other', 2,
         ifelse(field=='Marketing', 3,
         ifelse(field=='Human Resources', 4,
         ifelse(field=='Technical Degree', 5, 0))))))
}

marital_mapper <- function(marital){
  return(ifelse(marital=='Single', 1,
         ifelse(marital=='Married', 2,
         ifelse(marital=='Divorced', 3, 0))))
}

attrition_mapper <- function(attrition){
  return(ifelse(attrition=='Yes', 1, 0))
}

train_dt <- function(df, depth=5) {
  model <- rpart(
    Target ~ .,
    data = df,
    control = rpart.control(maxdepth = depth),
    method = "class"
  )
  
  return (model)
}

predict_dt <- function(model, df) {
  pred_frame <- predict(model, df, type = "class")
  
  return (pred_frame)
}

train_rf <- function(df, mtry=8) {
  model <- randomForest(
    Target ~ .,
    data = df,
    mtry = mtry,
    importance=TRUE,
    proximity=TRUE,
    ntree=500)
  
  return (model)
}

predict_rf <- function(model, df) {
  pred_frame <- predict(model, df)
  
  return (pred_frame)
}

train_lg <- function(df) {
  model <- glm(
    Target ~ .,
    data = df,
    family = "binomial")
  
  return (model)
}

predict_lg <- function(model, df, attrition_rate=0.5) {
  pred <- predict(model, df, type="response")
  pred_frame <- ifelse(pred > attrition_rate, 1, 0)
  
  return (pred_frame)
}

train_sv <- function(df, kernel='linear') {
  model <- svm(
    Target ~ .,
    data = df,
    kernel = kernel)

  return (model)
}

predict_sv <- function(model, df) {
  pred_frame <- predict(model, df)

  return (pred_frame)
}

build_cm <- function(df, pred) {
  result_frame <- data.frame(truth = df$Target,
                             pred = pred)
  result_table <- table(result_frame)

    return(result_table)
}

cal_pred_result <- function(cM) {
  true_col <- 2
  false_col <- 1
  positive_row <- 2
  negative_row <- 1
  
  TP <- cM[positive_row, ][true_col]
  FN <- cM[positive_row, ][false_col]
  TN <- cM[negative_row, ][false_col]
  FP <- cM[negative_row, ][true_col]
  
  result <- c(TP, FP, TN, FN)
  names(result) = c("TP", "FP", "TN", "FN")

  return(result)
}

get_rounded <- function(value, round_digits = 2) {
  return(round(value, digits = round_digits))
}

cal_accu <- function(pred_result) {
  TP <- unname(pred_result["TP"])
  FN <- unname(pred_result["FN"])
  TN <- unname(pred_result["TN"])
  FP <- unname(pred_result["FP"])
  
  total_rows = TP + FN + TN + FP
  accuracy <- (TP + TN) / total_rows

  return(get_rounded(accuracy))
}

cal_sensitivity <- function(pred_result) {
  TP <- unname(pred_result["TP"])
  FN <- unname(pred_result["FN"])
  
  sensitivity <- TP / (TP + FN)
  
  return(get_rounded(sensitivity))
}

cal_specificity <- function(pred_result) {
  TN <- unname(pred_result["TN"])
  FP <- unname(pred_result["FP"])
  
  specificity <- TN / (TN + FP)
  
  return(get_rounded(specificity))
}

cal_F1 <- function(pred_result) {
  precision <- cal_precision(pred_result)
  recall <- cal_sensitivity(pred_result)
  F1 <- (2 * precision * recall) / (precision + recall)
  return(get_rounded(F1))
}

cal_precision <- function(pred_result) {
  TP <- unname(pred_result["TP"])
  FP <- unname(pred_result["FP"])
  
  precision <- TP / (TP + FP)
  
  return(get_rounded(precision))
}

evaluate_model <- function(df, pred){
  predict_cm <- build_cm(df, pred)
  predict_result <- cal_pred_result(predict_cm)
  accu <- cal_accu(predict_result)
  preci <- cal_precision(predict_result)
  sens <- cal_sensitivity(predict_result)
  recl <- sens
  spec <- cal_specificity(predict_result)
  f1 <- cal_F1(predict_result)
  
  return(list(cm=predict_cm,
              result=predict_result,
              accuracy=accu,
              recall=recl,
              precision=preci,
              sensitivity=sens,
              specificity=spec,
              F1=f1))
}

