wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wd)
getwd()
today <- Sys.Date()
time <- format(Sys.time(), '%Hh-%Mm-%Ss')

exec <- list()
exec$verbose <- TRUE

source("header.R")

logging_dir <- "./logs/"
log <- init_log(logging_dir, today, time)

load_libraries()

source_functions(list.files(path = ".",
                            pattern = "_functions.R",
                            all.files = TRUE,
                            full.names = TRUE))

# ==== create directories ====
msg <- "Creating directories..."
if (exec$verbose) cat(msg, "\n")

level(log) <- 'INFO'
info(log, msg)

tryCatch(
  expr = {

    sql_dir <- "./sql/"

    data_out_dir <- "./data_out/"

    create_dir(data_out_dir)

    msg <- paste0("...Successfully created all directories")
    level(log) <- 'INFO'
    info(log, msg)
  },
  error = function(e) {
    msg <- paste0("Failed to create all directories", " with ", e)
    level(log) <- 'ERROR'
    error(log, msg)
  }
)

classifier <- function(model_type) {
  titanic_raw <- read.csv("C:\\Users\\jastam\\Downloads\\titanic\\train.csv")

  # Encoding target feature as factor
  titanic_raw$Survived <- factor(titanic_raw$Survived, levels = c(0, 1))

  set.seed(123)
  split <- sample.split(titanic_raw$Survived, SplitRatio = 0.90)
  training_set <- subset(titanic_raw, split == TRUE)
  test_set <- subset(titanic_raw, split == FALSE)

  training_set <- training_set[, c(2:8, 10)]
  test_set <- test_set[, c(2:8, 10)]

  training_set <- training_set %>%
    mutate(Gender = ifelse(Sex == "male", 1,
                           ifelse(Sex == "female", 0, NA))) %>%
  mutate(Age = replace_na(Age,mean(Age, na.rm = TRUE)))
  test_set <- test_set %>%
    mutate(Gender = ifelse(Sex == "male", 1,
                           ifelse(Sex == "female", 0, NA))) %>%
  mutate(Age = replace_na(Age,mean(Age, na.rm = TRUE)))


  #Feature scaling

  training_set[, c(2, 5:9)] <- scale(training_set[, c(2, 5:9)])
  test_set[, c(2, 5:9)] <- scale(test_set[, c(2, 5:9)])

  training_set <- training_set %>% dplyr::select(-Sex, -Name)
  test_set <- test_set %>% dplyr::select(-Sex, -Name)

  # Fitting Logistic Model
  if (model_type == "logistic") {
    classifier <- glm(formula = Survived ~ .,
                      family = binomial,
                      data = training_set)
    y_pred <- predict(classifier, type = 'response', newdata = test_set[-1])
    y_pred <- as.numeric(as.character(y_pred))
    y_pred <- ifelse(y_pred > 0.5, 1, 0)
  }
    # Fitting K-NN
  else if (model_type == "knn") {
    y_pred <- knn(train = training_set[, -1], test = test_set[, -1],
                  cl = training_set[, 1], 5)
    y_pred <- as.numeric(as.character(y_pred))
    y_pred <- ifelse(y_pred > 0.5, 1, 0)
  }
    # Fitting SVM
  else if (model_type == "svm") {
    classifier <- svm(formula = Survived ~ .,
                      data = training_set,
                      type = 'C-classification',
                      kernel = 'linear')
    y_pred <- predict(classifier, newdata = test_set[-1])
    y_pred <- as.numeric(as.character(y_pred))
    y_pred <- ifelse(y_pred > 0.5, 1, 0)
  }
    # Fitting Kernel SVM
  else if (model_type == "kernelsvm") {
    classifier <- svm(formula = Survived ~ .,
                      data = training_set,
                      type = 'C-classification',
                      kernel = 'radial')
    y_pred <- predict(classifier, newdata = test_set[-1])
    y_pred <- as.numeric(as.character(y_pred))
    y_pred <- ifelse(y_pred > 0.5, 1, 0)
  }
    # Fitting Naive Bayes
  else if (model_type == "naivebayes") {
    classifier <- naiveBayes(x = training_set[, -1],
                             y = training_set$Survived)
    y_pred <- predict(classifier, newdata = test_set[-1])
    y_pred <- as.numeric(as.character(y_pred))
    y_pred <- ifelse(y_pred > 0.5, 1, 0)
  }
    # Fitting Decision Tree
  else if (model_type == "decisiontree") {
    classifier <- rpart(formula = Survived ~ .,
                        data = training_set)
    y_pred <- predict(classifier, newdata = test_set[-1], type = 'class')
    y_pred <- as.numeric(as.character(y_pred))
    y_pred <- ifelse(y_pred > 0.5, 1, 0)
    plot(classifier)
  }
    # Fitting Random Forest
  else if (model_type == "randomforest") {
    classifier <- randomForest(x = training_set[-1],
                               y = training_set$Survived,
                               ntree = 10)
    y_pred <- predict(classifier, newdata = test_set[-1], type = 'class')
    y_pred <- as.numeric(as.character(y_pred))
    y_pred <- ifelse(y_pred > 0.5, 1, 0)
  }
  else print("No algorithm detected")

  # Confusion Matrix

  test_set$Survived <- factor(test_set$Survived, levels = c(0, 1))
  y_pred <- factor(y_pred, levels = c(0, 1))
  cm <- confusionMatrix(data = y_pred, reference = test_set$Survived)

  tryCatch(
        expr = {

          test_data_file <- paste0("test", ".csv")
          test_data_file_path <- paste0(data_out_dir, test_data_file)

          write.matrix(cm, test_data_file_path, sep="\t")

          msg <- paste0("...Successfully saved test data table to ", test_data_file_path)
          level(log) <- 'DEBUG'
          debug(log, msg)
        },
        error = function(e) {
          msg <- paste0("Failed to create RFM table", " with ", e)
          level(log) <- 'ERROR'
          error(log, msg)
        }
      )
}

classifier("logistic")