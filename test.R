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


  titanic_raw <- read.csv("C:\\Users\\jastam\\Downloads\\titanic\\train.csv")

  titanic_raw$Title <- gsub('(.*, )|(\\..*)', '', titanic_raw$Name)

  rare_title <- c('Dona', 'Lady', 'the Countess', 'Capt', 'Col', 'Don',
                  'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
  titanic_raw$Title[titanic_raw$Title == 'Mlle'] <- 'Miss'
  titanic_raw$Title[titanic_raw$Title == 'Ms'] <- 'Miss'
  titanic_raw$Title[titanic_raw$Title == 'Mme'] <- 'Mrs'
  titanic_raw$Title[titanic_raw$Title %in% rare_title] <- 'Rare Title'

  #table(titanic_raw$Sex, titanic_raw$Title)

  titanic_raw$Surname <- sapply(titanic_raw$Name,
                                function(x) strsplit(x, split = '[,.]')[[1]])


  titanic_raw$Fsize <- titanic_raw$SibSp + titanic_raw$Parch + 1
  titanic_raw$FsizeD[titanic_raw$Fsize == 1] <- 'singleton'
  titanic_raw$FsizeD[titanic_raw$Fsize < 5 & titanic_raw$Fsize > 1] <- 'small'
  titanic_raw$FsizeD[titanic_raw$Fsize > 4] <- 'large'

  titanic_raw$Deck <- factor(sapply(titanic_raw$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

  titanic_raw$Sex <- as.factor(titanic_raw$Sex)
  titanic_raw$Embarked <- factor(titanic_raw$Embarked)
  titanic_raw$Deck <- factor(titanic_raw$Deck)
  titanic_raw$FsizeD <- factor(titanic_raw$FsizeD)
  titanic_raw$Title <- factor(titanic_raw$Title)

  str(titanic_raw)
  class(titanic_raw$Embarked)

  factor_vars <- c('Pclass', 'Sex', 'Embarked',
                   'Title', 'FsizeD')
  titanic_raw[factor_vars] <- lapply(titanic_raw[factor_vars], function(x) as.vector(x))

  set.seed(123)
  titanic_raw[titanic_raw == ""] <- NA
  mice_mod <- mice(titanic_raw[, !names(titanic_raw) %in% c('PassengerId', 'Name', 'Ticket', 'Cabin', 'Family', 'Surname', 'Survived')], method = 'rf')
  mice_output <- complete(mice_mod)
  titanic_raw$Age <- mice_output$Age
  titanic_raw$Child[titanic_raw$Age < 18] <- 'Child'
  titanic_raw$Child[titanic_raw$Age >= 18] <- 'Adult'
  # titanic_raw$Mother[titanic_raw$Sex == 'female' &
  #                      titanic_raw$Parch > 0 &
  #                      titanic_raw$Age > 18 &
  #                      titanic_raw$Title != 'Miss'] <- 'Mother'
  titanic_raw$Child <- as.factor(titanic_raw$Child)
  # titanic_raw$Mother <- as.factor(titanic_raw$Mother)

  # Encoding target feature as factor
  titanic_raw$Survived <- factor(titanic_raw$Survived, levels = c(0, 1))

  set.seed(123)
  split <- sample.split(titanic_raw$Survived, SplitRatio = 0.90)
  training_set <- subset(titanic_raw, split == TRUE)
  test_set <- subset(titanic_raw, split == FALSE)

  training_set <- training_set[, c(2,3,5:8,10,12,13,16,18)]
  test_set <- test_set[, c(2,3,5:8,10,12,13,16,18)]

  training_set[,c(2,4:7)] <- scale(training_set[, c(2,4:7)])
  test_set[, c(2,4:7)] <- scale(test_set[, c(2,4:7)])

  training_set$Sex <- factor(training_set$Sex)
  training_set$Embarked <- factor(training_set$Embarked)
  training_set$Title <- factor(training_set$Title)
  training_set$FsizeD <- factor(training_set$FsizeD)

  test_set$Sex <- factor(test_set$Sex)
  test_set$Embarked <- factor(test_set$Embarked)
  test_set$Title <- factor(test_set$Title)
  test_set$FsizeD <- factor(test_set$FsizeD)

str(training_set)
str(test_set)


   folds <- createFolds(training_set$Survived, 10)
    cv <- lapply(folds, function(x) {
      training_fold <- training_set[-x,]
      test_fold <- training_set[x,]
      classifier <- svm(formula = Survived ~ .,
                        data = training_fold,
                        type = 'C-classification',
                        kernel = 'linear')
      y_pred <- predict(classifier, newdata = test_fold[-1])
      y_pred <- as.numeric(as.character(y_pred))
      y_pred <- ifelse(y_pred > 0.5, 1, 0)
      test_fold$Survived <- factor(test_fold$Survived, levels = c(0, 1))
      y_pred <- factor(y_pred, levels = c(0, 1))
      cm <- table(test_fold[, 1], y_pred)
      accuracy <- (cm[1, 1] + cm[2, 2]) / (cm[1, 1] + cm[2, 2] + cm[1, 2] + cm[2, 1])
      return(accuracy)
    })

      classifier <- svm(formula = Survived ~ .,
                        data = training_set,
                        type = 'C-classification',
                        kernel = 'linear')
      y_pred <- predict(classifier, type = 'response', newdata = test_set[-1])
      y_pred <- as.numeric(as.character(y_pred))
      y_pred <- ifelse(y_pred > 0.5, 1, 0)
      test_set$Survived <- factor(test_set$Survived, levels = c(0, 1))
      y_pred <- factor(y_pred, levels = c(0, 1))
      cm <- table(test_set[, 1], y_pred)
      accuracy <- (cm[1, 1] + cm[2, 2]) / (cm[1, 1] + cm[2, 2] + cm[1, 2] + cm[2, 1])
