wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wd)
getwd()
today <- Sys.Date()
time <- format(Sys.time(), '%Hh-%Mm-%Ss')

exec <- list()
exec$verbose <- FALSE

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
  titanic_raw[titanic_raw == ""] <- NA
  titanic_raw$Embarked <- as.numeric(as.factor(titanic_raw$Embarked))


  upper_whisker <- boxplot.stats(titanic_raw$Age)$stats[5]
  outlier.filter <- titanic_raw$Age < upper_whisker
  titanic_raw[outlier.filter,]
  age.equation = "Age ~ Pclass + Sex + Fare + SibSp + Parch + Embarked"
  age.model <- lm(
    formula = age.equation,
    data = titanic_raw[outlier.filter,]
  )
  age.row <- titanic_raw[
    is.na(titanic_raw$Age),
    c("Pclass", "Sex", "Fare", "SibSp", "Parch", "Embarked")]
  Age.predictions <- predict(age.model, newdata = age.row)
  titanic_raw[is.na(titanic_raw$Age), "Age"] <- Age.predictions

  upper_whisker <- boxplot.stats(titanic_raw$Embarked)$stats[5]
  outlier.filter <- titanic_raw$Embarked < upper_whisker
  titanic_raw[outlier.filter,]
  Embarked.equation = "Embarked ~ Pclass + Sex + Fare + SibSp + Parch + Age"
  Embarked.model <- lm(
    formula = Embarked.equation,
    data = titanic_raw[outlier.filter,]
  )
  Embarked.row <- titanic_raw[
    is.na(titanic_raw$Embarked),
    c("Pclass", "Sex", "Fare", "SibSp", "Parch", "Age")]
  Embarked.predictions <- predict(Embarked.model, newdata = Embarked.row)
  titanic_raw[is.na(titanic_raw$Embarked), "Embarked"] <- Embarked.predictions

  titanic_raw$Title <- gsub('(.*, )|(\\..*)', '', titanic_raw$Name)

  rare_title <- c('Dona', 'Lady', 'the Countess', 'Capt', 'Col', 'Don',
                  'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
  titanic_raw$Title[titanic_raw$Title == 'Mlle'] <- 'Miss'
  titanic_raw$Title[titanic_raw$Title == 'Ms'] <- 'Miss'
  titanic_raw$Title[titanic_raw$Title == 'Mme'] <- 'Mrs'
  titanic_raw$Title[titanic_raw$Title %in% rare_title] <- 'Rare Title'

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

  factor_vars <- c('Pclass', 'Sex', 'Embarked',
                   'Title', 'FsizeD')
  titanic_raw[factor_vars] <- lapply(titanic_raw[factor_vars], function(x) as.vector(x))

  set.seed(123)
  titanic_raw[titanic_raw == ""] <- NA
  titanic_raw$Child[titanic_raw$Age < 18] <- 'Child'
  titanic_raw$Child[titanic_raw$Age >= 18] <- 'Adult'
  titanic_raw$Family_member[titanic_raw$Sex == 'female' &
                              titanic_raw$Parch > 0 &
                              titanic_raw$Age > 18 &
                              titanic_raw$Title != 'Miss'] <- 'Mother'
  titanic_raw$Child <- as.factor(titanic_raw$Child)
  titanic_raw$Family_member <- as.factor(titanic_raw$Family_member)

  # Encoding target feature as factor
  titanic_raw$Survived <- factor(titanic_raw$Survived, levels = c(0, 1))

  set.seed(123)
  split <- sample.split(titanic_raw$Survived, SplitRatio = 0.9)
  training_set <- subset(titanic_raw, split == TRUE)
  test_set <- subset(titanic_raw, split == FALSE)

  training_set <- training_set[, c(2, 3, 5:8, 10, 12, 13, 16, 18)]
  test_set <- test_set[, c(2, 3, 5:8, 10, 12, 13, 16, 18)]
  training_set_ml <- titanic_raw[, c(2, 3, 5:8, 10, 12, 13, 16, 18)]


  training_set[, c(2, 4:7)] <- scale(training_set[, c(2, 4:7)])
  training_set_ml[, c(2, 4:7)] <- scale(training_set_ml[, c(2, 4:7)])

  test_set[, c(2, 4:7)] <- scale(test_set[, c(2, 4:7)])

  training_set$Sex <- as.numeric(as.factor(training_set$Sex))
  training_set$Embarked <- as.numeric(as.factor(training_set$Embarked))
  training_set$Title <- as.numeric(as.factor(training_set$Title))
  training_set$FsizeD <- as.numeric(as.factor(training_set$FsizeD))
  training_set$Child <- as.numeric(as.factor(training_set$Child))

  test_set$Sex <- as.numeric(as.factor(test_set$Sex))
  test_set$Embarked <- as.numeric(as.factor(test_set$Embarked))
  test_set$Title <- as.numeric(as.factor(test_set$Title))
  test_set$FsizeD <- as.numeric(as.factor(test_set$FsizeD))
  test_set$Child <- as.numeric(as.factor(test_set$Child))


  training_set <- na.omit(training_set)


 y_pred <- knn(train = training_set[, -1], test = test_set[, -1],
                    cl = training_set[, 1], 5)
      y_pred <- ifelse(y_pred > 0.5, 1, 0)
      y_pred <- as.numeric(as.character(y_pred))
      y_pred <- ifelse(y_pred > 0.5, 1, 0)
  # Confusion Matrix
  test_set$Survived <- factor(test_set$Survived, levels = c(0, 1))
  y_pred <- factor(y_pred, levels = c(0, 1))

  cm <- confusionMatrix(data = y_pred, reference = test_set$Survived)
  summary <- as.data.frame(as.list(cm$byClass))



logistic<- classifier("logistic")