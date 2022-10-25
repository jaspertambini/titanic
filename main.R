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
head(titanic_raw)
min(titanic_raw$Age)
upper_whisker <- boxplot.stats(titanic_raw$Age)$stats[5]
boxplot(titanic_raw$Age, main = "Age Data",
        xlab = "", ylab = "Age")
outlier.filter <- titanic_raw$Age < upper_whisker
titanic_raw[outlier.filter,]
age.equation <- "Age ~ Pclass + Sex + Fare + SibSp + Parch + Embarked"
age.model <- lm(
  formula = age.equation,
  data = titanic_raw[outlier.filter,]
)
plot(Age ~ Pclass, data = titanic_raw[outlier.filter,]
)
avPlots(age.model)

age.row <- titanic_raw[
  is.na(titanic_raw$Age),
  c("Pclass", "Sex", "Fare", "SibSp", "Parch", "Embarked")]
Age.predictions <- predict(age.model, newdata = age.row)
titanic_raw[is.na(titanic_raw$Age), "Age"] <- Age.predictions

Embarked.equation <- "Embarked ~ Pclass + Sex + Fare + SibSp + Parch + Age"
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
counts <- titanic_raw %>% count(Title)
counts <- rename(counts, count = n)
ggplot(counts, aes(x = reorder(Title, -count), y = count)) + geom_bar(stat = 'identity')

rare_title <- c('Dona', 'Lady', 'the Countess', 'Capt', 'Col', 'Don',
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
titanic_raw$Title[titanic_raw$Title == 'Mlle'] <- 'Miss'
titanic_raw$Title[titanic_raw$Title == 'Ms'] <- 'Miss'
titanic_raw$Title[titanic_raw$Title == 'Mme'] <- 'Mrs'
titanic_raw$Title[titanic_raw$Title %in% rare_title] <- 'Rare Title'

counts <- titanic_raw %>% count(Title)
counts <- rename(counts, count = n)
ggplot(counts, aes(x = reorder(Title, -count), y = count)) + geom_bar(stat = 'identity')


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

classifier <- function(model_type) {
  # Fitting Logistic Model
  if (model_type == "logistic") {
    classifier <- glm(formula = Survived ~ .,
                      family = binomial,
                      data = training_set)
    y_pred <- predict(classifier, type = 'response', newdata = test_set[-1])
    y_pred <- as.numeric(as.character(y_pred))
    y_pred <- ifelse(y_pred > 0.5, 1, 0)
    feature_importance <- data.frame()
  }
    # Fitting K-NN
  else if (model_type == "knn") {
    y_pred <- knn(train = training_set[, -1], test = test_set[, -1],
                  cl = training_set[, 1], 5)
    y_pred <- as.numeric(as.character(y_pred))
    y_pred <- ifelse(y_pred > 0.5, 1, 0)
    feature_importance <- data.frame()
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
    feature_importance <- data.frame()
  }
    # Fitting Kernel SVM
  else if (model_type == "kernelsvm") {
    classifier <- svm(formula = Survived ~ .,
                      data = training_set,
                      type = 'C-classification',
                      kernel = 'radial', sigma = 0.2656, C = 0.5)
    y_pred <- predict(classifier, newdata = test_set[-1])
    y_pred <- as.numeric(as.character(y_pred))
    y_pred <- ifelse(y_pred > 0.5, 1, 0)
    feature_importance <- data.frame()
  }
    # Fitting Naive Bayes
  else if (model_type == "naivebayes") {
    classifier <- naiveBayes(x = training_set[-1],
                             y = training_set$Survived)
    y_pred <- predict(classifier, newdata = test_set[-1])
    y_pred <- as.numeric(as.character(y_pred))
    y_pred <- ifelse(y_pred > 0.5, 1, 0)
    feature_importance <- data.frame()
  }
    # Fitting Decision Tree
  else if (model_type == "decisiontree") {
    classifier <- rpart(formula = Survived ~ .,
                        data = training_set)
    y_pred <- predict(classifier, newdata = test_set[-1], type = 'class')
    y_pred <- as.numeric(as.character(y_pred))
    y_pred <- ifelse(y_pred > 0.5, 1, 0)
    df <- data.frame(imp = classifier$variable.importance)
    feature_importance <- df %>%
      tibble::rownames_to_column() %>%
      dplyr::rename("variable" = rowname) %>%
      dplyr::arrange(imp) %>%
      dplyr::mutate(variable = forcats::fct_inorder(variable))
  }
    # Fitting Random Forest
  else if (model_type == "randomforest") {
    classifier <- randomForest(x = training_set[-1],
                               y = training_set$Survived,
                               ntree = 10)
    y_pred <- predict(classifier, newdata = test_set[-1], type = 'class')
    y_pred <- as.numeric(as.character(y_pred))
    y_pred <- ifelse(y_pred > 0.5, 1, 0)
    feature_importance <- varImpPlot(classifier)
  }
    # Fitting XGBoost
  else if (model_type == "xgboost") {
    classifier <- xgboost(data = as.matrix(training_set[-1]),
                          label = training_set$Survived,
                          nrounds = 10, verbose = 0)
    y_pred <- predict(classifier, newdata = as.matrix(test_set[-1]), type = 'class')
    y_pred <- as.numeric(as.character(y_pred))
    y_pred <- ifelse(y_pred > 0.5, 1, 0)
    feature_importance <- data.frame()
  }
  else print("No algorithm detected")

  # Confusion Matrix

  test_set$Survived <- factor(test_set$Survived, levels = c(0, 1))
  y_pred <- factor(y_pred, levels = c(0, 1))
  cm_test <- table(data = y_pred, reference = test_set$Survived)
  accuracy <- sum(cm_test[1], cm_test[4]) / sum(cm_test[1:4])
  precision <- cm_test[4] / sum(cm_test[4], cm_test[2])
  sensitivity <- cm_test[4] / sum(cm_test[4], cm_test[3])
  fscore <- (2 * (sensitivity * precision)) / (sensitivity + precision)
  specificity <- cm_test[1] / sum(cm_test[1], cm_test[2])

  summary <- data.frame(metric = c("accuracy", "precision", "sensitivity", "fscore", "specificity"),
                        value = c(accuracy, precision, sensitivity, fscore, specificity))
  listOfDataframe <- list(summary, feature_importance)
  return(listOfDataframe)
}

k_fold_cv <- function(model_type) {
  # Fitting Logistic Model
  if (model_type == "logistic") {
    folds <- createFolds(training_set$Survived, 10)
    cv <- lapply(folds, function(x) {
      training_fold <- training_set[-x,]
      test_fold <- training_set[x,]
      classifier <- glm(formula = Survived ~ .,
                        family = binomial,
                        data = training_fold)
      y_pred <- predict(classifier, type = 'response', newdata = test_fold[-1])
      y_pred <- as.numeric(as.character(y_pred))
      y_pred <- ifelse(y_pred > 0.5, 1, 0)
      test_fold$Survived <- factor(test_fold$Survived, levels = c(0, 1))
      y_pred <- factor(y_pred, levels = c(0, 1))
      cm_test <- table(data = y_pred, reference = test_fold$Survived)
      accuracy <- sum(cm_test[1], cm_test[4]) / sum(cm_test[1:4])
      precision <- cm_test[4] / sum(cm_test[4], cm_test[2])
      sensitivity <- cm_test[4] / sum(cm_test[4], cm_test[3])
      fscore <- (2 * (sensitivity * precision)) / (sensitivity + precision)
      specificity <- cm_test[1] / sum(cm_test[1], cm_test[2])

      summary <- data.frame(metric = c("accuracy", "precision", "sensitivity", "fscore", "specificity"),
                            val_value = c(accuracy, precision, sensitivity, fscore, specificity))
    })
    return((cv))
  }
    # Fitting K-NN
  else if (model_type == "knn") {
    folds <- createFolds(training_set$Survived, 10)
    cv <- lapply(folds, function(x) {
      training_fold <- training_set[-x,]
      test_fold <- training_set[x,]
      y_pred <- knn(train = training_fold[, -1], test = test_fold[, -1],
                    cl = training_fold[, 1], 5)
      y_pred <- as.numeric(as.character(y_pred))
      y_pred <- ifelse(y_pred > 0.5, 1, 0)
      test_fold$Survived <- factor(test_fold$Survived, levels = c(0, 1))
      y_pred <- factor(y_pred, levels = c(0, 1))
      cm_test <- table(data = y_pred, reference = test_fold$Survived)
      accuracy <- sum(cm_test[1], cm_test[4]) / sum(cm_test[1:4])
      precision <- cm_test[4] / sum(cm_test[4], cm_test[2])
      sensitivity <- cm_test[4] / sum(cm_test[4], cm_test[3])
      fscore <- (2 * (sensitivity * precision)) / (sensitivity + precision)
      specificity <- cm_test[1] / sum(cm_test[1], cm_test[2])

      summary <- data.frame(metric = c("accuracy", "precision", "sensitivity", "fscore", "specificity"),
                            val_value = c(accuracy, precision, sensitivity, fscore, specificity))
    })
    return((cv))
  }
    # Fitting SVM
  else if (model_type == "svm") {
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
      cm_test <- table(data = y_pred, reference = test_fold$Survived)
      accuracy <- sum(cm_test[1], cm_test[4]) / sum(cm_test[1:4])
      precision <- cm_test[4] / sum(cm_test[4], cm_test[2])
      sensitivity <- cm_test[4] / sum(cm_test[4], cm_test[3])
      fscore <- (2 * (sensitivity * precision)) / (sensitivity + precision)
      specificity <- cm_test[1] / sum(cm_test[1], cm_test[2])

      summary <- data.frame(metric = c("accuracy", "precision", "sensitivity", "fscore", "specificity"),
                            val_value = c(accuracy, precision, sensitivity, fscore, specificity))
    })
    return((cv))
  }
    # Fitting Kernel SVM
  else if (model_type == "kernelsvm") {
    folds <- createFolds(training_set$Survived, 10)
    cv <- lapply(folds, function(x) {
      training_fold <- training_set[-x,]
      test_fold <- training_set[x,]
      classifier <- svm(formula = Survived ~ .,
                        data = training_fold,
                        type = 'C-classification',
                        kernel = 'radial', sigma = 0.2656, C = 0.5)
      y_pred <- predict(classifier, newdata = test_fold[-1])
      y_pred <- as.numeric(as.character(y_pred))
      y_pred <- ifelse(y_pred > 0.5, 1, 0)
      test_fold$Survived <- factor(test_fold$Survived, levels = c(0, 1))
      y_pred <- factor(y_pred, levels = c(0, 1))
      cm_test <- table(data = y_pred, reference = test_fold$Survived)
      accuracy <- sum(cm_test[1], cm_test[4]) / sum(cm_test[1:4])
      precision <- cm_test[4] / sum(cm_test[4], cm_test[2])
      sensitivity <- cm_test[4] / sum(cm_test[4], cm_test[3])
      fscore <- (2 * (sensitivity * precision)) / (sensitivity + precision)
      specificity <- cm_test[1] / sum(cm_test[1], cm_test[2])

      summary <- data.frame(metric = c("accuracy", "precision", "sensitivity", "fscore", "specificity"),
                            val_value = c(accuracy, precision, sensitivity, fscore, specificity))
    })
    return((cv))
  }
    # Fitting Naive Bayes
  else if (model_type == "naivebayes") {
    folds <- createFolds(training_set$Survived, 10)
    cv <- lapply(folds, function(x) {
      training_fold <- training_set[-x,]
      test_fold <- training_set[x,]
      classifier <- naiveBayes(x = training_fold[-1],
                               y = training_fold$Survived)
      y_pred <- predict(classifier, newdata = test_fold[-1])
      y_pred <- as.numeric(as.character(y_pred))
      y_pred <- ifelse(y_pred > 0.5, 1, 0)
      test_fold$Survived <- factor(test_fold$Survived, levels = c(0, 1))
      y_pred <- factor(y_pred, levels = c(0, 1))
      cm_test <- table(data = y_pred, reference = test_fold$Survived)
      accuracy <- sum(cm_test[1], cm_test[4]) / sum(cm_test[1:4])
      precision <- cm_test[4] / sum(cm_test[4], cm_test[2])
      sensitivity <- cm_test[4] / sum(cm_test[4], cm_test[3])
      fscore <- (2 * (sensitivity * precision)) / (sensitivity + precision)
      specificity <- cm_test[1] / sum(cm_test[1], cm_test[2])

      summary <- data.frame(metric = c("accuracy", "precision", "sensitivity", "fscore", "specificity"),
                            val_value = c(accuracy, precision, sensitivity, fscore, specificity))
    })
    return((cv))
  }
    # Fitting Decision Tree
  else if (model_type == "decisiontree") {
    folds <- createFolds(training_set$Survived, 10)
    cv <- lapply(folds, function(x) {
      training_fold <- training_set[-x,]
      test_fold <- training_set[x,]
      classifier <- rpart(formula = Survived ~ .,
                          data = training_fold)
      y_pred <- predict(classifier, newdata = test_fold[-1], type = 'class')
      y_pred <- as.numeric(as.character(y_pred))
      y_pred <- ifelse(y_pred > 0.5, 1, 0)
      test_fold$Survived <- factor(test_fold$Survived, levels = c(0, 1))
      y_pred <- factor(y_pred, levels = c(0, 1))
      cm_test <- table(data = y_pred, reference = test_fold$Survived)
      accuracy <- sum(cm_test[1], cm_test[4]) / sum(cm_test[1:4])
      precision <- cm_test[4] / sum(cm_test[4], cm_test[2])
      sensitivity <- cm_test[4] / sum(cm_test[4], cm_test[3])
      fscore <- (2 * (sensitivity * precision)) / (sensitivity + precision)
      specificity <- cm_test[1] / sum(cm_test[1], cm_test[2])

      summary <- data.frame(metric = c("accuracy", "precision", "sensitivity", "fscore", "specificity"),
                            val_value = c(accuracy, precision, sensitivity, fscore, specificity))
    })
    return((cv))
  }
    # Fitting Random Forest
  else if (model_type == "randomforest") {
    folds <- createFolds(training_set$Survived, 10)
    cv <- lapply(folds, function(x) {
      training_fold <- training_set[-x,]
      test_fold <- training_set[x,]
      classifier <- randomForest(x = training_fold[-1],
                                 y = training_fold$Survived,
                                 ntree = 10)
      y_pred <- predict(classifier, newdata = test_fold[-1], type = 'class')
      y_pred <- as.numeric(as.character(y_pred))
      y_pred <- ifelse(y_pred > 0.5, 1, 0)
      test_fold$Survived <- factor(test_fold$Survived, levels = c(0, 1))
      y_pred <- factor(y_pred, levels = c(0, 1))
      cm_test <- table(data = y_pred, reference = test_fold$Survived)
      accuracy <- sum(cm_test[1], cm_test[4]) / sum(cm_test[1:4])
      precision <- cm_test[4] / sum(cm_test[4], cm_test[2])
      sensitivity <- cm_test[4] / sum(cm_test[4], cm_test[3])
      fscore <- (2 * (sensitivity * precision)) / (sensitivity + precision)
      specificity <- cm_test[1] / sum(cm_test[1], cm_test[2])

      summary <- data.frame(metric = c("accuracy", "precision", "sensitivity", "fscore", "specificity"),
                            val_value = c(accuracy, precision, sensitivity, fscore, specificity))
    })
    return((cv))
  }
    # Fitting XGBoost
  else if (model_type == "xgboost") {
    folds <- createFolds(training_set$Survived, 10)
    cv <- lapply(folds, function(x) {
      training_fold <- training_set[-x,]
      test_fold <- training_set[x,]
      classifier <- xgboost(data = as.matrix(training_fold[-1]),
                            label = training_fold$Survived,
                            nrounds = 10, verbose = 0)
      y_pred <- predict(classifier, newdata = as.matrix(test_fold[-1]), type = 'class')
      y_pred <- as.numeric(as.character(y_pred))
      y_pred <- ifelse(y_pred > 0.5, 1, 0)
      test_fold$Survived <- factor(test_fold$Survived, levels = c(0, 1))
      y_pred <- factor(y_pred, levels = c(0, 1))
      cm_test <- table(data = y_pred, reference = test_fold$Survived)
      accuracy <- sum(cm_test[1], cm_test[4]) / sum(cm_test[1:4])
      precision <- cm_test[4] / sum(cm_test[4], cm_test[2])
      sensitivity <- cm_test[4] / sum(cm_test[4], cm_test[3])
      fscore <- (2 * (sensitivity * precision)) / (sensitivity + precision)
      specificity <- cm_test[1] / sum(cm_test[1], cm_test[2])

      summary <- data.frame(metric = c("accuracy", "precision", "sensitivity", "fscore", "specificity"),
                            val_value = c(accuracy, precision, sensitivity, fscore, specificity))
    })
    return((cv))
  }
  else print("No algorithm detected")
}

logistic <- classifier("logistic")[1] %>%
  as.data.frame() %>%
  mutate(type = "logistic")
knn <- classifier("knn")[1] %>%
  as.data.frame() %>%
  mutate(type = "knn")
svm <- classifier("svm")[1] %>%
  as.data.frame() %>%
  mutate(type = "svm")
kernelsvm <- classifier("kernelsvm")[1] %>%
  as.data.frame() %>%
  mutate(type = "kernelsvm")
naivebayes <- classifier("naivebayes")[1] %>%
  as.data.frame() %>%
  mutate(type = "naivebayes")
decisiontree <- classifier("decisiontree")[1] %>%
  as.data.frame() %>%
  mutate(type = "decisiontree")
randomforest <- classifier("randomforest")[1] %>%
  as.data.frame() %>%
  mutate(type = "randomforest")
xgboost <- classifier("xgboost")[1] %>%
  as.data.frame() %>%
  mutate(type = "xgboost")


  summary <- logistic %>%
  bind_rows(knn, svm, kernelsvm, naivebayes, decisiontree, randomforest, xgboost)

# classifier("logistic")
# classifier("knn")
# classifier("svm")
# classifier("kernelsvm")
# classifier("naivebayes")
# classifier("decisiontree")
# classifier("randomforest")
# classifier("xgboost")

logistic_cv <- k_fold_cv("logistic") %>%
  plyr::ldply(rbind) %>%
  dplyr::select(-.id) %>%
  mutate(type = "logistic") %>%
  dplyr::group_by(metric, type) %>%
  summarise(val_value = mean(val_value)) %>%
  as.data.frame()
knn_cv <- k_fold_cv("knn") %>%
  plyr::ldply(rbind) %>%
  dplyr::select(-.id) %>%
  mutate(type = "knn") %>%
  dplyr::group_by(metric, type) %>%
  summarise(val_value = mean(val_value)) %>%
  as.data.frame()
svm_cv <- k_fold_cv("svm") %>%
  plyr::ldply(rbind) %>%
  dplyr::select(-.id) %>%
  mutate(type = "svm") %>%
  dplyr::group_by(metric, type) %>%
  summarise(val_value = mean(val_value)) %>%
  as.data.frame()
kernelsvm_cv <- k_fold_cv("kernelsvm") %>%
  plyr::ldply(rbind) %>%
  dplyr::select(-.id) %>%
  mutate(type = "kernelsvm") %>%
  dplyr::group_by(metric, type) %>%
  summarise(val_value = mean(val_value)) %>%
  as.data.frame()
naivebayes_cv <- k_fold_cv("naivebayes") %>%
  plyr::ldply(rbind) %>%
  dplyr::select(-.id) %>%
  mutate(type = "naivebayes") %>%
  dplyr::group_by(metric, type) %>%
  summarise(val_value = mean(val_value)) %>%
  as.data.frame()
decisiontree_cv <- k_fold_cv("decisiontree") %>%
  plyr::ldply(rbind) %>%
  dplyr::select(-.id) %>%
  mutate(type = "decisiontree") %>%
  dplyr::group_by(metric, type) %>%
  summarise(val_value = mean(val_value)) %>%
  as.data.frame()
randomforest_cv <- k_fold_cv("randomforest") %>%
  plyr::ldply(rbind) %>%
  dplyr::select(-.id) %>%
  mutate(type = "randomforest") %>%
  dplyr::group_by(metric, type) %>%
  summarise(val_value = mean(val_value)) %>%
  as.data.frame()
xgboost_cv <- k_fold_cv("xgboost") %>%
  plyr::ldply(rbind) %>%
  dplyr::select(-.id) %>%
  mutate(type = "xgboost") %>%
  dplyr::group_by(metric, type) %>%
  summarise(val_value = mean(val_value)) %>%
  as.data.frame()


summary_cv <- logistic_cv %>%
  bind_rows(knn_cv, svm_cv, kernelsvm_cv, naivebayes_cv, decisiontree_cv, randomforest_cv, xgboost_cv) %>%
  as.data.frame()

summary_all <- summary %>%
  full_join(summary_cv, by = c("metric", "type")) %>%
  dplyr::select(metric, type, value, val_value)

feature_importance_tree <- classifier("decisiontree")[2] %>% as.data.frame()
feature_plot_tree <- ggplot2::ggplot(feature_importance_tree) +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw()

feature_importance_forest <- classifier("randomforest")[2] %>% as.data.frame()
feature_importance_forest <-  feature_importance_forest %>%
    rownames_to_column('variable')
feature_plot_forest <- ggplot2::ggplot(feature_importance_forest) +
  geom_col(aes(x = reorder(variable, MeanDecreaseGini), y = MeanDecreaseGini),
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw()

grid.arrange(feature_plot_tree, feature_plot_forest, ncol=2)
    feature_importance <- varImpPlot(classifier)


hyp_pam_classifier <- train(form = Survived ~ .,
                            data = training_set, method = 'svmRadial'
  , na.action = na.omit)
hyp_pam_classifier
hyp_pam_classifier$bestTune
#
#
# # xgbLinear
# # svmRadial
# # ada
# # blackboost
# # deepboost
# # xgbTree
# # fda


tryCatch(
  expr = {

    test_data_file <- paste0("titanic_", ".csv")
    test_data_file_path <- paste0(data_out_dir, test_data_file)

    write.csv(summary_all, test_data_file_path, row.names = TRUE)

    msg <- paste0("...Successfully saved test data table to ", test_data_file_path)
    level(log) <- 'DEBUG'
    debug(log, msg)
  },
  error = function(e) {
    msg <- paste0("Failed to create Titanic table", " with ", e)
    level(log) <- 'ERROR'
    error(log, msg)
  }
)

