
library(dplyr)
library(gender)
library(stringr)
library(rpart)
library(rpart.plot)

#### DATA SET ####

# Read the text file (comma-delimited)
data <- read.csv("data/HAMILTON.txt")

wanted <- c(
  "SOS_VOTERID",
  "FIRST_NAME",
  "LAST_NAME",
  "PARTY_AFFILIATION",
  "DATE_OF_BIRTH",
  "gender",
  "REGISTRATION_DATE",
  "CITY",
  "CITY_SCHOOL_DISTRICT",
  "MUNICIPAL_COURT_DISTRICT",
  "PRECINCT_NAME",
  "TOWNSHIP",
  "VILLAGE",
  "WARD",
  "GENERAL.11.06.2001", "GENERAL.11.04.2003", "PRIMARY.05.03.2005",
  "PRIMARY.09.13.2005", "GENERAL.11.08.2005", "PRIMARY.05.08.2007",
  "PRIMARY.09.11.2007", "GENERAL.11.06.2007", "PRIMARY.05.05.2009",
  "PRIMARY.09.08.2009", "PRIMARY.09.15.2009", "PRIMARY.09.29.2009",
  "GENERAL.11.03.2009", "PRIMARY.05.03.2011", "PRIMARY.09.13.2011",
  "GENERAL.11.08.2011", "PRIMARY.05.07.2013", "PRIMARY.09.10.2013",
  "PRIMARY.10.01.2013", "GENERAL.11.05.2013", "PRIMARY.05.05.2015",
  "PRIMARY.09.15.2015", "GENERAL.11.03.2015",
  "PRIMARY.05.02.2017", "PRIMARY.09.12.2017", "GENERAL.11.07.2017",
  "PRIMARY.05.07.2019", "PRIMARY.09.10.2019", "GENERAL.11.05.2019",
  "PRIMARY.05.04.2021", "PRIMARY.08.03.2021", "GENERAL.11.02.2021",
  "PRIMARY.05.02.2023", "PRIMARY.10.03.2023", "GENERAL.11.07.2023",
  "PRIMARY.05.06.2025", "PRIMARY.09.09.2025" # example name for this year's primary — replace with your exact column name
)

predict_data <- data %>%
  select(all_of(wanted))

data <- data %>%
  mutate(
    FIRST_NAME = iconv(FIRST_NAME, from = "", to = "UTF-8", sub = ""),  # fix encoding
    FIRST_NAME = gsub("[^A-Za-z]", "", FIRST_NAME),                     # remove symbols/numbers
    FIRST_NAME = trimws(FIRST_NAME),                                    # trim whitespace
    FIRST_NAME = tolower(FIRST_NAME)                                    # make lowercase
  )

name_gender <- gender(unique(data$FIRST_NAME), method = "ssa")

data <- data %>%
  left_join(name_gender %>% select(name, gender), by = c("FIRST_NAME" = "name"))

predict_data$will_vote_2025 <- sample(c(0, 1), nrow(predict_data), replace = TRUE, prob = c(0.65, 0.35))

predict_data <- predict_data %>%
  mutate(gender = ifelse(is.na(gender), "unknown", gender))

predict_data$DATE_OF_BIRTH <- as.numeric(floor((Sys.Date() - as.Date(predict_data$DATE_OF_BIRTH)) / 365.25))
colnames(predict_data)[colnames(predict_data) == "DATE_OF_BIRTH"] <- "AGE"

predict_data$LOCATION <- paste(
  predict_data$PRECINCT_NAME,
  predict_data$TOWNSHIP,
  predict_data$VILLAGE,
  predict_data$WARD,
  sep = ", "
)

predict_data <- predict_data[, !names(predict_data) %in% c("PRECINCT_NAME", "TOWNSHIP", "VILLAGE", "WARD")]

predict_data <- predict_data[, c(
  names(predict_data)[1:which(names(predict_data) == "gender")],
  "LOCATION",
  names(predict_data)[(which(names(predict_data) == "gender") + 1):ncol(predict_data)]
)]

offcycle_cols <- c(
  "GENERAL.11.06.2001", "GENERAL.11.04.2003", "PRIMARY.05.03.2005",
  "PRIMARY.09.13.2005", "GENERAL.11.08.2005", "PRIMARY.05.08.2007",
  "PRIMARY.09.11.2007", "GENERAL.11.06.2007", "PRIMARY.05.05.2009",
  "PRIMARY.09.08.2009", "PRIMARY.09.15.2009", "PRIMARY.09.29.2009",
  "GENERAL.11.03.2009", "PRIMARY.05.03.2011", "PRIMARY.09.13.2011",
  "GENERAL.11.08.2011", "PRIMARY.05.07.2013", "PRIMARY.09.10.2013",
  "PRIMARY.10.01.2013", "GENERAL.11.05.2013", "PRIMARY.05.05.2015",
  "PRIMARY.09.15.2015", "GENERAL.11.03.2015", "PRIMARY.05.02.2017",
  "PRIMARY.09.12.2017", "GENERAL.11.07.2017", "PRIMARY.05.07.2019",
  "PRIMARY.09.10.2019", "GENERAL.11.05.2019", "PRIMARY.05.04.2021",
  "PRIMARY.08.03.2021", "GENERAL.11.02.2021", "PRIMARY.05.02.2023",
  "PRIMARY.10.03.2023", "GENERAL.11.07.2023", "PRIMARY.05.06.2025",
  "PRIMARY.09.09.2025"
)

# Convert "x" to 1 and blanks to 0
predict_data[offcycle_cols] <- lapply(predict_data[offcycle_cols], function(x) {
  x_clean <- trimws(tolower(as.character(x)))  # trim spaces & lowercase
  ifelse(x_clean == "x", 1, 0)
})

# Create a new column that marks if they voted in ANY off-cycle election
predict_data$offcycle_turnout <- ifelse(rowSums(predict_data[offcycle_cols], na.rm = TRUE) > 0, 1, 0)

predict_data <- subset(predict_data, select = -LOCATION.1)

write.csv(predict_data, "data/predict_data.csv", row.names = FALSE)


#### MODEL 1 ####


# 1️⃣ Split data into training and testing sets (80/20)
set.seed(42)  # for reproducibility
train_index <- sample(1:nrow(predict_data), 0.8 * nrow(predict_data))
train_data <- predict_data[train_index, ]
test_data <- predict_data[-train_index, ]

# 2️⃣ Fit a decision tree model
tree_model <- rpart(
  GENERAL.11.02.2021 ~ gender + AGE + PARTY_AFFILIATION + LOCATION + REGISTRATION_DATE
  + GENERAL.11.06.2001 + GENERAL.11.04.2003 + PRIMARY.05.03.2005 +
  PRIMARY.09.13.2005 + GENERAL.11.08.2005 + PRIMARY.05.08.2007 + 
  PRIMARY.09.11.2007 + GENERAL.11.06.2007 + PRIMARY.05.05.2009 +
  PRIMARY.09.08.2009 + PRIMARY.09.15.2009 + PRIMARY.09.29.2009 +
  GENERAL.11.03.2009 + PRIMARY.05.03.2011 + PRIMARY.09.13.2011 +
  GENERAL.11.08.2011 + PRIMARY.05.07.2013 + PRIMARY.09.10.2013 +
  PRIMARY.10.01.2013 + GENERAL.11.05.2013 + PRIMARY.05.05.2015 +
  PRIMARY.09.15.2015 + GENERAL.11.03.2015 +
  PRIMARY.05.02.2017 + PRIMARY.09.12.2017 + GENERAL.11.07.2017 +
  PRIMARY.05.07.2019 + PRIMARY.09.10.2019 + GENERAL.11.05.2019 +
  PRIMARY.05.04.2021 + PRIMARY.08.03.2021, 
  data = train_data,
  method = "class",   # classification tree
  control = rpart.control(cp = 0.01, minsplit = 20)  # basic anti-overfitting parameters
)

# 3️⃣ Visualize the tree
rpart.plot(tree_model, type = 2, extra = 104, fallen.leaves = TRUE)

# 4️⃣ Check complexity parameter table to monitor overfitting
printcp(tree_model)
plotcp(tree_model)

# 5️⃣ Find the optimal CP (complexity parameter)
best_cp <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]), "CP"]

# 6️⃣ Prune the tree to avoid overfitting
pruned_tree <- prune(tree_model, cp = best_cp)
rpart.plot(pruned_tree, type = 2, extra = 104, fallen.leaves = TRUE)

test_data$REGISTRATION_DATE <- factor(test_data$REGISTRATION_DATE, 
                                      levels = levels(train_data$REGISTRATION_DATE))

# 7️⃣ Evaluate accuracy on test data
pred <- predict(pruned_tree, test_data, type = "class")
accuracy <- mean(pred == test_data$offcycle_turnout)
cat("Test Accuracy:", round(accuracy, 3), "\n")

summary(pruned_tree)

pred_prob <- predict(pruned_tree, train_data, type = "prob")[,2]

# Actual outcomes
actual <- train_data$offcycle_turnout

# Pseudo R-squared (squared correlation)
R2_train <- cor(pred_prob, actual)^2
R2_train

pred_prob_test <- predict(pruned_tree, test_data, type = "prob")[,2]
actual_test <- test_data$offcycle_turnout
R2_test <- cor(pred_prob_test, actual_test)^2
R2_test
