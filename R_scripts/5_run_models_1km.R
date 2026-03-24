df <- read.csv("./Data/Processed_data/complete_dataset_2013_1999_survey_1km.csv")
df$Fen_._habitat <- NULL

df$moorland_habitat <- df$Bog_._habitat +
  df$Heather_._habitat +
  df$Heather.grassland_._habitat +
  df$Acid.grassland_._habitat

df$Bog_._habitat <- NULL
df$Heather_._habitat <- NULL
df$Heather.grassland_._habitat <- NULL
df$Acid.grassland_._habitat <- NULL

mod_df <- df

mod_df$presence <- as.factor(mod_df$presence)
mod_df$year <- as.factor(mod_df$year)

# scale continuous predictors
num_cols <- sapply(mod_df, is.numeric)
num_cols["presence"] <- FALSE

mod_df[, num_cols] <- scale(mod_df[, num_cols])

m_final <- glm(
  presence ~ . - year,
  data = mod_df,
  family = binomial
)

summary(m_final)


library(pROC)

prob <- predict(m_final, type="response")

roc_obj <- roc(mod_df$presence, prob)

auc(roc_obj)


# Cross validation
library(caret)

train_control <- trainControl(method="cv", number=5)

cv_model <- train(
  presence ~ .- year,
  data = mod_df,
  method = "glm",
  family = "binomial",
  trControl = train_control
)

library(pROC)

prob <- predict(cv_model$finalModel, mod_df, type="response")

roc_obj <- roc(mod_df$presence, prob)

auc(roc_obj)
# Random Forest


library(randomForest)

rf_model <- randomForest(
  as.factor(presence) ~ . - year,
  data = mod_df,
  ntree = 500,
  importance = TRUE
)


rf_prob <- predict(rf_model, type="prob")[,2]
roc_obj <- pROC::roc(mod_df$presence, rf_prob)
pROC::auc(roc_obj)
varImpPlot(rf_model)

# Cross validation
train_control <- trainControl(
  method="cv",
  number=5,
  classProbs=TRUE,
  summaryFunction=twoClassSummary
)

mod_df$presence <- factor(
  mod_df$presence,
  levels = c(0,1),
  labels = c("absence","presence")
)

rf_cv <- train(
  as.factor(presence) ~ . - year,
  data=mod_df,
  method="rf",
  metric="ROC",
  trControl=train_control
)
imp <- varImp(rf_cv)
plot(imp, top = 12)
