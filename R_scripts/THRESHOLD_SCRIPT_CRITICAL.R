# libs <- c("pROC", "caret", "ggplot2", "dplyr", "tidyr")
# 
# installed_libs <- libs %in% rownames(installed.packages())
# if (any(installed_libs == FALSE)) install.packages(libs[!installed_libs])
# 
# invisible(lapply(libs, library, character.only = TRUE))
# rm(list = ls())
# 
# df <- read.csv("./Data/Processed_data/Complete_datasets/coast_excluded/10km.csv")
# 
# df$moorland_habitat <- df$Bog_._habitat +
#   df$Heather_._habitat +
#   df$Heather.grassland_._habitat +
#   df$Acid.grassland_._habitat
# 
# df$Bog_._habitat <- NULL
# df$Heather_._habitat <- NULL
# df$Heather.grassland_._habitat <- NULL
# df$Acid.grassland_._habitat <- NULL
# 
# mod_df <- df
# 
# mod_df$presence <- factor(
#   mod_df$presence,
#   levels = c(1, 0),
#   labels = c("presence", "absence")
# )
# 
# mod_df$year <- factor(mod_df$year)
# 
# # choose block size in metres
# block_size <- 50000  # 50 km
# 
# mod_df$x_block <- floor(mod_df$Easting / block_size)
# mod_df$y_block <- floor(mod_df$Northing / block_size)
# mod_df$block_id <- interaction(mod_df$x_block, mod_df$y_block, drop = TRUE)
# 
# set.seed(42)
# unique_blocks <- unique(mod_df$block_id)
# fold_lookup <- data.frame(
#   block_id = unique_blocks,
#   fold = sample(rep(1:5, length.out = length(unique_blocks)))
# )
# 
# mod_df <- merge(mod_df, fold_lookup, by = "block_id", all.x = TRUE)
# mod_df <- mod_df[order(mod_df$year, mod_df$Easting, mod_df$Northing), ]
# 
# # Spatial cross validation
# index_list <- lapply(1:5, function(k) which(mod_df$fold != k))
# index_out_list <- lapply(1:5, function(k) which(mod_df$fold == k))
# 
# train_control_sp <- trainControl(
#   method = "cv",
#   number = 5,
#   index = index_list,
#   indexOut = index_out_list,
#   classProbs = TRUE,
#   summaryFunction = twoClassSummary,
#   savePredictions = "final"
# )
# 
# #########################
# ######### GLM ###########
# #########################
# 
# glm_sp <- train(
#   presence ~ . - year - Easting - Northing - x_block - y_block - block_id - fold,
#   data = mod_df,
#   method = "glm",
#   family = "binomial",
#   metric = "ROC",
#   trControl = train_control_sp
# )
# 
# print(glm_sp)
# 
# #####################
# ######### RF ########
# #####################
# 
# rf_sp <- train(
#   presence ~ . - year - Easting - Northing - x_block - y_block - block_id - fold,
#   data = mod_df,
#   method = "rf",
#   metric = "ROC",
#   tuneLength = 5,
#   trControl = train_control_sp
# )
# 
# print(rf_sp)
# 
# ##############################
# #### THRESHOLDED METRICS #####
# ##############################
# 
# dir.create(file.path("./Plots/", "Model_summary_table"), showWarnings = FALSE, recursive = TRUE)
# 
# # -------------------
# # GLM: ROC + threshold
# # -------------------
# glm_pred <- glm_sp$pred
# 
# roc_glm <- roc(glm_pred$obs, glm_pred$presence, levels = c("absence", "presence"), direction = "<")
# 
# best_glm <- coords(
#   roc_glm,
#   x = "best",
#   best.method = "youden",
#   ret = c("threshold", "sensitivity", "specificity"),
#   transpose = FALSE
# )
# 
# glm_thresh <- as.numeric(best_glm[, "threshold"])
# 
# glm_pred$pred_thresh <- ifelse(
#   glm_pred$presence >= glm_thresh,
#   "presence",
#   "absence"
# )
# 
# glm_pred$pred_thresh <- factor(glm_pred$pred_thresh, levels = c("presence", "absence"))
# 
# glm_cm <- confusionMatrix(
#   data = glm_pred$pred_thresh,
#   reference = glm_pred$obs,
#   positive = "presence"
# )
# 
# # -------------------
# # RF: ROC + threshold
# # -------------------
# rf_pred <- rf_sp$pred
# rf_pred <- rf_pred[rf_pred$mtry == rf_sp$bestTune$mtry, ]
# 
# roc_rf <- roc(rf_pred$obs, rf_pred$presence, levels = c("absence", "presence"), direction = "<")
# 
# best_rf <- coords(
#   roc_rf,
#   x = "best",
#   best.method = "youden",
#   ret = c("threshold", "sensitivity", "specificity"),
#   transpose = FALSE
# )
# 
# rf_thresh <- as.numeric(best_rf[, "threshold"])
# 
# rf_pred$pred_thresh <- ifelse(
#   rf_pred$presence >= rf_thresh,
#   "presence",
#   "absence"
# )
# 
# rf_pred$pred_thresh <- factor(rf_pred$pred_thresh, levels = c("presence", "absence"))
# 
# rf_cm <- confusionMatrix(
#   data = rf_pred$pred_thresh,
#   reference = rf_pred$obs,
#   positive = "presence"
# )
# 
# #######################
# #### OUTPUT TABLE #####
# #######################
# 
# model_summary <- data.frame(
#   Model = c("GLM", "Random forest"),
#   AUC = c(as.numeric(auc(roc_glm)), as.numeric(auc(roc_rf))),
#   Threshold = c(glm_thresh, rf_thresh),
#   Sensitivity = c(
#     glm_cm$byClass["Sensitivity"],
#     rf_cm$byClass["Sensitivity"]
#   ),
#   Specificity = c(
#     glm_cm$byClass["Specificity"],
#     rf_cm$byClass["Specificity"]
#   ),
#   Accuracy = c(
#     glm_cm$overall["Accuracy"],
#     rf_cm$overall["Accuracy"]
#   ),
#   Kappa = c(
#     glm_cm$overall["Kappa"],
#     rf_cm$overall["Kappa"]
#   )
# )
# 
# model_summary <- model_summary |>
#   mutate(across(where(is.numeric), round, 3))
# 
# print(model_summary)
# 
# write.csv(
#   model_summary,
#   "./Plots/Model_summary_table/Summary_stats_100m.csv",
#   row.names = FALSE
# )



####################
####################
####################



libs <- c("pROC", "caret", "ggplot2", "dplyr", "tidyr")

installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)) install.packages(libs[!installed_libs])

invisible(lapply(libs, library, character.only = TRUE))
rm(list = ls())

df <- read.csv("./Data/Processed_data/Complete_datasets/coast_excluded/10km.csv")

df$moorland_habitat <- df$Bog_._habitat +
  df$Heather_._habitat +
  df$Heather.grassland_._habitat +
  df$Acid.grassland_._habitat

df$Bog_._habitat <- NULL
df$Heather_._habitat <- NULL
df$Heather.grassland_._habitat <- NULL
df$Acid.grassland_._habitat <- NULL

mod_df <- df

# IMPORTANT:
# Keep absence first and presence second for fitting,
# so the GLM models probability of presence correctly
mod_df$presence <- factor(
  mod_df$presence,
  levels = c(0, 1),
  labels = c("absence", "presence")
)

mod_df$year <- factor(mod_df$year)

# choose block size in metres
block_size <- 50000  # 50 km

mod_df$x_block <- floor(mod_df$Easting / block_size)
mod_df$y_block <- floor(mod_df$Northing / block_size)
mod_df$block_id <- interaction(mod_df$x_block, mod_df$y_block, drop = TRUE)

set.seed(42)
unique_blocks <- unique(mod_df$block_id)
fold_lookup <- data.frame(
  block_id = unique_blocks,
  fold = sample(rep(1:5, length.out = length(unique_blocks)))
)

mod_df <- merge(mod_df, fold_lookup, by = "block_id", all.x = TRUE)
mod_df <- mod_df[order(mod_df$year, mod_df$Easting, mod_df$Northing), ]

# Spatial cross validation
index_list <- lapply(1:5, function(k) which(mod_df$fold != k))
index_out_list <- lapply(1:5, function(k) which(mod_df$fold == k))

train_control_sp <- trainControl(
  method = "cv",
  number = 5,
  index = index_list,
  indexOut = index_out_list,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

#########################
######### GLM ###########
#########################

glm_sp <- train(
  presence ~ . - year - Easting - Northing - x_block - y_block - block_id - fold,
  data = mod_df,
  method = "glm",
  family = "binomial",
  metric = "ROC",
  trControl = train_control_sp
)

print(glm_sp)

#####################
######### RF ########
#####################

rf_sp <- train(
  presence ~ . - year - Easting - Northing - x_block - y_block - block_id - fold,
  data = mod_df,
  method = "rf",
  metric = "ROC",
  tuneLength = 5,
  trControl = train_control_sp
)

print(rf_sp)

##############################
#### THRESHOLDED METRICS #####
##############################

dir.create(file.path("./Plots/", "Model_summary_table"),
           showWarnings = FALSE, recursive = TRUE)

# -------------------
# GLM: ROC + threshold
# -------------------
glm_pred <- glm_sp$pred

# Safety: keep only relevant columns and ensure factor order is correct
glm_pred$obs <- factor(glm_pred$obs, levels = c("absence", "presence"))

roc_glm <- roc(
  response = glm_pred$obs,
  predictor = glm_pred$presence,
  levels = c("absence", "presence"),
  direction = "<"
)

best_glm <- coords(
  roc_glm,
  x = "best",
  best.method = "youden",
  ret = c("threshold", "sensitivity", "specificity"),
  transpose = FALSE
)

glm_thresh <- as.numeric(best_glm[, "threshold"])

glm_pred$pred_thresh <- ifelse(
  glm_pred$presence >= glm_thresh,
  "presence",
  "absence"
)

glm_pred$pred_thresh <- factor(glm_pred$pred_thresh, levels = c("absence", "presence"))

glm_cm <- confusionMatrix(
  data = glm_pred$pred_thresh,
  reference = glm_pred$obs,
  positive = "presence"
)

# -------------------
# RF: ROC + threshold
# -------------------
rf_pred <- rf_sp$pred
rf_pred <- rf_pred[rf_pred$mtry == rf_sp$bestTune$mtry, ]

rf_pred$obs <- factor(rf_pred$obs, levels = c("absence", "presence"))

roc_rf <- roc(
  response = rf_pred$obs,
  predictor = rf_pred$presence,
  levels = c("absence", "presence"),
  direction = "<"
)

best_rf <- coords(
  roc_rf,
  x = "best",
  best.method = "youden",
  ret = c("threshold", "sensitivity", "specificity"),
  transpose = FALSE
)

rf_thresh <- as.numeric(best_rf[, "threshold"])

rf_pred$pred_thresh <- ifelse(
  rf_pred$presence >= rf_thresh,
  "presence",
  "absence"
)

rf_pred$pred_thresh <- factor(rf_pred$pred_thresh, levels = c("absence", "presence"))

rf_cm <- confusionMatrix(
  data = rf_pred$pred_thresh,
  reference = rf_pred$obs,
  positive = "presence"
)

#######################
#### OUTPUT TABLE #####
#######################

model_summary <- data.frame(
  Model = c("GLM", "Random forest"),
  AUC = c(
    as.numeric(auc(roc_glm)),
    as.numeric(auc(roc_rf))
  ),
  Threshold = c(
    glm_thresh,
    rf_thresh
  ),
  Sensitivity = c(
    glm_cm$byClass["Sensitivity"],
    rf_cm$byClass["Sensitivity"]
  ),
  Specificity = c(
    glm_cm$byClass["Specificity"],
    rf_cm$byClass["Specificity"]
  ),
  Accuracy = c(
    glm_cm$overall["Accuracy"],
    rf_cm$overall["Accuracy"]
  ),
  Kappa = c(
    glm_cm$overall["Kappa"],
    rf_cm$overall["Kappa"]
  )
)

model_summary <- model_summary |>
  mutate(across(where(is.numeric), round, 3))

print(model_summary)

write.csv(
  model_summary,
  "./Plots/Model_summary_table/Summary_stats_100m.csv",
  row.names = FALSE
)