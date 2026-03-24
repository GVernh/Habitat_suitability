

libs <- c("pROC", "caret", "ggplot2", "dplyr", "tidyr")

installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)) install.packages(libs[!installed_libs])

invisible(lapply(libs, library, character.only = TRUE))
rm(list = ls())

df <- read.csv("./Data/Processed_data/Complete_datasets/complete_dataset_2013_1999_survey_100m_15km_excl_SELECTED.csv")

df$moorland_habitat <- df$Bog_._habitat +
  df$Heather_._habitat +
  df$Heather.grassland_._habitat +
  df$Acid.grassland_._habitat

df$Bog_._habitat <- NULL
df$Heather_._habitat <- NULL
df$Heather.grassland_._habitat <- NULL
df$Acid.grassland_._habitat <- NULL

mod_df <- df

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

set.seed(888)
unique_blocks <- unique(mod_df$block_id)
fold_lookup <- data.frame(
  block_id = unique_blocks,
  fold = sample(rep(1:5, length.out = length(unique_blocks)))
)

mod_df <- merge(mod_df, fold_lookup, by = "block_id", all.x = TRUE)
mod_df <- mod_df[order(mod_df$year, mod_df$Easting, mod_df$Northing), ]

# Spatial cross calidation
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

glm_sp

# Uncertainty plots
glm_pred <- glm_sp$pred

glm_pred_df <- mod_df[glm_pred$rowIndex,
                      c("Easting","Northing","presence")]

glm_pred_df$obs <- glm_pred$obs
glm_pred_df$pred_class <- glm_pred$pred
glm_pred_df$pred_prob <- glm_pred$presence

glm_pred_df$error_type <- ifelse(
  glm_pred_df$obs == "presence" & glm_pred_df$pred_class == "absence", "False negative",
  ifelse(
    glm_pred_df$obs == "absence" & glm_pred_df$pred_class == "presence", "False positive",
    "Correct"
  )
)

ggplot(glm_pred_df, aes(Easting, Northing, colour = error_type)) +
  geom_point(size = 1) +
  coord_equal() +
  theme_minimal() +
  labs(
    title = "Spatial distribution of cross-validated prediction errors (GLM)",
    colour = "Prediction"
  )

dir.create(file.path("./Plots/", "Predicted_probability"), showWarnings = FALSE)
ggsave(
  "./Plots/Predicted_probability/Spatial_distribution_glm.png",
  width = 7,
  height = 6,
  dpi = 300
)

ggplot(glm_pred_df, aes(Easting, Northing, colour = pred_prob)) +
  geom_point(size = 1) +
  coord_equal() +
  scale_colour_viridis_c(limits = c(0, 1),
                         breaks = c(0.2, 0.4, 0.6, 0.8, 1)) +
  theme_minimal() +
  labs(
    title = "Cross-validated predicted probability of Twite presence (GLM)",
    colour = "Predicted probability"
  )

ggsave(
  "./Plots/Predicted_probability/Predicted_probability_glm.png",
  width = 7,
  height = 6,
  dpi = 300
)

glm_pred_df$obs_num <- ifelse(glm_pred_df$obs == "presence",1,0)

glm_pred_df$bin <- cut(
  glm_pred_df$pred_prob,
  breaks = seq(0,1,0.1),
  include.lowest = TRUE
)

cal_df_glm <- aggregate(
  cbind(obs_num, pred_prob) ~ bin,
  data = glm_pred_df,
  FUN = mean
)

ggplot(cal_df_glm, aes(pred_prob, obs_num)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE, colour = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  theme_minimal() +
  xlim(0,1) +
  ylim(0,1) +
  labs(
    title = "Calibration of GLM predictions",
    x = "Mean predicted probability",
    y = "Observed proportion of presences"
  )

dir.create(file.path("./Plots/", "Calibration"), showWarnings = FALSE)
ggsave(
  "./Plots/Calibration/Calibration_curve_glm.png",
  width = 7,
  height = 6,
  dpi = 300
)

# Variable importance

# dataframe used for the GLM
pred_df <- mod_df[, !(names(mod_df) %in%
                        c("year", "Easting", "Northing", "x_block", "y_block", "block_id", "fold"))]

# predictor names (exclude response)
pred_names <- setdiff(names(pred_df), "presence")

# helper: make a prediction dataframe for one variable
make_glm_effect <- function(var, model, data, n = 100) {
  
  # typical values for all predictors
  base_row <- data[1, , drop = FALSE]
  
  for (nm in names(base_row)) {
    if (nm == "presence") next
    
    if (is.numeric(data[[nm]])) {
      base_row[[nm]] <- median(data[[nm]], na.rm = TRUE)
    } else if (is.factor(data[[nm]])) {
      base_row[[nm]] <- levels(data[[nm]])[1]
    } else {
      base_row[[nm]] <- data[[nm]][1]
    }
  }
  
  # sequence for focal predictor
  xseq <- seq(min(data[[var]], na.rm = TRUE),
              max(data[[var]], na.rm = TRUE),
              length.out = n)
  
  newdat <- base_row[rep(1, n), , drop = FALSE]
  newdat[[var]] <- xseq
  
  # predict probability
  newdat$pred_prob <- predict(model, newdata = newdat, type = "response")
  newdat$variable <- var
  newdat$x <- xseq
  
  newdat[, c("variable", "x", "pred_prob")]
}

# build effect dataframe for all predictors
glm_eff_list <- lapply(pred_names, make_glm_effect, model = glm_sp$finalModel, data = pred_df)
glm_eff_df <- bind_rows(glm_eff_list)

# nicer labels
nice_labels <- c(
  tas = "Temperature",
  rainfall = "Rainfall",
  sfcWind = "Wind speed",
  groundfrost = "Ground frost",
  moorland_habitat = "Moorland habitat %",
  aspect_conc = "Aspect",
  DTM_100 = "Elevation",
  Slope_mean = "Slope",
  Coast_dist = "Distance to coast",
  Roads_dist = "Distance to roads",
  Urban_dist = "Distance to urban",
  Lake_dist2ha = "Distance to lakes",
  dist_feeding_ground = "Distance to feeding ground",
  log_effort = "Log effort"
)

glm_eff_df$variable_lab <- ifelse(
  glm_eff_df$variable %in% names(nice_labels),
  nice_labels[glm_eff_df$variable],
  glm_eff_df$variable
)

# plot
ggplot(glm_eff_df, aes(x = x, y = pred_prob)) +
  geom_line(linewidth = 0.9, colour = "#2c7fb8") +
  facet_wrap(~ variable_lab, scales = "free_x", ncol = 3) +
  labs(
    title = "Marginal effects of environmental predictors on Twite suitability (GLM)",
    x = NULL,
    y = "Predicted probability of presence"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

ggsave(
  "./Plots/Partial_dependence/twite_partial_dependence_glm.png",
  width = 12,
  height = 10,
  dpi = 300
)

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

rf_sp


# Performance plots

head(rf_sp$pred)
pred_out <- rf_sp$pred

# keep only the best tuning parameter
pred_out <- pred_out[pred_out$mtry == rf_sp$bestTune$mtry, ]

# join back to original rows
pred_map_df <- mod_df[pred_out$rowIndex, c("Easting", "Northing", "presence")]
pred_map_df$obs <- pred_out$obs
pred_map_df$pred_class <- pred_out$pred
pred_map_df$pred_prob <- pred_out$presence

pred_map_df$error_type <- ifelse(
  pred_map_df$obs == "presence" & pred_map_df$pred_class == "absence", "False negative",
  ifelse(
    pred_map_df$obs == "absence" & pred_map_df$pred_class == "presence", "False positive",
    "Correct"
  )
)

table(pred_map_df$error_type)

ggplot(pred_map_df, aes(Easting, Northing, colour = error_type)) +
  geom_point(size = 1) +
  coord_equal() +
  theme_minimal() +
  labs(
    title = "Spatial distribution of cross-validated prediction errors (RF)",
    x = "Easting",
    y = "Northing",
    colour = "Prediction"
  )

ggsave(
  "./Plots/Predicted_probability/Spatial_distribution_rf.png",
  width = 7,
  height = 6,
  dpi = 300
)

ggplot(pred_map_df, aes(Easting, Northing, colour = pred_prob)) +
  geom_point(size = 1) +
  coord_equal() +
  scale_colour_viridis_c(
    limits = c(0, 1),
    breaks = c(0.2, 0.4, 0.6, 0.8, 1)) +
  theme_minimal() +
  labs(
    title = "Cross-validated predicted probability of Twite presence (RF)",
    x = "Easting",
    y = "Northing",
    colour = "Predicted probability"
  )

ggsave(
  "./Plots/Predicted_probability/Predicted_probability_rf.png",
  width = 7,
  height = 6,
  dpi = 300
)

pred_map_df$obs_num <- ifelse(pred_map_df$obs == "presence", 1, 0)

pred_map_df$bin <- cut(
  pred_map_df$pred_prob,
  breaks = seq(0, 1, by = 0.1),
  include.lowest = TRUE
)

cal_df <- aggregate(
  cbind(obs_num, pred_prob) ~ bin,
  data = pred_map_df,
  FUN = mean
)

ggplot(cal_df, aes(pred_prob, obs_num)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE, colour = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  xlim(0,1) + ylim(0,1) +
  theme_minimal() +
  labs(
    title = "Calibration of RF predictions",
    x = "Mean predicted probability",
    y = "Observed proportion of presences"
  )


ggsave(
  "./Plots/Calibration/Calibration_curve_rf.png",
  width = 7,
  height = 6,
  dpi = 300
)

# =========================
# Variable importance plot
# =========================
imp <- varImp(rf_sp)$importance

imp$variable <- rownames(imp)

imp <- imp %>%
  arrange(desc(Overall))

library(pdp)

pred_df <- mod_df[, !(names(mod_df) %in%
                        c("year","Easting","Northing","x_block","y_block","block_id","fold"))]

pred_names <- imp$variable


pdp_list <- lapply(pred_names, function(v){
  
  p <- partial(
    object = rf_sp$finalModel,
    pred.var = v,
    train = pred_df,
    prob = TRUE,
    which.class = "presence",
    plot = FALSE
  )
  
  names(p)[1] <- "value"
  names(p)[2] <- "suitability"
  
  p$variable <- v
  return(p)
  
})

pdp_df <- bind_rows(pdp_list)



nice_names <- c(
  tas = "Temperature",
  rainfall = "Rainfall",
  sfcWind = "Wind speed",
  groundfrost = "Ground frost",
  moorland_habitat = "Moorland habitat %",
  aspect_conc = "Aspect",
  DTM_100 = "Elevation",
  Slope_mean = "Slope",
  Coast_dist = "Distance to coast",
  Roads_dist = "Distance to roads",
  Urban_dist = "Distance to urban",
  Lake_dist2ha = "Distance to lakes",
  dist_feeding_ground = "Distance to feeding ground",
  log_effort = "Log effort"
)

pdp_df$variable_label <- nice_names[pdp_df$variable]

ggplot(pdp_df, aes(value, suitability)) +
  geom_line(size = 1, colour = "#2c7fb8") +
  facet_wrap(~variable_label, scales="free_x", ncol=3) +
  labs(
    x = NULL,
    y = "Predicted probability of presence",
    title = "Partial dependence of environmental predictors on Twite suitability (RF)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face="bold"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust=0.5)
  )

dir.create(file.path("./Plots/", "Partial_dependence"), showWarnings = FALSE)

ggsave(
  "./Plots/Partial_dependence/twite_partial_dependence_rf.png",
  width = 12,
  height = 10,
  dpi = 300
)

#################
# Combined plot #
#################

# GLM predictions
glm_prob <- glm_sp$pred$presence
glm_obs  <- glm_sp$pred$obs

# RF predictions
rf_prob <- rf_sp$pred$presence
rf_obs  <- rf_sp$pred$obs

roc_glm <- roc(glm_obs, glm_prob)
roc_rf  <- roc(rf_obs, rf_prob)

roc_glm_df <- data.frame(
  tpr = roc_glm$sensitivities,
  fpr = 1 - roc_glm$specificities,
  model = "GLM"
)

roc_rf_df <- data.frame(
  tpr = roc_rf$sensitivities,
  fpr = 1 - roc_rf$specificities,
  model = "Random forest"
)

roc_df <- rbind(roc_glm_df, roc_rf_df)
auc_glm <- auc(roc_glm)
auc_rf  <- auc(roc_rf)

ggplot(roc_df, aes(x = fpr, y = tpr, colour = model)) +
  geom_line(size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_equal() +
  theme_minimal(base_size = 12) +
  labs(
    title = "ROC curves comparing GLM and Random Forest models",
    x = "False positive rate",
    y = "True positive rate",
    colour = "Model"
  ) +
  scale_colour_manual(
  values = c("steelblue","darkorange"),
  labels = c(
    paste0("GLM (AUC = ", round(auc_glm,3), ")"),
    paste0("Random forest (AUC = ", round(auc_rf,3), ")")
  )
)



roc_df$model <- ifelse(
  roc_df$model == "GLM",
  paste0("GLM (AUC = ", round(auc_glm, 3), ")"),
  paste0("Random forest (AUC = ", round(auc_rf, 3), ")")
)


dir.create(file.path("./Plots/", "AUC_curves"), showWarnings = FALSE)

ggsave(
  "./Plots/AUC_curves/model_ROC_comparison.png",
  width = 7,
  height = 6,
  dpi = 300
)

#######################
#### OUTPUT TABLE #####
#######################

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

########################
## VARAIBLE IMPORTANCE #
########################

# ----------------------------
# 1. Extract variable importance
# ----------------------------
glm_imp <- varImp(glm_sp)$importance
glm_imp$variable <- rownames(glm_imp)

glm_imp <- glm_imp %>%
  rename(importance_raw = Overall) %>%
  mutate(
    importance = importance_raw / max(importance_raw, na.rm = TRUE) * 100,
    model = "GLM"
  ) %>%
  select(variable, importance, model)

rf_imp <- varImp(rf_sp)$importance
rf_imp$variable <- rownames(rf_imp)

rf_imp <- rf_imp %>%
  rename(importance_raw = Overall) %>%
  mutate(
    importance = importance_raw / max(importance_raw, na.rm = TRUE) * 100,
    model = "Random forest"
  ) %>%
  select(variable, importance, model)

imp_df <- bind_rows(glm_imp, rf_imp)

# ----------------------------
# 2. Nice predictor labels
# ----------------------------
nice_labels <- c(
  tas = "Temperature",
  rainfall = "Rainfall",
  sfcWind = "Wind speed",
  groundfrost = "Ground frost",
  moorland_habitat = "Moorland habitat",
  aspect_conc = "Aspect",
  DTM_100 = "Elevation",
  Slope_mean = "Slope",
  Coast_dist = "Distance to coast",
  Roads_dist = "Distance to roads",
  Urban_dist = "Distance to urban",
  Lake_dist2ha = "Distance to lakes",
  dist_feeding_ground = "Distance to feeding ground",
  log_effort = "Log effort"
)

imp_df$variable_lab <- nice_labels[imp_df$variable]

# order predictors by mean importance across models
var_order <- imp_df %>%
  group_by(variable_lab) %>%
  summarise(mean_imp = mean(importance, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_imp) %>%
  pull(variable_lab)

imp_df$variable_lab <- factor(imp_df$variable_lab, levels = var_order)

# ----------------------------
# 3. Nice plot
# ----------------------------
p <- ggplot(imp_df, aes(x = importance, y = variable_lab, fill = model)) +
  geom_col(width = 0.72, show.legend = FALSE) +
  facet_wrap(~ model, ncol = 2) +
  scale_x_continuous(limits = c(0, 105), expand = c(0,0)) +
  scale_fill_manual(values = c("GLM" = "#4C78A8", "Random forest" = "#F58518")) +
  labs(
    title = "Relative predictor importance by model type",
    subtitle = "Importance scaled to 100 within each model",
    x = "Relative importance",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    panel.spacing = unit(1.2, "lines")
  )

p

dir.create(file.path("./Plots/", "Model_importance"), showWarnings = FALSE)

ggsave(
  "./Plots/Model_importance/model_variable_importance_comparison.png",
  width = 10,
  height = 7,
  dpi = 300
)

# ----------------------------
# sensitivity and specificity across all thresholds
# ----------------------------

# ROC objects from cross-validated predictions
roc_glm <- roc(glm_sp$pred$obs, glm_sp$pred$presence)
roc_rf  <- roc(rf_sp$pred$obs, rf_sp$pred$presence)

# extract threshold curves
glm_coords <- coords(
  roc_glm,
  x = "all",
  ret = c("threshold", "sensitivity", "specificity"),
  transpose = FALSE
)

rf_coords <- coords(
  roc_rf,
  x = "all",
  ret = c("threshold", "sensitivity", "specificity"),
  transpose = FALSE
)

# convert to data frames and tag model
glm_df <- as.data.frame(glm_coords)
glm_df$model <- "GLM"

rf_df <- as.data.frame(rf_coords)
rf_df$model <- "Random forest"

# combine
curve_df <- bind_rows(glm_df, rf_df)

# make sure these are plain numeric
curve_df$threshold <- as.numeric(curve_df$threshold)
curve_df$sensitivity <- as.numeric(curve_df$sensitivity)
curve_df$specificity <- as.numeric(curve_df$specificity)

# long format for plotting
curve_long <- curve_df %>%
  pivot_longer(
    cols = c(sensitivity, specificity),
    names_to = "metric",
    values_to = "value"
  )


best_glm <- coords(
  roc_glm,
  "best",
  best.method = "youden",
  ret = c("threshold", "Sensitivity", "Specificity"),
  transpose = FALSE
)

best_rf <- coords(
  roc_rf,
  "best",
  best.method = "youden",
  ret = c("threshold", "sensitivity", "specificity"),
  transpose = FALSE
)

best_thresh_df <- data.frame(
  model = c("GLM", "Random forest"),
  threshold = c(
    as.numeric(best_glm[, "threshold"]),
    as.numeric(best_rf[, "threshold"])
  )
)

p_thresh <- ggplot(curve_long, aes(x = threshold, y = value, colour = model)) +
  geom_line(linewidth = 1.1) +
  geom_vline(
    data = best_thresh_df,
    aes(xintercept = threshold, colour = model),
    linetype = "dashed",
    linewidth = 0.8,
    show.legend = FALSE
  ) +
  geom_text(
    data = best_thresh_df,
    aes(
      x = threshold,
      y = 0.9,
      label = paste0("Opt: ", round(threshold, 2)),
      colour = model
    ),
    angle = 90,
    vjust = -0.4,
    size = 3,
    show.legend = FALSE
  ) +
  facet_wrap(~ metric, ncol = 1) +
  scale_colour_manual(
    values = c("GLM" = "#4C78A8", "Random forest" = "#F58518")
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Sensitivity and specificity across classification thresholds",
    subtitle = "Spatial cross-validation comparison of GLM and Random Forest models",
    x = "Probability threshold",
    y = "Metric value",
    colour = "Model"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey30"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

p_thresh

dir.create(file.path("./Plots/", "Sens_spec_threshold"), showWarnings = FALSE)

ggsave(
  "./Plots/Sens_spec_threshold/threshold_sensitivity_specificity_glm_vs_rf.png",
  plot = p_thresh,
  width = 8,
  height = 8,
  dpi = 300
)

# ----------------------------
# 2024 Projection raster
# ----------------------------

library(terra)

all_files <- list.files(
  "./Data/Processed_data/",
  pattern = "\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)

template_100m <- rast("./Data/Processed_data/template_raster_100m.tif")

# 2024 climate
tmp_2024 <- all_files[grep("tas_.*_2024\\.tif$", all_files)]
prec_2024 <- all_files[grep("rainfall_.*_2024\\.tif$", all_files)]
wind_2024 <- all_files[grep("sfcWind_.*_2024\\.tif$", all_files)]
groundfrost_2024 <- all_files[grep("groundfrost_.*_2024\\.tif$", all_files)]

# 2024 habitat
hab_2024 <- all_files[grep("LC_2024_.*%_habit.*100m\\.tif$", all_files)]
feed_2024 <- all_files[grep("LC_2024_.*dist_to_twite_feeding_.*100m\\.tif$", all_files)]

# static layers
terrain <- all_files[grep("(DTM|slope_mean|aspect).*100m", all_files)]
distance <- all_files[grep("(road|urban|coast).*100m", all_files)]
lake <- all_files[grep("lake_dist_100m_2ha\\.tif$", all_files)]

distance <- c(distance, lake)

# read rasters
clim_2024_1km <- rast(c(tmp_2024, prec_2024, wind_2024, groundfrost_2024))
clim_2024_100m <- resample(clim_2024_1km, template_100m, method = "near")

hab_2024_r <- rast(hab_2024)
terrain_r <- rast(terrain)
distance_r <- rast(distance)
feed_r <- rast(feed_2024)

# check habitat layer names first
names(hab_2024_r)

moorland_2024 <- hab_2024_r[["Bog_%_habitat"]] +
  hab_2024_r[["Heather_%_habitat"]] +
  hab_2024_r[["Heather grassland_%_habitat"]] +
  hab_2024_r[["Acid grassland_%_habitat"]]

names(moorland_2024) <- "moorland_habitat"


# Effort becomes constant

log_effort_const <- template_100m
values(log_effort_const) <- median(mod_df$log_effort, na.rm = TRUE)
names(log_effort_const) <- "log_effort"

pred_2024_all <- c(
  clim_2024_100m,
  terrain_r,
  distance_r,
  moorland_2024,
  feed_r,
  log_effort_const
)

# Names must be identicle and in the same order
names(pred_2024_all)
names(pred_2024_all) <- gsub("_2024", "", names(pred_2024_all))

glm_vars <- glm_sp$coefnames
rf_vars  <- rf_sp$coefnames

names(pred_2024_all)
glm_vars
rf_vars

pred_2024_glm <- pred_2024_all[[glm_vars]]
pred_2024_rf  <- pred_2024_all[[rf_vars]]

# Distance control

cap_raster <- function(r, min_val, max_val) {
  r[r < min_val] <- min_val
  r[r > max_val] <- max_val
  r
}

train_ranges <- data.frame(
  min = sapply(mod_df[, glm_vars], min),
  max = sapply(mod_df[, glm_vars], max)
)

for (v in glm_vars) {
  pred_2024_glm[[v]] <- clamp(
    pred_2024_glm[[v]],
    lower = train_ranges[v, "min"],
    upper = train_ranges[v, "max"]
  )
}

# GLM prediction

hs_glm_2024 <- predict(
  pred_2024_glm,
  glm_sp$finalModel,
  type = "response",
  na.rm = TRUE
)

names(hs_glm_2024) <- "Twite_suitability_GLM_2024"

terra::plot(hs_glm_2024)

dir.create(file.path("./Plots/", "prediction_rasters"), showWarnings = FALSE)

out_file <- "./Plots/prediction_rasters/GLM_Pred.png"

png(filename = out_file, width = 1500, height = 2000, res = 300)

terra::plot(hs_glm_2024)

dev.off()

# RF prediction

rf_fun <- function(model, data) {
  predict(model, data, type = "prob")[, "presence"]
}

train_ranges <- data.frame(
  min = sapply(mod_df[, rf_vars], min),
  max = sapply(mod_df[, rf_vars], max)
)

for (v in rf_vars) {
  pred_2024_rf[[v]] <- clamp(
    pred_2024_rf[[v]],
    lower = train_ranges[v, "min"],
    upper = train_ranges[v, "max"]
  )
}

hs_rf_2024 <- predict(
  pred_2024_rf,
  rf_sp$finalModel,
  fun = rf_fun,
  na.rm = TRUE
)

terra::plot(hs_rf_2024)
names(hs_rf_2024) <- "Twite_suitability_RF_2024"

out_file <- "./Plots/prediction_rasters/RF_Pred.png"

png(filename = out_file, width = 1500, height = 2000, res = 300)

plot(hs_rf_2024)

dev.off()

