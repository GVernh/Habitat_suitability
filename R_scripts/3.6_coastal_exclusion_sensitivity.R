# libs <- c("pROC", "caret", "ggplot2", "dplyr", "tidyr", "randomForest")
# 
# installed_libs <- libs %in% rownames(installed.packages())
# if (any(!installed_libs)) install.packages(libs[!installed_libs])
# 
# invisible(lapply(libs, library, character.only = TRUE))
# rm(list = ls())
# 
# #set.seed(123)
# 
# # -----------------------------
# # files
# # -----------------------------
# data_dir <- "./Data/Processed_data/Complete_datasets/coast_excluded"
# 
# csv_files <- list.files(
#   data_dir,
#   pattern = "\\.csv$",
#   full.names = TRUE
# )
# 
# # optional: sort in numeric order if filenames are like 2km.csv, 5km.csv, 10km.csv
# get_buffer_km <- function(x) {
#   as.numeric(gsub("km.*$", "", basename(tools::file_path_sans_ext(x))))
# }
# 
# csv_files <- csv_files[order(sapply(csv_files, get_buffer_km))]
# 
# # -----------------------------
# # modelling function
# # -----------------------------
# run_buffer_model <- function(file_path, block_size = 50000) {
#   
#   message("Running: ", basename(file_path))
#   
#   df <- read.csv(file_path)
#   
#   # derive moorland habitat
#   df$moorland_habitat <- df$Bog_._habitat +
#     df$Heather_._habitat +
#     df$Heather.grassland_._habitat +
#     df$Acid.grassland_._habitat
#   
#   df$Bog_._habitat <- NULL
#   df$Heather_._habitat <- NULL
#   df$Heather.grassland_._habitat <- NULL
#   df$Acid.grassland_._habitat <- NULL
#   
#   mod_df <- df
#   
#   mod_df$presence <- factor(
#     mod_df$presence,
#     levels = c(0, 1),
#     labels = c("absence", "presence")
#   )
#   
#   mod_df$year <- factor(mod_df$year)
#   
#   # spatial blocks
#   mod_df$x_block <- floor(mod_df$Easting / block_size)
#   mod_df$y_block <- floor(mod_df$Northing / block_size)
#   mod_df$block_id <- interaction(mod_df$x_block, mod_df$y_block, drop = TRUE)
#   
#   # reproducible fold assignment
#   #set.seed(seed)
#   unique_blocks <- unique(mod_df$block_id)
#   fold_lookup <- data.frame(
#     block_id = unique_blocks,
#     fold = sample(rep(1:5, length.out = length(unique_blocks)))
#   )
#   
#   mod_df <- merge(mod_df, fold_lookup, by = "block_id", all.x = TRUE)
#   mod_df <- mod_df[order(mod_df$year, mod_df$Easting, mod_df$Northing), ]
#   
#   index_list <- lapply(1:5, function(k) which(mod_df$fold != k))
#   index_out_list <- lapply(1:5, function(k) which(mod_df$fold == k))
#   
#   train_control_sp <- trainControl(
#     method = "cv",
#     number = 5,
#     index = index_list,
#     indexOut = index_out_list,
#     classProbs = TRUE,
#     summaryFunction = twoClassSummary,
#     savePredictions = "final"
#   )
#   
#   # -------------------------
#   # GLM
#   # -------------------------
#   glm_sp <- train(
#     presence ~ . - year - Easting - Northing - x_block - y_block - block_id - fold,
#     data = mod_df,
#     method = "glm",
#     family = "binomial",
#     metric = "ROC",
#     trControl = train_control_sp
#   )
#   
#   # -------------------------
#   # RF
#   # -------------------------
#   rf_sp <- train(
#     presence ~ . - year - Easting - Northing - x_block - y_block - block_id - fold,
#     data = mod_df,
#     method = "rf",
#     metric = "ROC",
#     tuneLength = 5,
#     trControl = train_control_sp
#   )
#   
#   # -------------------------
#   # summaries
#   # -------------------------
#   glm_res <- glm_sp$results
#   rf_res  <- rf_sp$results
#   rf_res  <- rf_res[rf_res$mtry == rf_sp$bestTune$mtry, ]
#   
#   # use only best-tuned RF predictions for confusion matrix
#   rf_pred_best <- rf_sp$pred
#   rf_pred_best <- rf_pred_best[rf_pred_best$mtry == rf_sp$bestTune$mtry, ]
#   
#   glm_cm <- confusionMatrix(
#     data = glm_sp$pred$pred,
#     reference = glm_sp$pred$obs
#   )
#   
#   rf_cm <- confusionMatrix(
#     data = rf_pred_best$pred,
#     reference = rf_pred_best$obs
#   )
#   
#   buffer_name <- tools::file_path_sans_ext(basename(file_path))
#   
#   model_summary <- data.frame(
#     Buffer = buffer_name,
#     Model = c("GLM", "Random forest"),
#     AUC = c(glm_res$ROC, rf_res$ROC),
#     Sensitivity = c(glm_res$Sens, rf_res$Sens),
#     Specificity = c(glm_res$Spec, rf_res$Spec),
#     Accuracy = c(glm_cm$overall["Accuracy"], rf_cm$overall["Accuracy"]),
#     Kappa = c(glm_cm$overall["Kappa"], rf_cm$overall["Kappa"]),
#     N = nrow(mod_df),
#     Presences = sum(mod_df$presence == "presence"),
#     Absences = sum(mod_df$presence == "absence")
#   )
#   
#   model_summary <- model_summary |>
#     mutate(across(where(is.numeric), round, 3))
#   
#   return(model_summary)
# }
# 
# # -----------------------------
# # run all files
# # -----------------------------
# all_results <- lapply(csv_files, run_buffer_model)
# all_results <- bind_rows(all_results)
# 
# print(all_results)
# 
# # -----------------------------
# # save
# # -----------------------------
# out_dir <- "./Plots/exclusion_sensitivity/"
# dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
# 
# write.csv(
#   all_results,
#   file.path(out_dir, "Summary_stats_all_buffers.csv"),
#   row.names = FALSE
# )




# libs <- c("pROC", "caret", "ggplot2", "dplyr", "tidyr", "randomForest")
# 
# installed_libs <- libs %in% rownames(installed.packages())
# if (any(!installed_libs)) install.packages(libs[!installed_libs])
# 
# invisible(lapply(libs, library, character.only = TRUE))
# rm(list = ls())
# 
# # -----------------------------
# # settings
# # -----------------------------
# data_dir <- "./Data/Processed_data/Complete_datasets/coast_excluded"
# out_dir <- "./Plots/exclusion_sensitivity/"
# 
# dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
# 
# n_iter <- 10
# block_size <- 50000  # 50 km
# 
# # -----------------------------
# # helper: extract buffer number
# # -----------------------------
# get_buffer_km <- function(x) {
#   as.numeric(gsub("km.*$", "", basename(tools::file_path_sans_ext(x))))
# }
# 
# # -----------------------------
# # list and sort dataset files
# # -----------------------------
# csv_files <- list.files(
#   data_dir,
#   pattern = "\\.csv$",
#   full.names = TRUE
# )
# 
# csv_files <- csv_files[order(sapply(csv_files, get_buffer_km))]
# 
# # -----------------------------
# # modelling function
# # -----------------------------
# run_buffer_model <- function(file_path, block_size = 50000, seed = 123) {
#   
#   message("Running: ", basename(file_path), " | seed = ", seed)
#   
#   df <- read.csv(file_path)
#   
#   # build moorland habitat variable
#   df$moorland_habitat <- df$Bog_._habitat +
#     df$Heather_._habitat +
#     df$Heather.grassland_._habitat +
#     df$Acid.grassland_._habitat
#   
#   # remove original habitat columns
#   df$Bog_._habitat <- NULL
#   df$Heather_._habitat <- NULL
#   df$Heather.grassland_._habitat <- NULL
#   df$Acid.grassland_._habitat <- NULL
#   
#   mod_df <- df
#   
#   # response as factor for caret
#   mod_df$presence <- factor(
#     mod_df$presence,
#     levels = c(0, 1),
#     labels = c("absence", "presence")
#   )
#   
#   mod_df$year <- factor(mod_df$year)
#   
#   # -----------------------------
#   # spatial blocks
#   # -----------------------------
#   mod_df$x_block <- floor(mod_df$Easting / block_size)
#   mod_df$y_block <- floor(mod_df$Northing / block_size)
#   mod_df$block_id <- interaction(mod_df$x_block, mod_df$y_block, drop = TRUE)
#   
#   # reproducible fold assignment
#   set.seed(seed)
#   unique_blocks <- unique(mod_df$block_id)
#   
#   fold_lookup <- data.frame(
#     block_id = unique_blocks,
#     fold = sample(rep(1:5, length.out = length(unique_blocks)))
#   )
#   
#   mod_df <- merge(mod_df, fold_lookup, by = "block_id", all.x = TRUE)
#   mod_df <- mod_df[order(mod_df$year, mod_df$Easting, mod_df$Northing), ]
#   
#   # spatial CV index
#   index_list <- lapply(1:5, function(k) which(mod_df$fold != k))
#   index_out_list <- lapply(1:5, function(k) which(mod_df$fold == k))
#   
#   train_control_sp <- trainControl(
#     method = "cv",
#     number = 5,
#     index = index_list,
#     indexOut = index_out_list,
#     classProbs = TRUE,
#     summaryFunction = twoClassSummary,
#     savePredictions = "final"
#   )
#   
#   # -----------------------------
#   # GLM
#   # -----------------------------
#   set.seed(seed)
#   glm_sp <- train(
#     presence ~ . - year - Easting - Northing - x_block - y_block - block_id - fold,
#     data = mod_df,
#     method = "glm",
#     family = "binomial",
#     metric = "ROC",
#     trControl = train_control_sp
#   )
#   
#   # -----------------------------
#   # Random forest
#   # -----------------------------
#   set.seed(seed)
#   rf_sp <- train(
#     presence ~ . - year - Easting - Northing - x_block - y_block - block_id - fold,
#     data = mod_df,
#     method = "rf",
#     metric = "ROC",
#     tuneLength = 5,
#     trControl = train_control_sp
#   )
#   
#   # -----------------------------
#   # model summaries
#   # -----------------------------
#   glm_res <- glm_sp$results
#   rf_res  <- rf_sp$results
#   rf_res  <- rf_res[rf_res$mtry == rf_sp$bestTune$mtry, ]
#   
#   # use only best-tuned RF predictions
#   rf_pred_best <- rf_sp$pred
#   rf_pred_best <- rf_pred_best[rf_pred_best$mtry == rf_sp$bestTune$mtry, ]
#   
#   glm_cm <- confusionMatrix(
#     data = glm_sp$pred$pred,
#     reference = glm_sp$pred$obs
#   )
#   
#   rf_cm <- confusionMatrix(
#     data = rf_pred_best$pred,
#     reference = rf_pred_best$obs
#   )
#   
#   buffer_name <- tools::file_path_sans_ext(basename(file_path))
#   
#   model_summary <- data.frame(
#     Buffer = buffer_name,
#     Model = c("GLM", "Random forest"),
#     AUC = c(glm_res$ROC, rf_res$ROC),
#     Sensitivity = c(glm_res$Sens, rf_res$Sens),
#     Specificity = c(glm_res$Spec, rf_res$Spec),
#     Accuracy = c(glm_cm$overall["Accuracy"], rf_cm$overall["Accuracy"]),
#     Kappa = c(glm_cm$overall["Kappa"], rf_cm$overall["Kappa"]),
#     N = nrow(mod_df),
#     Presences = sum(mod_df$presence == "presence"),
#     Absences = sum(mod_df$presence == "absence")
#   )
#   
#   return(model_summary)
# }
# 
# # -----------------------------
# # run all buffers x iterations
# # -----------------------------
# all_results_list <- list()
# counter <- 1
# 
# for (file in csv_files) {
#   
#   buffer_name <- tools::file_path_sans_ext(basename(file))
#   
#   for (i in 1:n_iter) {
#     
#     seed_i <- 1000 + i
#     
#     message("Buffer: ", buffer_name, " | Iteration: ", i, " / ", n_iter)
#     
#     res <- run_buffer_model(
#       file_path = file,
#       block_size = block_size,
#       seed = seed_i
#     )
#     
#     res$Iteration <- i
#     res$Seed <- seed_i
#     
#     all_results_list[[counter]] <- res
#     counter <- counter + 1
#   }
# }
# 
# all_results <- bind_rows(all_results_list)
# 
# # round raw results for readability
# all_results <- all_results |>
#   mutate(across(where(is.numeric), round, 3))
# 
# print(all_results)
# 
# # -----------------------------
# # summary across iterations
# # -----------------------------
# summary_results <- all_results %>%
#   group_by(Buffer, Model) %>%
#   summarise(
#     AUC_mean = mean(AUC, na.rm = TRUE),
#     AUC_sd = sd(AUC, na.rm = TRUE),
#     Sensitivity_mean = mean(Sensitivity, na.rm = TRUE),
#     Sensitivity_sd = sd(Sensitivity, na.rm = TRUE),
#     Specificity_mean = mean(Specificity, na.rm = TRUE),
#     Specificity_sd = sd(Specificity, na.rm = TRUE),
#     Accuracy_mean = mean(Accuracy, na.rm = TRUE),
#     Accuracy_sd = sd(Accuracy, na.rm = TRUE),
#     Kappa_mean = mean(Kappa, na.rm = TRUE),
#     Kappa_sd = sd(Kappa, na.rm = TRUE),
#     N = mean(N, na.rm = TRUE),
#     Presences = mean(Presences, na.rm = TRUE),
#     Absences = mean(Absences, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   mutate(across(where(is.numeric), round, 3))
# 
# print(summary_results)
# 
# # -----------------------------
# # save outputs
# # -----------------------------
# write.csv(
#   all_results,
#   file.path(out_dir, "Summary_stats_all_runs.csv"),
#   row.names = FALSE
# )
# 
# write.csv(
#   summary_results,
#   file.path(out_dir, "Summary_stats_buffer_means_sd.csv"),
#   row.names = FALSE
# )

libs <- c("pROC", "caret", "ggplot2", "dplyr", "tidyr", "randomForest")

installed_libs <- libs %in% rownames(installed.packages())
if (any(!installed_libs)) install.packages(libs[!installed_libs])

invisible(lapply(libs, library, character.only = TRUE))
rm(list = ls())

# -----------------------------
# settings
# -----------------------------
data_dir <- "./Data/Processed_data/Complete_datasets/coast_excluded"
out_dir <- "./Plots/exclusion_sensitivity/"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

n_iter <- 10
block_size <- 50000  # 50 km

# -----------------------------
# helper: extract buffer number
# -----------------------------
get_buffer_km <- function(x) {
  as.numeric(gsub("km.*$", "", basename(tools::file_path_sans_ext(x))))
}

# -----------------------------
# list and sort dataset files
# -----------------------------
csv_files <- list.files(
  data_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

csv_files <- csv_files[order(sapply(csv_files, get_buffer_km))]

# -----------------------------
# modelling function
# -----------------------------
run_buffer_model <- function(file_path, block_size = 50000, seed = 123) {
  
  message("Running: ", basename(file_path), " | seed = ", seed)
  
  df <- read.csv(file_path)
  
  # build moorland habitat variable
  df$moorland_habitat <- df$Bog_._habitat +
    df$Heather_._habitat +
    df$Heather.grassland_._habitat +
    df$Acid.grassland_._habitat
  
  # remove original habitat columns
  df$Bog_._habitat <- NULL
  df$Heather_._habitat <- NULL
  df$Heather.grassland_._habitat <- NULL
  df$Acid.grassland_._habitat <- NULL
  
  mod_df <- df
  
  # IMPORTANT:
  # Keep absence first and presence second so GLM models P(presence)
  mod_df$presence <- factor(
    mod_df$presence,
    levels = c(0, 1),
    labels = c("absence", "presence")
  )
  
  mod_df$year <- factor(mod_df$year)
  
  # -----------------------------
  # spatial blocks
  # -----------------------------
  mod_df$x_block <- floor(mod_df$Easting / block_size)
  mod_df$y_block <- floor(mod_df$Northing / block_size)
  mod_df$block_id <- interaction(mod_df$x_block, mod_df$y_block, drop = TRUE)
  
  # reproducible fold assignment
  set.seed(seed)
  unique_blocks <- unique(mod_df$block_id)
  
  fold_lookup <- data.frame(
    block_id = unique_blocks,
    fold = sample(rep(1:5, length.out = length(unique_blocks)))
  )
  
  mod_df <- merge(mod_df, fold_lookup, by = "block_id", all.x = TRUE)
  mod_df <- mod_df[order(mod_df$year, mod_df$Easting, mod_df$Northing), ]
  
  # spatial CV index
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
  
  # -----------------------------
  # GLM
  # -----------------------------
  set.seed(seed)
  glm_sp <- train(
    presence ~ . - year - Easting - Northing - x_block - y_block - block_id - fold,
    data = mod_df,
    method = "glm",
    family = "binomial",
    metric = "ROC",
    trControl = train_control_sp
  )
  
  # -----------------------------
  # Random forest
  # -----------------------------
  set.seed(seed)
  rf_sp <- train(
    presence ~ . - year - Easting - Northing - x_block - y_block - block_id - fold,
    data = mod_df,
    method = "rf",
    metric = "ROC",
    tuneLength = 5,
    trControl = train_control_sp
  )
  
  # -----------------------------
  # thresholded model summaries
  # -----------------------------
  
  # ---- GLM ----
  glm_pred <- glm_sp$pred
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
  
  glm_pred$pred_thresh <- factor(
    glm_pred$pred_thresh,
    levels = c("absence", "presence")
  )
  
  glm_cm <- confusionMatrix(
    data = glm_pred$pred_thresh,
    reference = glm_pred$obs,
    positive = "presence"
  )
  
  # ---- RF ----
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
  
  rf_pred$pred_thresh <- factor(
    rf_pred$pred_thresh,
    levels = c("absence", "presence")
  )
  
  rf_cm <- confusionMatrix(
    data = rf_pred$pred_thresh,
    reference = rf_pred$obs,
    positive = "presence"
  )
  
  # -----------------------------
  # output row block
  # -----------------------------
  buffer_name <- tools::file_path_sans_ext(basename(file_path))
  
  model_summary <- data.frame(
    Buffer = buffer_name,
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
    ),
    TSS = c(
      glm_cm$byClass["Sensitivity"] + glm_cm$byClass["Specificity"] - 1,
      rf_cm$byClass["Sensitivity"] + rf_cm$byClass["Specificity"] - 1
    ),
    N = nrow(mod_df),
    Presences = sum(mod_df$presence == "presence"),
    Absences = sum(mod_df$presence == "absence")
  )
  
  return(model_summary)
}

# -----------------------------
# run all buffers x iterations
# -----------------------------
all_results_list <- list()
counter <- 1

for (file in csv_files) {
  
  buffer_name <- tools::file_path_sans_ext(basename(file))
  
  for (i in 1:n_iter) {
    
    seed_i <- 1000 + i
    
    message("Buffer: ", buffer_name, " | Iteration: ", i, " / ", n_iter)
    
    res <- run_buffer_model(
      file_path = file,
      block_size = block_size,
      seed = seed_i
    )
    
    res$Iteration <- i
    res$Seed <- seed_i
    
    all_results_list[[counter]] <- res
    counter <- counter + 1
  }
}

all_results <- bind_rows(all_results_list)

# round raw results for readability
all_results <- all_results |>
  mutate(across(where(is.numeric), round, 3))

print(all_results)

# -----------------------------
# summary across iterations
# -----------------------------
summary_results <- all_results %>%
  group_by(Buffer, Model) %>%
  summarise(
    AUC_mean = mean(AUC, na.rm = TRUE),
    AUC_sd = sd(AUC, na.rm = TRUE),
    Threshold_mean = mean(Threshold, na.rm = TRUE),
    Threshold_sd = sd(Threshold, na.rm = TRUE),
    Sensitivity_mean = mean(Sensitivity, na.rm = TRUE),
    Sensitivity_sd = sd(Sensitivity, na.rm = TRUE),
    Specificity_mean = mean(Specificity, na.rm = TRUE),
    Specificity_sd = sd(Specificity, na.rm = TRUE),
    Accuracy_mean = mean(Accuracy, na.rm = TRUE),
    Accuracy_sd = sd(Accuracy, na.rm = TRUE),
    Kappa_mean = mean(Kappa, na.rm = TRUE),
    Kappa_sd = sd(Kappa, na.rm = TRUE),
    TSS_mean = mean(TSS, na.rm = TRUE),
    TSS_sd = sd(TSS, na.rm = TRUE),
    N = mean(N, na.rm = TRUE),
    Presences = mean(Presences, na.rm = TRUE),
    Absences = mean(Absences, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), round, 3))

print(summary_results)

# -----------------------------
# save outputs
# -----------------------------
write.csv(
  all_results,
  file.path(out_dir, "Summary_stats_all_runs.csv"),
  row.names = FALSE
)

write.csv(
  summary_results,
  file.path(out_dir, "Summary_stats_buffer_means_sd.csv"),
  row.names = FALSE
)

# ====================
# SELECT DESIRED MODEL
# ====================

selected_df <- read.csv("./data/Processed_data/Complete_datasets/coast_excluded/15km.csv")

write.csv(selected_df, "./data/Processed_data/Complete_datasets/complete_dataset_2013_1999_survey_100m_15km_excl_SELECTED.csv", row.names = F)

# -----------------------------
# optional plot: AUC comparison
# -----------------------------
# p_auc <- ggplot(summary_results,
#                 aes(x = Buffer, y = AUC_mean, colour = Model, group = Model)) +
#   geom_point(size = 3) +
#   geom_line(linewidth = 1) +
#   geom_errorbar(
#     aes(ymin = AUC_mean - AUC_sd, ymax = AUC_mean + AUC_sd),
#     width = 0.15
#   ) +
#   theme_minimal(base_size = 12) +
#   labs(
#     title = "Mean AUC across repeated runs",
#     x = "Coastal exclusion buffer",
#     y = "Mean AUC ± SD",
#     colour = "Model"
#   )
# 
# ggsave(
#   filename = file.path(out_dir, "AUC_buffer_comparison.png"),
#   plot = p_auc,
#   width = 8,
#   height = 6,
#   dpi = 300
# )