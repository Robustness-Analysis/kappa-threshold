# /usr/bin/env/Rscript

# Packages that need to be loaded
pacman::p_load(caret, iml, citation, dplyr, earth, lime, data.table)

# Set the seed 
set.seed(1)

# Load parameters
load_parameters <- function(params_file) {
  # Datasets from command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  datasets <- args
  # Parameters
  params <- read.csv(params_file, stringsAsFactors = FALSE)
  # Control parameters
  control = trainControl(method = "none", number = 1)
  list(
    datasets = datasets,
    #datasets = unlist(strsplit(subset(params, parameter == "dataset_name")$values, "\\|")),
    fold_names = unlist(strsplit(subset(params, parameter == "fold_name")$values, "\\|")),
    methods = unlist(strsplit(subset(params, parameter == "technique_name")$values, "\\|")),
    noise_level = as.numeric(unlist(strsplit(subset(params, parameter == "noise_level")$values, "\\|"))),
    instances = as.numeric(unlist(strsplit(subset(params, parameter == "instance_level")$values, "\\|"))),
    control = control
  )
}
 
# Model training function (instead of if-else block)
train_model <- function(method, train_df, control) {
  model_params <- list(
    "C5.0" = list(method = "C5.0"),
    "ctree" = list(method = "ctree"),
    "fda" = list(method = "fda"),
    "gbm" = list(method = "gbm"),
    "gcvEarth" = list(method = "gcvEarth"),
    "JRip" = list(method = "JRip"),
    "lvq" = list(method = "lvq"),
    "mlpML" = list(method = "mlpML"),
    "multinom" = list(method = "multinom", trControl = control, 
                     tuneGrid = expand.grid(decay = c(0)), MaxNWts = 10000),
    "naive_bayes" = list(method = "naive_bayes"),
    "PART" = list(method = "PART"),
    "rbfDDA" = list(method = "rbfDDA", trControl = control),
    "rda" = list(method = "rda"),
    "rf" = list(method = "rf"),
    "rpart" = list(method = "rpart"),
    "simpls" = list(method = "simpls"),
    "svmLinear" = list(method = "svmLinear"),
    "svmRadial" = list(method = "svmRadial"),
    "rfRules" = list(method = "rfRules"),
    "knn" = list(method = "knn", tuneGrid = expand.grid(k = 5:5), 
                preProcess = c("center", "scale"), trControl = control),
    "bayesglm" = list(method = "bayesglm", trControl = control)
  )
  
  params <- model_params[[method]]
  if(is.null(params)) {
    stop(paste("Unsupported method:", method))
  }
  
  do.call(caret::train, c(list(class ~ ., data = train_df), params))
}

# Obtain vector of altered instances
get_instances <- function(instances_df, index_list, index_counter, percent) {
  
  # Number of values to be altered (instances)
  sample_size = round(nrow(instances_df) * percent, 0)

  # Sample of ids that we want from the test df
  indices <- sample(instances_df[, index], sample_size)
  
  # Store vector of indices in list
  index_list[[index_counter]] <- indices
  index_counter = index_counter + 1
  
  # Return updated list and counter
  return(list(
    index_list = index_list,
    index_counter = index_counter,
    indices = indices
  ))
}

# Calculate predictions and generate confusion matrix
calculate_predictions <- function(fit, test_df, instances_df, noise) {
  cat("Calculate prediction\n")
  
  # Prediction without noise
  predict_unseen <- predict(fit, test_df)
  
  # Prediction with noise 
  noise_predict <- predict(fit, instances_df)
  
  # Confusion matrix
  cat("Confusion matrix\n")
  if(noise == 0) {
    conf_matrix <- caret::confusionMatrix(predict_unseen, predict_unseen)
  } else {
    conf_matrix <- caret::confusionMatrix(predict_unseen, noise_predict)
  }
  
  # Extract metrics from confusion matrix
  if(conf_matrix$overall["Kappa"] == "NaN") { 
    conf_matrix$overall["Kappa"] <- 1 
  }
  
  # Return the confusion matrix
  conf_matrix
}

# Instances-Noise
process_instances <- function(dataset, fold_index, method, mia, noise, percent, test_df, noise_df, fit, index_list, semaphore, index_counter) {
  # Print relevant information for the iteration
  cat("Dataset:", dataset, "\n")
  cat("Fold:", fold_index, "\n")
  cat("Method:", method, "\n")
  cat("Noise level: ", noise * 100, "%\n")
  cat("Percentage of altered instances: ", percent * 100, "%\n")

  # Create a new dataframe with the noise from noiseMIA_list
  noiselvl <- paste0("noise_", noise * 100)
  noise_df <- noiseMIA_list[[dataset]][[mia]][[noiselvl]]
  
  # Create a new df with the altered number of instances desired
  instances_df <- test_df
  
  # Convert to data.table and add indices efficiently
  setDT(test_df)[, index := .I]
  setDT(noise_df)[, index := .I]
  setDT(instances_df)[, index := .I]
  
  # Get instances indices only once if semaphore is TRUE
  indices <- NULL
  if(semaphore) {
    result <- get_instances(instances_df, index_list, index_counter, percent)
    index_list <- result$index_list
    index_counter <- result$index_counter
    indices <- result$indices
    semaphore <- FALSE
  }

  # Number of values to be altered (instances)
  sample_size <- round(nrow(instances_df) * percent, 0)
  
  # Sample of ids that we want from the test df
  indices <- tail(index_list, n = 1)
  indices <- indices[[1]][1:sample_size]
  
  cat("Alter ", percent * 100, "% of instances with noise\n")
  
  # Create auxiliary dataframe and manipulate rows efficiently with data.table
  aux_df <- noise_df[index %in% indices]
  
  # Remove and combine rows efficiently
  instances_df <- rbindlist(list(
    instances_df[!index %in% indices],
    aux_df
  ))
  
  # Reorder dataframe efficiently using data.table
  setorder(instances_df, index)
  
  # Remove index columns efficiently
  instances_df[, index := NULL]
  aux_df[, index := NULL]
  test_df[, index := NULL]
  noise_df[, index := NULL]
  
  # Calculate predictions and get confusion matrix
  conf_matrix <- calculate_predictions(fit, test_df, instances_df, noise)
  
  # Return results and updated control variables
  list(
    result = data.frame(
      dataset = dataset,
      fold = paste0("Fold_", fold_index),
      method = method,
      noise_level = noise,
      instance_level = percent,
      accuracy = conf_matrix$overall["Accuracy"],
      kappa = conf_matrix$overall["Kappa"],
      stringsAsFactors = FALSE
    ),
    index_list = index_list,
    index_counter = index_counter,
    semaphore = semaphore
  )
}

# Methods
process_method <- function(dataset, fold_index, train_df, test_df, method, mia_df, noise_level, instances, noiseMIA_list, index_list, index_counter, control) {
  # Train model
  fit <- train_model(method, train_df, control)
  # Get MIA
  mia <- subset(mia_df, dataset_name == dataset & technique == method)$most_important_attribute
  # Control variable for sampling
  semaphore <- TRUE
  # Store results using lapply over all combinations of noise_level and instances
  grid <- expand.grid(noise = noise_level, percent = instances, stringsAsFactors = FALSE)
  method_results <- apply(grid, 1, function(row) {
    noise <- as.numeric(row["noise"])
    percent <- as.numeric(row["percent"])
    noiselvl <- paste0("noise_", noise * 100)
    noise_df <- noiseMIA_list[[dataset]][[mia]][[noiselvl]]
    result <- process_instances(dataset, fold_index, method, mia, noise, percent, test_df, noise_df, fit, index_list, semaphore, index_counter)
    # Update stateful variables for next iteration
    index_list <<- result$index_list
    index_counter <<- result$index_counter
    semaphore <<- result$semaphore
    result$result
  })
  do.call(rbind, method_results)
}

# Folds
process_fold <- function(dataset, fold_index, df, methods, mia_df, noise_level, instances, noiseMIA_list, index_list, index_counter, control) {
  # Get train and test indices for this fold
  fold_train_indices <- createFolds(df$class, k = 5, list = TRUE, returnTrain = TRUE)
  fold_test_indices <- lapply(fold_train_indices, function(index) setdiff(1:nrow(df), index))
  train_indices <- fold_train_indices[[fold_index]]
  test_indices <- fold_test_indices[[fold_index]]
  train_df <- df[train_indices, ]
  test_df <- df[test_indices, ]
  # Print probability table for the fold
  cat("Dataset:", dataset, "\n")
  cat("Probability table:\n")
  prob_table <- prop.table(table(train_df$class))
  cat(paste(capture.output(prob_table), collapse="\n"), "\n")
  # Store results for all methods in this fold using lapply
  fold_results <- do.call(rbind, lapply(methods, function(method) {
    process_method(dataset, fold_index, train_df, test_df, method, mia_df, noise_level, instances, noiseMIA_list, index_list, index_counter, control)
  }))
  fold_results
}

# Load and extract parameters
parameters <- load_parameters("../data/files/parameters.csv")
datasets <- parameters$datasets
fold_names <- parameters$fold_names
methods <- parameters$methods
noise_level <- parameters$noise_level
instances <- parameters$instances
control <- parameters$control

# Load previous most important attribute table and
mia_df <- readRDS("../data/prev/mia_df.rds")
noiseMIA_list <- readRDS("../data/prev/noise_list.rds")

# Initialize empty results dataframe
results_df <- data.frame(
  dataset = character(),
  fold = character(),
  method = character(),
  noise_level = numeric(),
  instance_level = numeric(),
  accuracy = numeric(),
  kappa = numeric(),
  stringsAsFactors = FALSE
)

# Apply process_fold to each dataset using lapply
results_list <- lapply(datasets, function(dataset) {
  filename <- paste0("../data/datasets/", dataset, ".rds")
  df <- readRDS(filename)
  n_folds <- 5
  dataset_results <- do.call(rbind, lapply(1:n_folds, function(i) {
    process_fold(dataset, i, df, methods, mia_df, noise_level, instances, noiseMIA_list, index_list, index_counter, control)
  }))
  # Safeguard store by dataset
  out_filename <- paste0("../results/instances/by_dataset/", dataset, "_results.csv")
  write.csv(dataset_results, file = out_filename, row.names = FALSE)
  cat("Altered instances with noise recorded\n")
  cat("----------------\n")
  dataset_results
})
# Combine all results into a single data frame
results_df <- do.call(rbind, results_list)
out_filename <- paste0("../results/altered_instances_results.csv")
write.csv(results_df, file = out_filename, row.names = FALSE)

cat("****************\n")
cat("RESULTS RECORDED\n")
cat("****************\n")