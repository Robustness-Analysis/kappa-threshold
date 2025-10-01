#!/usr/bin/env Rscript

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  caret,
  iml,
  citation,
  dplyr,       # for data manipulation
  earth,
  lime,
  data.table,
  here         # for file path handling
)

set.seed(1) # Set seed for reproducibility

# Load configuration files and datasets
load_configuration <- function() {
  # Get dataset names from command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  datasets <- args
  
  # Load configuration files
  config <- list(
    dataset_names <- readRDS(here("data", "files", "dataset_name.rds")),
    fold_names = <- readRDS(here("data", "files", "fold_name.rds")),
    techniques <- readRDS(here("data", "files", "technique_name.rds")),
    noise_level = c(0.1, 0.2, 0.3)
  )
  
  # Load previous results
  results <- list(
    mia_df = readRDS("results/most_important_attr/mia_df.rds"),
    noiseMIA_list = readRDS("results/noise/noise_list.rds")
  )
  
  # Return a combined list
  return(list(
    datasets = datasets,
    config = config,
    results = results
  ))
}

# Create cross-validation folds for a dataset
create_folds <- function(df, k = 5) {
  # Create fold indices for training
  fold_train_indices <- createFolds(df$class, k = k, list = TRUE, returnTrain = TRUE)
  
  # Create fold indices for testing
  fold_test_indices <- lapply(fold_train_indices, function(index) {
    setdiff(1:nrow(df), index)
  })
  
  # Return both training and testing indices
  return(list(
    train = fold_train_indices,
    test = fold_test_indices
  ))
}

# Load a dataset by name
load_dataset <- function(dataset_name) {
  filename <- paste0("datasets/", dataset_name, ".rds")
  df <- readRDS(filename)
  return(df)
}

# Train a model based on the specified method
train_model <- function(train_df, method, control = NULL) {
  print(paste("Training method:", method))
  
  if (method == "knn") {
    fit <- caret::train(class ~ ., 
                       data = train_df, 
                       method = "knn", 
                       tuneGrid = expand.grid(k = 5:5), 
                       preProcess = c("center", "scale"), 
                       trControl = control)
  } else if (method == "bayesglm") {
    fit <- caret::train(class ~ ., 
                       data = train_df, 
                       method = "bayesglm", 
                       trControl = control)
  } else if (method == "C5.0") {
    fit <- caret::train(class ~ ., data = train_df, method = "C5.0")
  } else if (method == "ctree") {
    fit <- caret::train(class ~ ., data = train_df, method = "ctree")
  } else if (method == "fda") {
    fit <- caret::train(class ~ ., data = train_df, method = "fda")
  } else if (method == "gbm") {
    fit <- caret::train(class ~ ., data = train_df, method = "gbm")
  } else if (method == "gcvEarth") {
    fit <- caret::train(class ~ ., data = train_df, method = "gcvEarth")
  } else if (method == "JRip") {
    fit <- caret::train(class ~ ., data = train_df, method = "JRip")
  } else if (method == "lvq") {
    fit <- caret::train(class ~ ., data = train_df, method = "lvq")
  } else if (method == "mlpML") {
    fit <- caret::train(class ~ ., data = train_df, method = "mlpML")
  } else if (method == "multinom") {
    fit <- caret::train(class ~ ., 
                       data = train_df, 
                       method = "multinom", 
                       trControl = control, 
                       tuneGrid = expand.grid(decay = c(0)), 
                       MaxNWts = 10000)
  } else if (method == "naive_bayes") {
    fit <- caret::train(class ~ ., data = train_df, method = "naive_bayes")
  } else if (method == "PART") {
    fit <- caret::train(class ~ ., data = train_df, method = "PART")
  } else if (method == "rbfDDA") {
    fit <- caret::train(class ~ ., 
                       data = train_df, 
                       method = "rbfDDA", 
                       trControl = control)
  } else if (method == "rda") {
    fit <- caret::train(class ~ ., data = train_df, method = "rda")
  } else if (method == "rf") {
    fit <- caret::train(class ~ ., data = train_df, method = "rf")
  } else if (method == "rpart") {
    fit <- caret::train(class ~ ., data = train_df, method = "rpart")
  } else if (method == "simpls") {
    fit <- caret::train(class ~ ., data = train_df, method = "simpls")
  } else if (method == "svmLinear") {
    fit <- caret::train(class ~ ., data = train_df, method = "svmLinear")
  } else if (method == "svmRadial") {
    fit <- caret::train(class ~ ., data = train_df, method = "svmRadial")
  } else if (method == "rfRules") {
    fit <- caret::train(class ~ ., data = train_df, method = "rfRules")
  } else {
    stop(paste("Unsupported method:", method))
  }
  
  print("Training successful")
  return(fit)
}

# Get the most important attribute for a dataset and method
get_most_important_attribute <- function(mia_df, dataset_name, method) {
  mia <- subset(mia_df, dataset_name == dataset_name & technique == method)$most_important_attribute
  return(mia)
}

# Sample indices for applying noise
generate_instance_indices <- function(df, percent = 1.0) {
  sample_size <- round(nrow(df) * percent, 0)
  indices <- sample(rownames(df), sample_size)
  return(indices)
}

# Apply noise to selected instances
apply_noise_to_instances <- function(test_df, noise_df, indices) {
  # Add index column to both dataframes
  test_df_with_index <- cbind(index = rownames(test_df), test_df)
  noise_df_with_index <- cbind(index = rownames(noise_df), noise_df)
  
  # Get the noisy instances for the selected indices
  noisy_instances <- noise_df_with_index[indices, ]
  
  # Create a new dataframe without the instances to be replaced
  clean_instances <- test_df_with_index[!(test_df_with_index$index %in% indices), ]
  
  # Combine clean and noisy instances
  modified_df <- rbind(clean_instances, noisy_instances)
  
  # Sort by index
  modified_df$index <- as.numeric(as.character(modified_df$index))
  modified_df <- modified_df %>% arrange(index)
  rownames(modified_df) <- modified_df[, 1]
  
  # Remove index column
  modified_df <- modified_df %>% select(-index)
  
  return(modified_df)
}

# Evaluate model with original and noisy data
evaluate_model <- function(fit, test_df, modified_df, noise_level) {
  # Make predictions
  predictions_original <- predict(fit, test_df)
  predictions_modified <- predict(fit, modified_df)
  
  # Create confusion matrix
  if (noise_level == 0) {
    conf_matrix <- caret::confusionMatrix(predictions_original, predictions_original)
  } else {
    conf_matrix <- caret::confusionMatrix(predictions_original, predictions_modified)
  }
  
  # Handle NaN in Kappa
  if (is.nan(conf_matrix$overall["Kappa"])) {
    conf_matrix$overall["Kappa"] <- 1
  }
  
  return(list(
    original_predictions = predictions_original,
    modified_predictions = predictions_modified,
    confusion_matrix = conf_matrix
  ))
}

# Process a single combination of noise level and instance percentage
process_noise_instance_combination <- function(dataset, fold_index, test_df, noise_df, 
                                              method, noise, percent, fit, indices) {
  # Print progress information
  print(paste("Dataset:", dataset))
  print(paste("Fold:", fold_index))
  print(paste("Method:", method))
  print(paste0("Noise level: ", noise * 100, "%"))
  print(paste0("Percentage of altered instances: ", percent * 100, "%"))
  
  # Calculate how many instances to modify
  sample_size <- round(nrow(test_df) * percent, 0)
  
  # Select indices to modify (take the first n from the pre-generated indices)
  selected_indices <- indices[1:sample_size]
  
  # Apply noise to selected instances
  modified_df <- apply_noise_to_instances(test_df, noise_df, selected_indices)
  
  # Evaluate the model
  evaluation <- evaluate_model(fit, test_df, modified_df, noise)
  
  # Return the results
  return(list(
    modified_df = modified_df,
    evaluation = evaluation
  ))
}

# Process all noise levels and instance percentages for a method
process_method <- function(dataset, fold_index, train_df, test_df, method, 
                          noise_levels, noise_names, instances, instances_names,
                          mia_df, noiseMIA_list, control, index_list, index_counter) {
  # Train the model
  if (method == "knn") {
    # KNN is trained on the full dataset in the original script
    fit <- train_model(train_df, method, control)
  } else {
    fit <- train_model(train_df, method, control)
  }
  
  # Get the most important attribute
  mia <- get_most_important_attribute(mia_df, dataset, method)
  
  # Initialize results storage
  noise_results <- list()
  noise_CM <- list()
  
  # Flag to ensure we sample indices only once
  semaphore <- TRUE
  local_index_counter <- index_counter
  
  # Process each noise level
  for (noise_idx in 1:length(noise_levels)) {
    noise <- noise_levels[noise_idx]
    noise_name <- noise_names[noise_idx]
    
    # Get the dataframe with noise applied
    noiselvl <- paste0("noise_", noise * 100)
    noise_df <- noiseMIA_list[[dataset]][[mia]][[noiselvl]]
    
    # Initialize results for this noise level
    instances_results <- list()
    confMatrix_results <- list()
    
    # Generate indices for consistent instance selection
    if (semaphore) {
      indices <- generate_instance_indices(test_df)
      index_list[[local_index_counter]] <- indices
      local_index_counter <- local_index_counter + 1
      semaphore <- FALSE
    } else {
      # Use previously generated indices
      indices <- index_list[[local_index_counter - 1]]
    }
    
    # Process each instance percentage
    for (inst_idx in 1:length(instances)) {
      percent <- instances[inst_idx]
      
      # Process this combination
      result <- process_noise_instance_combination(
        dataset, fold_index, test_df, noise_df, 
        method, noise, percent, fit, indices
      )
      
      # Store results
      instances_results[[inst_idx]] <- result$modified_df
      confMatrix_results[[inst_idx]] <- result$evaluation$confusion_matrix
    }
    
    # Name the results
    names(instances_results) <- instances_names
    names(confMatrix_results) <- instances_names
    
    # Store in noise-level lists
    noise_results[[noise_idx]] <- instances_results
    noise_CM[[noise_idx]] <- confMatrix_results
  }
  
  # Name the noise-level results
  names(noise_results) <- noise_names
  names(noise_CM) <- noise_names
  
  # Return all results and the updated index counter
  return(list(
    noise_results = noise_results,
    noise_CM = noise_CM,
    index_counter = local_index_counter
  ))
}

# Process all folds for a dataset
process_dataset <- function(dataset, data_config, mia_df, noiseMIA_list, index_list, index_counter) {
  print(paste("Processing dataset:", dataset))
  
  # Load the dataset
  df <- load_dataset(dataset)
  
  # Create cross-validation folds
  folds <- create_folds(df)
  
  # Initialize results storage for this dataset
  folds_results <- list()
  folds_CM <- list()
  
  # Process each fold
  for (fold_idx in 1:length(folds$train)) {
    # Get training and testing data
    train_indices <- folds$train[[fold_idx]]
    test_indices <- folds$test[[fold_idx]]
    train_df <- df[train_indices, ]
    test_df <- df[test_indices, ]
    
    # Print class distribution
    print(paste("Dataset:", dataset))
    print("Probability table:")
    print(prop.table(table(train_df$class)))
    
    # Initialize results storage for methods
    method_results <- list()
    method_CM <- list()
    
    # Process KNN method (the only active one in the original script)
    method <- "knn"
    result <- process_method(
      dataset = dataset,
      fold_index = fold_idx,
      train_df = df,  # KNN uses full dataset in original script
      test_df = test_df,
      method = method,
      noise_levels = data_config$noise_level,
      noise_names = data_config$noise_names,
      instances = data_config$instances,
      instances_names = data_config$instances_names,
      mia_df = mia_df,
      noiseMIA_list = noiseMIA_list,
      control = data_config$control,
      index_list = index_list,
      index_counter = index_counter
    )
    
    # Update index counter
    index_counter <- result$index_counter
    
    # Store method results
    method_results[[1]] <- result$noise_results
    method_CM[[1]] <- result$noise_CM
    
    # Name the method results
    names(method_results) <- c("knn")
    names(method_CM) <- c("knn")
    
    # Store in fold lists
    folds_results[[fold_idx]] <- method_results
    folds_CM[[fold_idx]] <- method_CM
  }
  
  # Name the fold results
  names(folds_results) <- data_config$fold_names
  names(folds_CM) <- data_config$fold_names
  
  # Save the results for this dataset
  save_dataset_results(dataset, folds_results, folds_CM)
  
  # Return the results and updated index counter
  return(list(
    folds_results = folds_results,
    folds_CM = folds_CM,
    index_counter = index_counter
  ))
}

# Save results for a dataset
save_dataset_results <- function(dataset, folds_results, folds_CM) {
  # Create directory if it doesn't exist
  dir.create("results/instances/by_dataset", showWarnings = FALSE, recursive = TRUE)
  
  # Define filenames
  filename1 <- paste0("results/instances/by_dataset/", dataset, "_instances_KNN.rds")
  filename2 <- paste0("results/instances/by_dataset/", dataset, "_instancesCM_KNN.rds")
  
  # Save the results
  saveRDS(folds_results, file = filename1)
  saveRDS(folds_CM, file = filename2)
  
  print(paste("Results saved for dataset:", dataset))
}

# Main function to run the experiment
run_experiment <- function() {
  # Load packages
  load_required_packages()
  
  # Load configuration and data
  data <- load_configuration()
  
  # Initialize results storage
  dataset_results <- list()
  dataset_CM <- list()
  index_list <- list()
  index_counter <- 1
  
  # Process each dataset
  for (dataset_idx in 1:length(data$datasets)) {
    dataset <- data$datasets[dataset_idx]
    
    # Process the dataset
    result <- process_dataset(
      dataset = dataset,
      data_config = data$config,
      mia_df = data$results$mia_df,
      noiseMIA_list = data$results$noiseMIA_list,
      index_list = index_list,
      index_counter = index_counter
    )
    
    # Update index counter
    index_counter <- result$index_counter
    
    # Store dataset results
    dataset_results[[dataset_idx]] <- result$folds_results
    dataset_CM[[dataset_idx]] <- result$folds_CM
    
    print("Altered instances with noise recorded")
    print("----------------")
  }
  
  # Name the dataset results
  names(dataset_results) <- data$datasets
  names(dataset_CM) <- data$datasets
  
  # Optionally save the complete results
  # saveRDS(dataset_results, file = "results/instances/instances_list.rds")
  # saveRDS(dataset_CM, file = "results/instances/instancesCM_list.rds")
  
  print("****************")
  print("RESULTS RECORDED")
  print("****************")
  
  return(list(
    dataset_results = dataset_results,
    dataset_CM = dataset_CM
  ))
}

# Execute the experiment
if (!interactive()) {
  run_experiment()
}