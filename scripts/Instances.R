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
  print("Calculate prediction")
  
  # Prediction without noise
  predict_unseen <- predict(fit, test_df)
  
  # Prediction with noise 
  noise_predict <- predict(fit, instances_df)
  
  # Confusion matrix
  print("Confusion matrix")
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

# Function to process instances with noise
process_instances <- function(dataset, fold_index, method, noise, percent, test_df, noise_df, fit, index_list, semaphore, index_counter) {
  # Print relevant information for the iteration
  print(paste("Dataset:", dataset))
  print(paste("Fold:", fold_index))
  print(paste("Method:", method))
  print(paste0("Noise level: ", noise * 100, "%"))
  print(paste0("Percentage of altered instances: ", percent * 100, "%"))

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
  
  print(paste0("Alter ", percent * 100, "% of instances with noise"))
  
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

# Noise
# TODO

# Methods
# TODO

# Folds
# TODO

# Datasets
# TODO

# Load and extract parameters
parameters <- load_parameters("data/files/parameters.csv")
datasets <- parameters$datasets
fold_names <- parameters$fold_names
methods <- parameters$methods
noise_level <- parameters$noise_level
instances <- parameters$instances
control <- parameters$control

# Load previous most important attribute table and
mia_df <- readRDS("results/most_important_attr/mia_df.rds")
noiseMIA_list <- readRDS("results/noise/noise_list.rds")

# Pre-allocate results dataframe with expected size (more efficient)
total_rows <- length(datasets) * length(fold_names) * length(methods) * 
              length(noise_level) * length(instances)
results_df <- data.frame(
  dataset = character(total_rows),
  fold = character(total_rows),
  method = character(total_rows),
  noise_level = numeric(total_rows),
  instance_level = numeric(total_rows),
  accuracy = numeric(total_rows),
  kappa = numeric(total_rows),
  stringsAsFactors = FALSE
)

# Create a list that contains the indices and order to be altered
index_list <- list()
index_counter = 1

# Per dataset
for(dataset in datasets) {
  
  # Load dataset
  filename = paste0("data/datasets/", dataset, ".rds")
  df <- readRDS(filename)

  # Create fold indices (training and testing)
  fold_train_indices <- createFolds(df$class, k = 5, list = TRUE, returnTrain = TRUE)
  fold_test_indices <- lapply(fold_train_indices, function(index) setdiff(1:nrow(df), index))
  
  # Create a list to store the attribute lists per dataframe
  folds_list <- list()
  folds_CM <- list()
  folds_counter = 1
  
  # Loop through the folds and create a dataframe for training
  for (i in 1:length(fold_train_indices)) {
    
    # Get the training indices for the current fold
    train_indices <- fold_train_indices[[i]]
    test_indices <- fold_test_indices[[i]]
    
    # Create the corresponding dataframes using the current fold
    train_df <- df[train_indices, ]
    test_df <- df[test_indices, ]
    
    # Create a frequency table for the class in the 'Training' dataset
    # Calculate the frequency table as a proportion of all values
    print(paste("Dataset:", dataset))
    print("Probability table:")
    print(prop.table(table(train_df$class)))
    
    # Create a list to store the noise lists per method
    method_list <- list()
    method_CM <- list()
    method_counter = 1

    # Train for each method selected
    for(method in methods) {

        # Train to obtain model
        print(paste("Dataset:", dataset))
        print(paste("Method:", method))
        print("BEGINNING TRAINING")
        fit <- train_model(method, train_df, control)
        print("TRAINING SUCCESSFUL")
        
        # Select the MIA we will get the noise from
        mia <- subset(mia_df, dataset_name == dataset & technique == method)$most_important_attribute
        
        # Create a list to store the % instance lists per noise level
        noise_list <- list()
        noise_CM <- list()
        noise_counter = 1
        
        # Control variable to make sure sampling only happens on the first iteration
        semaphore = TRUE
        
        for(noise in noise_level){
          
          # Create a list to store the dataframe with instances per % of instances
          instances_list <- list()
          confMatrix_list <- list()
          instances_counter = 1
          
          for(percent in instances) {
            result <- process_instances(dataset, i, method, noise, percent, test_df, noise_df, fit, index_list, semaphore, index_counter)
  
            # Update control variables
            index_list <- result$index_list
            index_counter <- result$index_counter
            semaphore <- result$semaphore
            
            # Add results to the data frame
            results_df <- rbind(results_df, result$result)
            
            print("-")
          }
          
          print("-")
        }
      }
    }
  }
  
  # Safeguard store by dataset
  filename = paste0("results/instances/by_dataset/", dataset, "_results.csv")
  dataset_results <- subset(results_df, dataset == dataset)
  write.csv(dataset_results, file = filename, row.names = FALSE)
  
  print("Altered instances with noise recorded")
  print("----------------")
}

# Save the complete results
write.csv(results_df, file = "results/instances/complete_results.csv", row.names = FALSE)
print("****************")
print("RESULTS RECORDED")
print("****************")