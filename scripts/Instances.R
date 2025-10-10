# /usr/bin/env/Rscript

# Packages that need to be loaded
pacman::p_load(caret, iml, citation, dplyr, earth, lime, data.table)

# Set the seed to make the experiment reproducible
set.seed(1)

# Load parameters from CSV
params_df <- read.csv("data/files/parameters.csv", stringsAsFactors = FALSE)

# Extract parameters
datasets <- unlist(strsplit(subset(params_df, parameter == "dataset_name")$values, "\\|"))
fold_names <- unlist(strsplit(subset(params_df, parameter == "fold_name")$values, "\\|"))
methods <- unlist(strsplit(subset(params_df, parameter == "technique_name")$values, "\\|"))
method_names <- methods
noise_level <- as.numeric(unlist(strsplit(subset(params_df, parameter == "noise_level")$values, "\\|")))
noise_names <- paste0("noise_", noise_level * 100)
instances <- as.numeric(unlist(strsplit(subset(params_df, parameter == "instance_level")$values, "\\|")))
instances <- append(instances, c(0.25, 0.75))  # Adding additional instance levels
instances_names <- as.character(instances * 100)

# Control parameters
control = trainControl(method = "none", number = 1)

# Load previous most important attribute table and
mia_df <- readRDS("results/most_important_attr/mia_df.rds")
noiseMIA_list <- readRDS("results/noise/noise_list.rds")
 
# Create a model training function
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

# Pre-allocate results dataframe with expected size
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
  filename = paste0("datasets/", dataset, ".rds")
  df <- readRDS(filename)
  
  # Create fold indices, generating the training folds and the testing folds
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
            
            # Print relevant information for the iteration
            print(paste("Dataset:", dataset))
            print(paste("Fold:", i))
            print(paste("Method:", method))
            print(paste0("Noise level: ", noise * 100, "%"))
            print(paste0("Percentage of altered instances: ", percent * 100, "%"))
            
            # Create a new dataframe with the noise from noiseMIA_list
            noiselvl <- paste0("noise_", noise * 100)
            noise_df <- noiseMIA_list[[dataset]][[mia]][[noiselvl]]
            #noise_df <- noiseMIA_list[[mia]][[noiselvl]]
            
            # Create a new df with the altered number of instances desired
            instances_df <- test_df
            
            # Convert to data.table and add indices efficiently
            setDT(test_df)[, index := .I]
            setDT(noise_df)[, index := .I]
            setDT(instances_df)[, index := .I]
            
            # Determine the indices list to alter the same instances consistently
            if(semaphore) {
              print("Obtain vector of altered instances")
              
              # Number of values to be altered (instances)
              sample_size = round(nrow(instances_df) * 1, 0)
              
              # Sample of ids that we want from the test df
              indices <- sample(instances_df[, index], sample_size)
              
              # Store vector of indices in list
              index_list[[index_counter]] <- indices
              
              # Advance index counter
              index_counter = index_counter + 1
              
              # Set control variable to 0
              semaphore = FALSE
            }
            
            print(paste0("Alter ", percent * 100, "% of instances with noise"))
            
            # Number of values to be altered (instances)
            sample_size = round(nrow(instances_df) * percent, 0)
            
            # Sample of ids that we want from the test df
            indices <- tail(index_list, n = 1)
            indices <- indices[[1]][1:sample_size]
            
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
            
            # Prediction
            print("Calculate prediction")
            
            # Prediction without noise
            predict_unseen = predict(fit, test_df)
            predict_unseen
            
            # Prediction with noise 
            noise_predict = predict(fit, instances_df)
            noise_predict
            
            # Confusion matrix
            print("Confusion matrix")
            if(noise == 0) {
              conf_matrix = caret::confusionMatrix(predict_unseen, predict_unseen)
              #print(conf_matrix)
            }else {
              conf_matrix = caret::confusionMatrix(predict_unseen, noise_predict)
              #print(conf_matrix)
            }
            
            # Extract metrics from confusion matrix
            if(conf_matrix$overall["Kappa"] == "NaN"){ conf_matrix$overall["Kappa"] = 1 }
            
            # Add results to the data frame
            results_df <- rbind(results_df, data.frame(
              dataset = dataset,
              fold = paste0("Fold_", i),
              method = method,
              noise_level = noise,
              instance_level = percent,
              accuracy = conf_matrix$overall["Accuracy"],
              kappa = conf_matrix$overall["Kappa"],
              stringsAsFactors = FALSE
            ))
            
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
write.csv(results_df, file = "results/instances/complete_results.csv", row.names = FALSE)print("****************")
print("RESULTS RECORDED")
print("****************")
