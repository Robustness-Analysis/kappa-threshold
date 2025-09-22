#!/usr/bin/env Rscript

# Load required packages for path handling
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)  # for file path handling

# Create data lists
dataset_name <- c("analcatdata_authorship", "badges2", "banknote", "blood-transfusion-service-center", "breast-w", "cardiotocography", "climate-model-simulation-crashes", "cmc", "credit-g", "diabetes", "eucalyptus", "iris", "kc1", "liver-disorders", "mfeat-factors", "mfeat-karhunen", "mfeat-zernike", "ozone-level-8hr", "pc4", "phoneme", "qsar-biodeg", "tic-tac-toe", "vowel", "waveform-5000", "wdbc", "wilt") # Dataset names

fold_name <- c("Fold_1", "Fold_2", "Fold_3", "Fold_4", "Fold_5") # Set names to each fold

technique_name = c("C5.0", "ctree", "fda", "gbm", "gcvEarth", "JRip", "lvq", "mlpML", "multinom", "naive_bayes", "PART", "rbfDDA", "rda", "rf", "rpart", "simpls", "svmLinear", "svmRadial", "rfRules", "knn", "bayesglm") # ML techniques

noise_level = c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1) # Percentage of noise injected

instance_level <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1) # Percentage of instances to be altered

# Save data lists using proper path handling with here package
saveRDS(dataset_name, file = here("data", "files", "dataset_name.rds"))
saveRDS(fold_name, file = here("data", "files", "fold_name.rds"))
saveRDS(technique_name, file = here("data", "files", "technique_name.rds"))
saveRDS(noise_level, file = here("data", "files", "noise_level.rds"))
saveRDS(instance_level, file = here("data", "files", "instance_level.rds"))
