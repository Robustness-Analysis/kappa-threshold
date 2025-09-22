# /usr/bin/env/Rscript

# Create data lists
dataset_name <- c("analcatdata_authorship", "badges2", "banknote", "blood-transfusion-service-center", "breast-w", "cardiotocography", "climate-model-simulation-crashes", "cmc", "credit-g", "diabetes", "eucalyptus", "iris", "kc1", "liver-disorders", "mfeat-factors", "mfeat-karhunen", "mfeat-zernike", "ozone-level-8hr", "pc4", "phoneme", "qsar-biodeg", "tic-tac-toe", "vowel", "waveform-5000", "wdbc", "wilt") # Dataset names

fold_name <- c("Fold_1", "Fold_2", "Fold_3", "Fold_4", "Fold_5") # Set names to each fold

technique_name = c("C5.0", "ctree", "fda", "gbm", "gcvEarth", "JRip", "lvq", "mlpML", "multinom", "naive_bayes", "PART", "rbfDDA", "rda", "rf", "rpart", "simpls", "svmLinear", "svmRadial", "rfRules", "knn", "bayesglm") # ML techniques

noise_level = c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1) # Percentage of noise injected


instance_level <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1) # Percentage of instances to be altered

# Save data lists
saveRDS(dataset_name, file = "files/dataset_name.rds")
saveRDS(fold_name, file = "files/fold_name.rds")
saveRDS(technique_name, file = "files/technique_name.rds")
saveRDS(noise_level, file = "files/noise_level.rds")
saveRDS(instance_level, file = "files/instance_level.rds")
