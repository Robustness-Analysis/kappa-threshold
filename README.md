# Kappa Threshold Analysis

## Project Structure

```
kappa-threshold/
├── data/                               # Data files
│   ├── datasets/                       # Clean datasets from IJDSA repository
│   └── files                           # Names of necessary files
├── scripts/                            # R scripts
│   ├── package_installer.R             # 0 - To install required packages
│   ├── dataset_info.R                  # 0 - Summary of dataset information
│   ├── filenames.R                     # 0 - File name creator script
│   └── threshold_instance_finder.R     # 1 - Function to find optimal instances for each
├── results/                            # Output files
└── analysis/                           # R markdown files
    └── threshold_analysis.Rmd          # Visualization for results from `threshold_instance_finder.R`
```
