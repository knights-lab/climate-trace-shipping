# Shipping data predictive modeling for Climate Trace

## Overview
<<<<<<< HEAD
This repo contains a standalone commandline application for training ML models on shipping data with labeled kg CO2 per nautical mile, estimating hold-out performance of several models, and predicting values for new unlabeled ships. Training data currently must be in a specific csv format from EU-reported data. New csv-formatted data tables for prediction need only have the IMO numbers of ships in a column.
=======
This repo contains code for training ML models on shipping data with labeled kg CO2 per nautical mile, estimating hold-out performance of several models, and predicting values for new unlabeled ships. Training data currently must be in a specific csv format from EU-reported data. New csv-formatted data tables for prediction need only have the IMO numbers of ships in a column.
>>>>>>> 2794220 (made imputation package work with only one missing value.)

## Installation and Requirements
This package requires `R` and the following `R` packages:
 - optparse
 - mice
 - RColorBrewer
 - caret
 - xgboost
 - data.table
 - randomForest

`R`  may be installed from [rproject.org](https://www.r-project.org/). Once `R` is installed, the packages may be installed by running `R` and then the following command:
```bash
<<<<<<< HEAD
install.packages(c('optparse','mice','randomForest','RColorBrewer','xgboost','data.table'),repo='http://cran.wustl.edu',dep=TRUE)
=======
install.packages(c('optparse','mice','randomForest',RColorBrewer','xgboost','data.table'),repo='http://cran.wustl.edu',dep=TRUE)
>>>>>>> 2794220 (made imputation package work with only one missing value.)
```

If this command fails, please try installing packages one at a time, or try a different repository from the [list of mirrors](https://cran.r-project.org/mirrors.html).

<<<<<<< HEAD
To use this package, you must clone it (using `git clone https://github.com/knights-lab/climate-trace-shipping.git`) or download a static version of it [here](https://github.com/knights-lab/climate-trace-shipping/archive/refs/heads/main.zip) and extract the files from the downloaded zip file.

=======
>>>>>>> 2794220 (made imputation package work with only one missing value.)
Finally, this package requires that the path to the package be included in your `.Renviron` file in your home directory with the environment variable name `R_CLIMATE_TRACE_SHIPPING_HOME`. This can be achieved with the following command:
```bash
echo "R_CLIMATE_TRACE_SHIPPING_HOME='/Users/danknights/Dropbox/research/climate-trace-shipping'" >> ~/.Renviron
```

## Usage guide

### Tuning and evaluating models
<<<<<<< HEAD
Models may be tuned, evaluated, and trained using the script, `train_and_evaluate_models.r`. This and other executable scripts are contained in the `bin` directory in this repository. This requires as inputs:
=======
Models may be tuned, evaluated, and trained using the script, `train_and_evaluate_models.r`. This requires:
>>>>>>> 2794220 (made imputation package work with only one missing value.)

1. EU-formatted training data with IMO Numbers and these fields:
 - Ship type
 - distance.traveled.nm
 - average.speed.nm.h
 - Annual.Total.time.spent.at.sea.hours
 - Annual.average.CO.emissions.per.distance.kg.CO.n.mile (Note: this column contains the kg CO2/nm labels used for model training)

2. A ship metadata table, e.g. `IHS complete Ship Data.csv`, that contains these required training metadata fields:
 - Deadweight
 - FlagName
 - GrossTonnage
 - LengthOverallLOA
 - LengthRegistered
 - Breadth
 - Draught
 - ShiptypeLevel2
 - ShiptypeLevel3
 - ShiptypeLevel4
 - Powerkwmax
 - TotalPowerOfAuxiliaryEngines
 - Speedmax
 - Speed
 - YearOfBuild

<<<<<<< HEAD
View script usage instructions with `-h`. Run the command with `Rscript` followed by the full path the `train_and_evaluate_models.r` file, located in the `bin` directory of this repo.
```bash
Rscript /path/to/train_and_evaluate_models.r -h
=======
View script usage instructions with
```bash
Rscript train_and_evaluate_models.r -h
>>>>>>> 2794220 (made imputation package work with only one missing value.)
```

Usage examples:

<<<<<<< HEAD
Tune models and evaluate performance with the following command. Required input data are not distributed in this repository and must be supplied by the user. Note that currently final model can only be produce and saved for one model at a time, so this step only performs evaluation and comparison of models. This will run random forests (rf), extreme gradient boosting (xgb), linear modeling within each ship type, and ridge-regression modeling within each ship type. Each model will be tuned on training data and evaluated on hold-out test data using 5 randome train/test splits of 2/3 train, 1/3 test. Reported performance metrics are mean absolute error (MAE) and normalized root-mean-squared error (NRMSE).  Note that if the input data file as spaces in the filename, the entire filename must be surrounded by quotation marks as shown for the metadata file and input file in the following command.
=======
Tune models and evaluate performance with the following command. Required input data are not distributed in this repository and must be supplied by the user. Note that currently final model can only be produce and saved for one model at a time, so this step only performs evaluation and comparison of models. This will run random forests (rf), extreme gradient boosting (xgb), linear modeling within each ship type, and ridge-regression modeling within each ship type. Each model will be tuned on training data and evaluated on hold-out test data using 5 randome train/test splits of 2/3 train, 1/3 test. Reported performance metrics are mean absolute error (MAE) and normalized root-mean-squared error (NRMSE). 
>>>>>>> 2794220 (made imputation package work with only one missing value.)
```bash
time Rscript ../bin/train_and_evaluate_models.r -i "../data/EU MRV data 18-19-20.csv" -m "../data/IHS complete Ship Data.csv" -o output_model_eval --models 'rf,xgb,linear,ridge' -v --save_preprocessed_data --skip_final_model --repeats 5
```

The output file, `summary.txt` in the output directory shows a summary of performance of different models across train/test splits, and reports the chosen hyperparameters for each model (for those that require hyperparameters) in each train/test split.

The flag, `--save_preprocessed_data` will cause the script to save a file, `preprocessed.csv`, in the output folder. This will contain cleaned and preprocessed input data consisting only of the columns needed for building predictive models, and will imputation of missing values complete. This is for convenience. Future training/evaluation can be run on the preprocessed data by providing it as the `input` and adding the `--load_preprocessed_data` flag.

### Training a final model
Models may be tuned, evaluated, and trained using the script, `predict_emissions.r`. This requires:

Generate a final "random forests" model using hardcoded hyperparams with the following, skipping the tuning/evaluation steps:
```bash
time Rscript ../bin/train_and_evaluate_models.r -i "../data/EU MRV data 18-19-20.csv" -m "../data/IHS complete Ship Data.csv" -o output_final_rf --models 'rf' -v --skip_eval
```

Generate a final "random forests" model after using tuning/evaluation to choose the best hyperparameters over 5 random train/test splits:
```bash
time Rscript ../bin/train_and_evaluate_models.r -i "../data/EU MRV data 18-19-20.csv" -m "../data/IHS complete Ship Data.csv" -o output_final_rf --models 'rf' -v --repeats 5
```

### Run predictions on new data
Models may be tuned, evaluated, and trained using the script, `predict_emissions.r`. This requires:

1. Input data in csv format with one column containing IMO Numbers

2. A ship metadata table, e.g. `IHS complete Ship Data.csv`, that contains these required training metadata fields:
 - Deadweight
 - FlagName
 - GrossTonnage
 - LengthOverallLOA
 - LengthRegistered
 - Breadth
 - Draught
 - ShiptypeLevel2
 - ShiptypeLevel3
 - ShiptypeLevel4
 - Powerkwmax
 - TotalPowerOfAuxiliaryEngines
 - Speedmax
 - Speed
 - YearOfBuild

3. A final predictive model generated using the script `train_and_evaluate_models.r` as described above.

View script usage instructions with
```bash
Rscript predict_emissions.r -h
```

Usage examples:

<<<<<<< HEAD
Run predictions on new input CSV table. This assumes that IMO numbers are in a column named "imo num", for example. This uses the final model from the above training commands, `output_file_rf/final_model.rdata`. Note that if the input data file as spaces in the filename, the entire filename must be surrounded by quotation marks as shown for the metadata file in the following command.
=======
Run predictions on new input CSV table. This assumes that IMO numbers are in a column named "imo num", for example. This uses the final model from the above training commands, `output_file_rf/final_model.rdata`.
>>>>>>> 2794220 (made imputation package work with only one missing value.)
```bash
time Rscript ../bin/train_and_evaluate_models.r -i newdata.csv -I "imo num" -m "../data/IHS complete Ship Data.csv" -o newdata_predicted.csv --model_file output_final_rf/final_model.rdata -v
```

