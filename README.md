# Shipping data predictive modeling for Climate Trace

## Overview

This repo contains a standalone commandline application for training ML models on shipping data with labeled kg CO2 per nautical mile, estimating hold-out performance of several models, and predicting values for new unlabeled ships. Training data currently must be in a specific csv format from EU-reported data. New csv-formatted data tables for prediction need only have the IMO numbers of ships in a column.

## Installation and Requirements
This package requires `R` and the following `R` packages:
 - optparse
 - mice
 - RColorBrewer
 - caret
 - xgboost
 - data.table
 - randomForest
 - caret (only required if using ridge regression model, )

`R`  may be installed from [rproject.org](https://www.r-project.org/). Once `R` is installed, the packages may be installed by running `R` and then the following command:
```bash
install.packages(c('optparse','mice','randomForest','RColorBrewer','xgboost','data.table'),repo='http://cran.wustl.edu',dep=TRUE)
```

If this command fails, please try installing packages one at a time, or try a different repository from the [list of mirrors](https://cran.r-project.org/mirrors.html).

To use this package, you must clone it (using `git clone https://github.com/knights-lab/climate-trace-shipping.git`) or download a static version of it [here](https://github.com/knights-lab/climate-trace-shipping/archive/refs/heads/main.zip) and extract the files from the downloaded zip file.

Finally, this package requires that the path to the package be included in your `.Renviron` file in your home directory with the environment variable name `R_CLIMATE_TRACE_SHIPPING_HOME`. This can be achieved with the following command on UNIX (Linux or Mac), substituting the full path to the climate-trace-shipping repo top-level folder for `/path/to/climate-trace-shipping`:
```bash
echo "R_CLIMATE_TRACE_SHIPPING_HOME='/path/to/climate-trace-shipping'" >> ~/.Renviron
```

## Usage guide
### Imputing missing metadata
Ship metadata can be preprocessed prior to the main predictive modeling analysis. This is recommended as a best practice both for convenience, because it can take a long time to run, and for increased reproducibility, so that the same imputed metadata can be used repeatedly. The following command will take in a raw metadata file, perform all imputation and feature engineering, and write out the preprocessed file:
```bash
Rscript bin/preprocess_metadata.r -i "IHS complete Ship Data.csv" -o IHS-imputed-rf.csv
```

### Tuning and evaluating models
Models may be tuned, evaluated, and trained using the script, `train_and_evaluate_models.r`. This and other executable scripts are contained in the `bin` directory in this repository. This requires as inputs:

1. EU-formatted training data with IMO Numbers and these fields:
 - Ship type
 - distance.traveled.nm
 - average.speed.nm.h
 - Annual.Total.time.spent.at.sea.hours
 - Annual.average.CO.emissions.per.distance.kg.CO.n.mile (Note: this column contains the kg CO2/nm labels used for model training)

2. A preprocessed ship metadata table, e.g. `IHS-imputed-rf.csv`, that contains these required training metadata fields:
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

View script usage instructions with `-h`. Run the command with `Rscript` followed by the full path the `train_and_evaluate_models.r` file, located in the `bin` directory of this repo.
```bash
Rscript bin/train_and_evaluate_models.r -h
```

Usage examples:

Tune models and evaluate performance with the following command. Required input data are not distributed in this repository and must be supplied by the user. Note that currently the final model can only be produced and saved for one model at a time. The command below only performs evaluation and comparison of models. This will run random forests (rf), extreme gradient boosting (xgb), linear modeling within each ship type, and ridge-regression modeling within each ship type (delete `ridge` from models list if `caret` package is not installed). Each model will be tuned on training data and evaluated on hold-out test data using 5 random train/test splits of 2/3 train, 1/3 test. Reported performance metrics are mean absolute error (MAE) and normalized root-mean-squared error (NRMSE).  Note that if the input data file has spaces in the filename, the entire filename must be surrounded by quotation marks as shown for the metadata file and input file in the following command.

```bash
Rscript bin/train_and_evaluate_models.r -i "data/EU MRV data 18-19-20.csv" -m "data/IHS-imputed-rf.csv" -o output_model_eval --models "rf,xgb,linear,ridge" -v --skip_final_model --repeats 5
```

The output file, `summary.txt` in the output directory shows a summary of performance of different models across train/test splits, and reports the chosen hyperparameters for each model (for those that require hyperparameters) in each train/test split.

### Training a final model
Models may be tuned, evaluated, and trained using the script, `predict_emissions.r`. This requires:

Generate a final "random forests" model using hardcoded hyperparams with the following, skipping the tuning/evaluation steps:
```bash
Rscript bin/train_and_evaluate_models.r -i "data/EU MRV data 18-19-20.csv" -m "data/IHS-imputed-rf.csv" -o output_final_rf --models "rf" -v --skip_eval
```

Generate a final "random forests" model after using tuning/evaluation to choose the best hyperparameters over 5 random train/test splits:
```bash
Rscript bin/train_and_evaluate_models.r -i "data/EU MRV data 18-19-20.csv" -m "data/IHS-imputed-rf.csv" -o output_final_rf --models "rf" -v --repeats 5
```

### Run predictions on new data
Models may be tuned, evaluated, and trained using the script, `predict_emissions.r`. This requires:

1. Input data in csv format with one column containing IMO Numbers

2. A ship metadata table that contains these required training metadata fields:
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
Rscript bin/predict_emissions.r -h
```

Usage examples:

Run predictions on new input CSV table. This assumes that IMO numbers are in a column named "imo num", for example. This uses the final model from the above training commands, `output_file_rf/final_model.rdata`. Note that if the input data file has spaces in the filename, the entire filename must be surrounded by quotation marks as shown for the metadata file in the following command.

```bash
Rscript bin/predict_emissions.r -i newdata.csv -I "imo num" -m "data/IHS-imputed-rf.csv" -o newdata_predicted.csv --model_file output_final_rf/final_model.rdata -v
```

