# Final model trained on EU data 2022-02-25
### Tune models and evaluate performance with:
```bash
time Rscript ../bin/train_and_evaluate_models.r -i "../data/EU MRV data 18-19-20.csv" -m "../data/IHS complete Ship Data.csv" -o output_model_eval --models 'rf,xgb,linear,ridge' -v --save_preprocessed_data --skip_final_model
```

### Generate final "random forests" model using hardcoded hyperparams with:
```bash
time Rscript ../bin/train_and_evaluate_models.r -i "../data/EU MRV data 18-19-20.csv" -m "../data/IHS complete Ship Data.csv" -o output_final_rf --models 'rf' -v --skip_eval
```

### Run predictions on new data, assuming IMO numbers are in a column named "imo num", with:
```bash
time Rscript ../bin/train_and_evaluate_models.r -i newdata.csv -I "imo num" -m "../data/IHS complete Ship Data.csv" -o output_predicted --model_file output_final_rf/final_model.rdata -v
```
