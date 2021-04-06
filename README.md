# superconductor_crttmp

## Prediction of critical temperature from data derived from superconductor structure and elemental composition

### Summary
Superconductivity data set downloaded from from https://archive.ics.uci.edu/ml/datasets/Superconductivty+Data.
The object of this exercise was to predict the critical temperature of superconductors from data originally derived from the structure and elemental properties of a list of superconductors obtained at https://supercon.nims.go.jp/en/.

The effect of each feature on critical temperature was checked by first examining the correlation and then plotting each feature against critical temperature.

A variety of model types were initially tested using caret against the features that showed greatest correlation and the worst performing model types discarded.
In addition to models built using caret, a principal component analysis was performed and a model built using xgBoost
A model built using caret and Ranger produced the lowest RMSE for critical temperature predictions on the full dataset for the caret-built models, however this was still higher than the RMSE for the xgBoosted model

When these models were used to make predictions on the test set, the ranger model performed better than the xgBoosted model. On closer examination the ranger model appeared to be more accurate at critical temperatures close to zero.

### Files
File FullScript.R is the script used to download and process the data, create the model and assess the performance.
File superconductivity_report.Rmd is R markdown for the report.
File superconductivity_report.pdf is final knitted report.