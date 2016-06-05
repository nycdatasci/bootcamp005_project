Quantum Jitters Files:

Create Train Data File - with Rot.R
	Reads the original Kaggle training file, performs the feature engineering and saves as 'MichaelsData.rds'

Create Test Data File - with Rot.R
	Same as above but transforms Test data set

Training Data with Rot.rds
	This is the transformed data.  Should be renamed 'MichaelsData.rds' to be read by model code.

Run Model - XGBoost.R
	Sample code to run XGBOOST model.  Tuning parameters are the only changes that differ from run to run.