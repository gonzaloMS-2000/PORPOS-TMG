This folder includes our work on a Random Forest model to predict school type (university or college) based on the 2019 SMTO dataset.
The code files in this folder are outdated but can be ran by updating the file paths.

Code files:
- Train_Test_Split.ipynb: Split the 2019 SMTO dataset into traning and testing data
- Feature_Selection.ipynb (documented): Tune random forest model by selecting appropriate features
- SMTO_2019_Probabilities.ipynb: Generate 2019_RF_Predictions.csv
- SMTO_Combined_Probabilities.ipynb: Generate Combined_RF_Predictions.csv

Created data files:
- Training.csv: Data to train random forest model
- Testing.csv: Data to test random forest model
- 2019_RF_Predictions.csv: Probabilities and predictions for 2019 dataset
- Combined_RF_Predictions.csv: Probabilities and predictions for combined 2019 and 2015 datasets

Results files:
- F1_Scores: Confusion matrices and F1 scores generated during feature selection