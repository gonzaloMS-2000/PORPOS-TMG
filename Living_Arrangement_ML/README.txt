This folder contains our work on creating a machine learning model to classify students according to their living arrangement.
The model is trained using labelled data from the 2015 SMTO survey and then ran on the 2016 TTS data.
Note that we must make sure to use only information that is available from both datasets.

We developed an original model using ML.NET's Model Builder tool on Visual Studio. These files are kept in the Outdated folder.
The selected model was a light gradient-boosting machine. Because of this, we developed a light gradient-boosting machine using Python.

Code files:
- Train_Model: Train gradient-boosting machine to create model.txt
- Predictions.ipynb: Predict living arrangement using model and write Predictions.csv

Created data files:
- Predictions.csv: Predictions for filtered TTS data

Other created files:
- model.txt: The trained lightgbm model, loaded in Predictions.ipynb