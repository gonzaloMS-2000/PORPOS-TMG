This folder includes our work related to segmenting the 2015 SMTO dataset by living arrangement.
We noticed that our proposed model performed much worse for students living with their family.

Code files:
- Segmented_Results.R: A comparison of results for our model when segmented by living arrangement
- Family_Mode_Choice.R (outdated): Testing availability determination in mode choice models for the Family segment
- Family_Accessibility_vs_Distance.R (outdated): Comparing results from accessibility model to distance model, uses Accessibility_Data.csv

Data files:
- Accessibility_Data.csv: Columns added for accessibility (manually calculated using coefficients from mode choice model)

Results files:
- Proposed_Model_Results_Comparison.xlsx: Key performance metrics for segmented and unsegmented proposed model and family mode choice model
- Confusion_Matrices.xlsx: Hardmax and softmax confusion matrices for segmented and unsegmented proposed model