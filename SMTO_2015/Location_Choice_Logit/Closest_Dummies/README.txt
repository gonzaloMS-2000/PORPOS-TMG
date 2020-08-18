This folder contains our work on introducing closest dummies in the gravity model for SMTO 2015 location choice.
The idea is to add a term to the utility model B_Closest * C_ij, where C_ij is 1 if school j is closest to student i's home zone.
We also exsperiment with imposing a threshold t such that C_ij can only be 1 if the distance d_ij <= t.
In this folder, the "Eric" model refers to a doubly-constrained gravity model with alternative-specific constants and B_ENROL fixed to 1.
The "Proposed" model refers to a singly-constrained gravity model where B_ENROL is free.
In both cases the log of enrollment is included.

Code files:
- Threshold_Analysis.ipynb: In this documented notebook we experiment with various thresholds and determine that the ideal one is 2km
- Single_with_Threshold.ipynb: Here, we use the 2km threshold with the singly-constrained model and look at the results

Result files:
- Initial_Results.xlsx: Early results from our first experimentation with the closest dummies. Inspired the threshold analysis.
- Confusion_Matrices.xlsx: Confusion matrices for "Proposed" vs. "Eric" models

HTML files:
These are files produced by Biogeme that show the outputs for various threshold models.
The files were created with the code files listed above.

