This folder contains our work on the combined SMTO dataset including the 2015 and 2019 data.

Code files:
- Subset_Models.R: Estimation with only seven schools from 2015 survey
- Nested.R: Nested model with 2015 schools in one nest and 2019 schools in another.
- Bias_Term.R: Experimental implementation with an additional enrollment term for 2015 schools to account for over-representation bias in naive pooling.
- Initial_Runs.R: Runs with naive pooling and random forest terms

Result files:
- Subset_Results.xlsx: Comparison of subset models for 2015, 2019, and combined datasets.
- Nested_Results.xlsx: Results from Nested.R
- RF_Results.xlsx: Results from Initial_Runs.R with various approaches to include random forest predictions andprobabilities