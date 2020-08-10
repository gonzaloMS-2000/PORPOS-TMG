This folder contains data files for the 2015 SMTO dataset.

Original data files (renamed):
- Households.csv
- Respondents.csv
- Data_Guide.xlsx

Code files:
- Data_Transformation.ipynb: Transform original data files and add needed columns to create Formatted.csv

Created data files:
- Campus_Info.csv: List of campuses and attributes, zones from Joven's 'MOE-Schools & uni with DA.xlsx'
	Enrollments from https://cudo.ouac.on.ca/ or https://data.ontario.ca/dataset/college-enrolment if source is indicated as 'Data'
	If source is ASC, enrollment was predicted using logit model results.
	If source is Prop, the sampling rate for different campuses were used where breakdowns by campus were unavailable
- Formatted.csv: Full data file to be used as input for models

TBD:
- CampusZones.csv