This folder contains our work on a nested logit model for the 2019 SMTO location choice problem.
We estimate models using both Biogeme (Python) and mlogit (R).
The nesting structure is such that the universities are in one nest and the colleges in another.

Code files:
- One_MU.ipynb (outdated): Estimate nested model with one scale parameter
- Two_MUs.ipynb (outdated): Estimate nested model with two scale parameters
- Nested_Model.R: Nested model estimations

Results files:
- Nested_Results.xlsx: Results from nested model formulations
- Col_MU.html: One scale parameter, for the colleges
- Uni_MU.html: One scale parameter, for the universities
- Uni_MU_with_ASCs: As above, with the addition of ASCs for schools for which no enrollment information is available