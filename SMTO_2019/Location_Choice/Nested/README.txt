This folder contains our work on a nested logit model for the 2019 SMTO location choice problem.
We estimate models using both Biogeme (Python) and mlogit (R).
The nesting structure is such that the universities are in one nest and the colleges in another.

Code files (outdated):
- One_MU.ipynb: Estimate nested model with one scale parameter
- Two_MUs.ipynb: Estimate nested model with two scale parameters
- Nested_Model.R: Various nested model estimations

Results files:
- Col_MU.html: One scale parameter, for the colleges
- Uni_MU.html: One scale parameter, for the universities
- Uni_MU_with_ASCs: As above, with the addition of ASCs for schools for which no enrollment information is available