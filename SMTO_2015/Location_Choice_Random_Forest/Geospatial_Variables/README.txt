This folder includes our work on geospatial variables for the Random Forest location choide model for SMTO 2015.
By geospatial variables, we refer to distances (absolute and relative ranks), zones and planning districts, and coordinates.
These variables have to do with students' home zones (and, possibly, their relative location to the campuses).

Code files:
- Feature_Engineering.ipynb (documented): Feature engineering and selection for distance-related variables: 
- PDs_Coordinates.ipynb: Results with planning districts and coordinates

Results files:
- Closests_vs_Distances.xlsx: Comparison of results
	Closests: Columns indicating the schools' relative distance rank (i.e. which campus is closest? second-closest? etc.)
	Distances: Columns indicating distances to campuses
- N_Closest_and_Distances.xlsx: See above, where the number of columns passed is varied
- N_Distances.xlsx: The number of distances passed is varied
- Various_Features_Results.xlsx: Results for various formats of geospatial variables