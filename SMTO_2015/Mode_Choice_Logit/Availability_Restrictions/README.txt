This folder contains our experimentation with availability restrictions in the 2015 SMTO mode choice model.
Two types of availability restrictions were considered:
1. Imposing a maximum travel time threshold for active modes
2. Restricting auto mode based on vehicle ownership or indicated availability

Code files:
- Active_Threshold.R (outdated): Iterate over various threshold times and print metrics to file
- Auto_Availability.R (outdated): Test various auto availability restrictions and print metrics to file
- Availability_and_Socioeconomic_Variables.R (outdated): Try combinations of socioeconomic variables with availability restrictions

Results files:
- Active_Threshold_Results.xlsx: Plots for metrics for different walking thresholds
- Auto_Availability_Results.xlsx: Key metrics for various auto availability restrictions with various active mode availabilities
- Active_Mode_Results_Summary: Results for different approaches with active modes, and specifically the distinction between walkers and bikers.
	For each model, the best threshold was found and performance reported.
- Socioeconomic_Variable_Results.xlsx: Somre results from Availability_and_Socioeconomic_Variables.R