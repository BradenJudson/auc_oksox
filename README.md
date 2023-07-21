# Okanagan Sockeye Salmon spawner abundance and composition analyses
Function to compute and visualize the Area-Under-the-Curve (AUC) for spawning salmonids.

# Step 1: Estimate the abundance of spawning _O. nerka_ by year and location

Use `auc_func.R` to compute and visualize the Trapezoidal Area-under-the-curve (TAUC) and apply to Sockeye counts at 1) the Lower Okanagan River, and 2) Penticton Channel.

Note that this script estimates the abundance of _O. nerka_, and thus additional biometric data are required to estimate the relative proportions of anadromous Sockeye, non-anadromous Kokanee and their hybrids. These aggregate estimates are then written to their respective subdirectory and processed there. See the following two subfolder descriptions:

1. `pen_channel`

2. `osoyoos`

# Step 2: Estimate the composition of spawning _O. nerka_ by year and location
Annual and site-specific sampling efforts are quantified and visualized in the folder `deadpitch`. The script in this folder also characterizes the annual composition of sampled carcasses for both spawning locations and writes these summary data to the directories `pen_channel` and `osoyoos`. 
