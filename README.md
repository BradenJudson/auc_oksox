# Okanagan Sockeye Salmon spawner abundance and composition analyses

## Step 1: Estimate the abundance of spawning _O. nerka_ by year and location

Use `auc_func.R` to compute and visualize the Trapezoidal Area-under-the-curve (TAUC) and apply to Sockeye counts at four study sites: i) the lower Okanagan River (above Osoyoos Lake), ii) the middle Okanagan River (above McIntyre Dam), iii) Penticton Channel, and iv) Shingle Creek. Also included is an [AUC calculator shiny app](https://bradenjudson.shinyapps.io/auc_calculator/) to expedite and standardize the AUC calculation process. 

Note that this script estimates the abundance of _O. nerka_, and thus additional biometric data are required to estimate the relative proportions of anadromous Sockeye and non-anadromous Kokanee. These aggregate estimates are then written to their respective subdirectory (e.g., `pen_channel` and  `osoyoos`) and are further processed there using the biometric data obtained from analyses conducted in the directory `deadpitch`. 

## Step 2: Estimate the composition of spawning _O. nerka_ by year and location

Annual and site-specific sampling efforts are quantified and visualized in the folder `deadpitch`. The script in this folder also characterizes the annual composition (i.e., proportion of spawners identified as Kokanee, hatchery-origin (H-O) Sockeye or natural-origin (N-O) Sockeye) of sampled carcasses for both spawning locations and writes these summary data to the directories `pen_channel` and `osoyoos`. The scripts in this directory also assess the quality of the biometric data used here by visualizing the intensity and temporal distribution of carcass data collections. 

## Step 3: Partition aggregate spawner abundances into fractions of Kokanee and H-O and N-O Sockeye Salmon

Using the biometric data obtained in Step 2, the abundance of _O. nerka_ is partitioned into three discrete categories: Kokanee (<35cm fork-length) or H-O or N-O Sockeye (presence or absence of marked otoliths). Some accompanying code tidies these data into several wide-format output files. 



