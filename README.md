# ABCD_ParRisk

These R scripts will re-execute the main analysis in our ABCD Parental Risk / Brain Structure paper [not yet published]. The 'ABCD_ParRiskDataComp.R' file takes an input directory of raw ABCD3.0 data and outputs a dataframe with just the variables of interest. The 'lmePrep.R' file will read in this output and perform several data pre-analysis steps, and output a new dataframe. The 'LME_Clean.R' fill will take this output and perform the main mixed linear model tests presented in the paper. 
