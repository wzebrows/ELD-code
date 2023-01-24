# ELD-code
Code and data (from public sources) to support the article: The Electronic Logging Device Mandate’s Impact on Rates for Refrigerated Truck Agricultural Transportation.

data holds the raw data files, origins described in paper.
eld_build.R creates the analysis dataset rtruck_analysis.csv from the data in the data folder, as well as ELD_month.csv, ELD_quarter.csv, and ELD_year.csv for aggregates by larger time periods. 
eld_did.R creates the visuals and analysis for the paper.
output stores the results of eld_did.R
