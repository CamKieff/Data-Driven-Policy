# Type 2 Diabetes Mellitus Cardiovascular Outcomes Project

### Diabetes Data Retreival and Analysis

Diabetes data were retrieved from ClinicalTrials.gov using the provided online search functionality. 
https://clinicaltrials.gov/ct2/results?cond=Type+2+Diabetes&term=&type=Intr&rslt=&age_v=&gndr=&intr=&titles=&outc=&spons=&lead=&id=&cntry=&state=&city=&dist=&locn=&strd_s=01%2F01%2F2004&strd_e=12%2F31%2F2017&prcd_s=&prcd_e=&sfpd_s=&sfpd_e=&lupd_s=&lupd_e=&sort=nwst

Industry funded interventional studies of Type 2 Diabetes (T2DM) from 1/1/2004 - 12/31/2017 (start date).

Some of the exported (CSV) T2DM results had many study locations and formatting issues caused them to be wrapped around onto the next line. These were removed and saved as the edited file. In all retrieved CSV files the column headings were formatted and the first column of sequential numbers was removed prior to import into R.

The file T2DM.R contains the analyses of these retrieved files.


### Links to other ClinicalTrials.gov Searches

Link for breast cancer trials search:
https://clinicaltrials.gov/ct2/results?cond=Breast+Cancer&term=&strd_s=01%2F01%2F2004&strd_e=12%2F31%2F2017&cntry=&state=&city=&dist=&Search=Search&sort=nwst&type=Intr

Link for hypertension trials search:
https://clinicaltrials.gov/ct2/results?cond=High+Blood+Pressure&term=&strd_s=01%2F01%2F2004&strd_e=12%2F31%2F2017&cntry=&state=&city=&dist=&Search=Search&sort=nwst&type=Intr

Link for obesity trials search:
https://clinicaltrials.gov/ct2/results?cond=Obesity&term=&strd_s=01%2F01%2F2004&strd_e=12%2F31%2F2017&cntry=&state=&city=&dist=&Search=Search&sort=nwst&type=Intr
