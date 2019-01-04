# Type 2 Diabetes Mellitus Cardiovascular Outcomes (CVOT) Project

### Diabetes Data Retrieval and Analysis

Diabetes data were retrieved from ClinicalTrials.gov using the provided online search functionality.
https://clinicaltrials.gov/ct2/results?cond=Type+2+Diabetes&term=&type=Intr&rslt=&age_v=&gndr=&intr=&titles=&outc=&spons=&lead=&id=&cntry=&state=&city=&dist=&locn=&strd_s=01%2F01%2F2004&strd_e=12%2F31%2F2017&prcd_s=&prcd_e=&sfpd_s=&sfpd_e=&lupd_s=&lupd_e=&sort=nwst

Industry funded interventional studies of Type 2 Diabetes (T2DM) from 1/1/2004 - 12/31/2017 (start date).

Some of the exported (CSV) T2DM results had many study locations and formatting issues caused them to be wrapped around onto the next line. These were removed and saved as the edited file. In all retrieved CSV files the column headings were formatted and the first column of sequential numbers was removed prior to import into R.

The file T2DM_yoy.R contains an average year-on-year (yoy) percent change analysis of the clinical trial data.

CTfunctions.R contains functions used for formatting clinical trial data and for creating output tables for yoy results.

The file T2DM_regression.R contains initial exploratory analyses. This file contains a plethora of different methods for generating graphs and an analysis on raw (rather than normalized) counts of clinical trials using linear regression to find changes over time rather than percent change with yoy.

### Patent Data from Google Patents

Information on the number of patents filed for novel anti-diabetic agents was found using the following search:
https://patents.google.com/?q=(diabetes)+(("type+2")+OR+("type-2"))+(("anti-diabetic+agent")+OR+("antidiabetic+agent"))&country=US&before=filing:20151231&after=filing:20150101&status=GRANT&language=ENGLISH&type=PATENT&page=1

The dates were changed for each year, from 2004-2015. The number of results for each year was recorded and plotted.

### Links to other ClinicalTrials.gov Searches

Link for breast cancer trials search:
https://clinicaltrials.gov/ct2/results?cond=Breast+Cancer&term=&strd_s=01%2F01%2F2004&strd_e=12%2F31%2F2017&cntry=&state=&city=&dist=&Search=Search&sort=nwst&type=Intr

Link for hypertension trials search:
https://clinicaltrials.gov/ct2/results?cond=High+Blood+Pressure&term=&strd_s=01%2F01%2F2004&strd_e=12%2F31%2F2017&cntry=&state=&city=&dist=&Search=Search&sort=nwst&type=Intr

Link for obesity trials search:
https://clinicaltrials.gov/ct2/results?cond=Obesity&term=&strd_s=01%2F01%2F2004&strd_e=12%2F31%2F2017&cntry=&state=&city=&dist=&Search=Search&sort=nwst&type=Intr
