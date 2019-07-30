# Impact of FDA-Required Cardiovascular Outcome Trials on Type 2 Diabetes Clinical Study Initiation From 2008 to 2017

Kieffer, C. M., & Robertson, A. S. (2019). Impact of FDA-Required Cardiovascular Outcome Trials on Type 2 Diabetes Clinical Study Initiation From 2008 to 2017. _Therapeutic Innovation & Regulatory Science_. https://doi.org/10.1177/2168479019860122

### Data Retrieval and Analysis

Diabetes data were retrieved from [ClinicalTrials.gov][1]

Industry funded interventional studies of Type 2 Diabetes Mellitus (T2DM) with start dates between January 1, 2000 and December 31, 2017 were downloaded. Some of the exported (CSV) T2DM results had many study locations and formatting issues caused them to be wrapped around onto the next line. These were removed by hand and saved as an edited file. In all retrieved CSV files the column headings were formatted and the first column of sequential numbers was removed prior to import into R.

Count data for all trials was extracted manually (these searches were too large to be downloaded as CSV files from ClinicalTrials.gov search). A search with no disease term was performed for a single year from January 1st of that year to December 31st.  Trials were interventional and in either phase 1, 2, or 3. For industry trials, the "industry" trial box was selected for the search. For non-industry trials, the "NIH", "other US Federal agencies", and "all others" boxes were selected.

The file T2DM_yoy.R contains an average year-on-year (yoy) percent change analysis of the clinical trial data.

CTfunctions.R contains functions used for formatting clinical trial data and for creating output tables for yoy results.

The file T2DM_regression.R is an unfinished collection of initial exploratory analyses using linear regression to find changes over time rather than percent change with yoy.

The data folder contains the data used to perform the analysis. The sub-folder OtherTherapeuticAreas contains searches for a variety of other therapeutic areas that were not comorbidities of diabetes and were not included in the analysis.

### Patent Data from Google Patents

Information on the number of patents filed for novel anti-diabetic agents was found using the following search:
https://patents.google.com/?q=(diabetes)+(("type+2")+OR+("type-2"))+(("anti-diabetic+agent")+OR+("antidiabetic+agent"))&country=US&before=filing:20151231&after=filing:20150101&status=GRANT&language=ENGLISH&type=PATENT&page=1

The search was manually performed for each year, from 2000-2015. The number of results for each year was recorded and plotted. 2016-2017 were omitted from the analysis because of the lag between patent filing and publication.

### Links to other ClinicalTrials.gov Searches

[Clink for breast cancer trials search][2]

[Clink for hypertension trials search][3]

[Clink for obesity trials search.][4]

[Click for nephropathy trials search][5]

[1]:https://clinicaltrials.gov/ct2/results?cond=Type+2+Diabetes&term=&type=Intr&rslt=&age_v=&gndr=&intr=&titles=&outc=&spons=&lead=&id=&cntry=&state=&city=&dist=&locn=&strd_s=01%2F01%2F2000&strd_e=12%2F31%2F2017&prcd_s=&prcd_e=&sfpd_s=&sfpd_e=&lupd_s=&lupd_e=&sort=nwst

[2]:https://clinicaltrials.gov/ct2/results?cond=Breast+Cancer&term=&strd_s=01%2F01%2F2000&strd_e=12%2F31%2F2017&cntry=&state=&city=&dist=&Search=Search&sort=nwst&type=Intr

[3]:https://clinicaltrials.gov/ct2/results?cond=High+Blood+Pressure&term=&strd_s=01%2F01%2F2000&strd_e=12%2F31%2F2017&cntry=&state=&city=&dist=&Search=Search&sort=nwst&type=Intr

[4]:https://clinicaltrials.gov/ct2/results?cond=Obesity&term=&strd_s=01%2F01%2F2000&strd_e=12%2F31%2F2017&cntry=&state=&city=&dist=&Search=Search&sort=nwst&type=Intr

[5]:https://clinicaltrials.gov/ct2/results?cond=nephropathy&term=&strd_s=01%2F01%2F2000&strd_e=12%2F31%2F2017&cntry=&state=&city=&dist=&Search=Search&sort=nwst&type=Intr
