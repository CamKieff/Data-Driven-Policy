# FDA Reported Use of Patient Experience Data in 2018 Drug Approvals

### Data

The file `2019-02-08_patientExperience.csv` contains the raw, hand-extracted data from FDA Review Documentation. This includes the boxes marked in the 21st Century Cures Act Section 3001 patient experience data (PED) table if applicable. The products are all available reviews back to 11/2/2017. The dataset is larger than the final dataset used in the analysis. Some products are supplements or were approved through the 505(b)(2) pathway, as opposed to being new molecular entities (NME).

`2019-01-31_novel_approvals.csv` includes other data related to approved NMEs for 2018, specifically special regulatory designations (priority review [PR], breakthrough therapy designation [BTD], orphan drug designation [ODD] and fast track designation [FTD]) The approval designations were hand-extracted from CDER's [Novel Drug Approvals for 2018][1] document.

### Analysis

`PED-analysis.R` includes the analysis code for combining the datasets processing the data, and graphing the results. All changes to the raw checked box data are reflected in the code.

`2019_04-18_formatted_PED_output.csv` is a table of NMEs that reflects the formatting changes that were made during the analysis. A formatted version of this table was included as supplementary Table 1 in the published manuscript.

[1]: https://www.fda.gov/media/120357/download
