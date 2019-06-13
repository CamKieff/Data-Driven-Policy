# Analysis of the use of the Section 3001 Table to include Patient Experience Data in FDA Drug Approval Packages

### Data
The file `2019-02-08_patientExperience.csv` contains the raw, hand-extracted data from FDA approval packages. This includes the boxes marked in the patient experience data (PED) table if applicable.  The products are all available reviews back to 11/2/2017. Some products are supplements or were approved through the 505(b)2 pathway, as opposed to being new molecular entities (NME). The dataset is larger than the final dataset used in the analysis.

`2019-01-31_novel_approvals_withBW.csv` includes other data related to approved NMEs for 2018. This includes boxed warnings and information regarding expedited approval designations (such as priority review, breakthrough therapy, orphan designation, etc.) The approval designations were hand-extracted from CDER's [Novel Drug Approvals for 2018][1] document. Boxed warning information was collected using the openFDA [structured product labeling API][2] (code not shown).

`PED-analysis.R` includes the analysis code for combining the datasets processing the data, and graphing the results. If any changes were made to the boxes selected, they are reflected in the code.

`2019_04-18_formatted_PED_output.csv` is a table of NMEs that reflects the formatting changes that were made during the analysis.

[1]: https://www.fda.gov/drugs/developmentapprovalprocess/druginnovation/ucm592464.htm
[2]:https://open.fda.gov/apis/drug/label/
