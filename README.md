# Importation models for travel-related SARS-CoV-2 cases reported in Newfoundland and Labrador during the COVID-19 pandemic.

The R code and data for the mansucript `Importation models for travel-related SARS-CoV-2 cases reported in Newfoundland and Labrador during the COVID-19 pandemic`
[medRxiv](https://www.medrxiv.org/content/10.1101/2023.06.08.23291136v1). Royal Society Open Science.

Figure 1. in the main text can be produced from Volume-FocusOrigin.Rmd. The only issue is the dependency on 'ExtractTDF.csv'* see below, however, this effects panel A. The other panels use the TotalTraVol.csv file which is produced from using the ExtractTDF.csv file and available in the Data/TraVolData folder.

Figure 2. is produced by running plots.R which requires running modelling.R first.

Table 3. (and related tables in the Supplementary Material) are produced from modelling.R

epimodel-versions produces Appendix F in the Supplementary Material.

*The travel declaration form data is not public, so the data file 'ExtractTDF.csv' required to run the 'Volume-FocusOrigin.Rmd' script has not been shared here. This file is subject to a Health Research Ethics Board approval. The travel declaration form data can be obtained with an ethics application supported by Newfoundland and Labrador Health Services, Digital health. Please contact ahurford@mun.ca (corresponding author) for support in applying for access to the Travel Declaration Form data.
