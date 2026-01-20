# household-consumption-analysis-hces-2022
## Data Source
This analysis uses publicly available data from the National Sample Survey Office (NSSO), Government of India:
- NSS Round 79: Household Consumption Expenditure Survey (2022-23)
- Data files: hces22_lvl_3.dta, hces22_lvl_14.dta, hces22_lvl_15.dta
  
  ## How to Run
1. Download HCES data files from NSSO
2. Set your working directory to the folder containing the data
3. Run `hces_consumption_analysis.R`

Analysis of Household Consumption Patterns in India (HCES 2022-23)
This project analyzes household consumption expenditure across India using data from the NSS Round 79 Household Consumption Expenditure Survey (2022-23).

Key analyses:
1. Calculated monthly per capita consumption expenditure (MPCE) and total household expenditure across rural and urban sectors
2. Examined consumption patterns across income quintiles to understand spending distribution
3. Analyzed expenditure differences by social group (SC/ST/OBC/General) and religion
4. Created geographic visualization showing state-wise consumption patterns across India

Tools used: R (tidyverse, ggplot2, sf for mapping)

Main findings:
1. Urban households show significantly higher consumption expenditure than rural households
2. Clear expenditure gradients exist across income quintiles and social groups
3. Central Indian states exhibit the lowest per capita consumption levels

