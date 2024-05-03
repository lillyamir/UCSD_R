# 1) Updating the New Americans Voter Report

## 1.1) Identifying Where Newly Naturalized Voters are From and Residing (2016-2020)

### 1.1.1) What Countries are They From?
Source: USCIS\
Associated file: "1-natz_by_country_2016_2020.R"

USCIS has posted for each FY, 2016 through 2020, an excel spreadsheet documenting country of origin and state of residence (in the United States). To get the total numbers from 2016 to 2020, I concatenated the five individual datasets row-wise. I aggregated by country of birth to get the total number of newly naturalized individuals for the entire time period. I saved this file as "natz_by_country_2016_2020.csv".


### 1.1.2) What Regions are They From?
Source: UNSD, USCIS\
Associated File: "3-by_state_natz.R"

The designated regions used to summarize country of birth are from the United Nations. I cleaned the data such that all country names in the  dataset generated from USCIS data ("natz_by_country_2016_2020.csv") and the United Nations dataset ("UNSD_Methodology.csv").

To determine which regions newly naturalized voters are from, I aggregated by region name and took the sum. For comprehension purposes, I transposed the data. With this structure, you can sort by a region and view the top countries that contribute to newly naturalized voters from that region. I saved this file as "Natz_RegionBirth_by_State.csv".

## 1.2) Swayable States
### 1.1.1) Election Margins
Source: MIT Data Lab\
Associated Files: "2-election_margins.R"

MIT published a dataset that details state-level data for number of candidate votes for all presidential and senatorial elections from 1976 to 2020. I first subsetted to the years of 2012, 2016, and 2020 and checked to make sure all states had at least two entries.

In the 2012 elections, the number of votes for the democratic candidate (Obama) was missing. I manually filled in this information, taking the numbers from Minnesota's Secretary of State.

For each election and for each state, I sorted the candidates by number of votes, then only kept the top two. To determine the margin, I aggregated by state and took the difference between the maxmimum value and minimum value.

### 1.1.2) Swayable States
Source: MIT Data Lab, USCIS\
Associated Files: "3-by_state_natz.R"

For the presidential elections, for each state, I compared the margin of victory to the number of newly naturalized voters. If the number of newly naturalized voters was higher, I noted down the state as "swayable". I noted down a state as "nearly swayable" if the numbers of newly naturalized voters were within five percent of the election margin.

For senate elections, I noted down the seats up for re-election and the predicted results. For states that were not considered "safe", I compared the number of newly naturalized citizens from 2016 to 2020 to the margin of victory for that specific seat. For those which the number of newly naturalized voters were higher, I noted it down as "swayable".

### 1.3) Total and Projected Number of Newly Naturalized
Data: USCIS, DHS\
Associated Files: "4-FY2022_projections.R"

To get the total known number of newly naturalized citizens in 2016 to 2022, I took the sum of the total number of naturalizations from FY 2016-2020 in the USCIS published Reports.

To get the projected number of newly naturalized citizens from 2016-2022, the most disaggregated data available was quarterly totals of naturalizations applications approved released by DHS. The data is available up until Q1 of 2022. I use a linear regression to predict the number of naturalizations for Q2, 3, and 4 of FY 2022.

Because there is a slight (unexplaine) discrepancy between the number of applications approved reported by DHS and number of newly naturalized reported by USCIS for each fiscal year, I calculated the total number of newly naturalized citizens by taking the sum of the USCIS counts for 2016-2020, the DHS reports of approved applications from 2021-2022(Q1), and the projections for 2022 (Q2, 3, 4).


# 2) Updating the Ohio Report

## 2.1) Ohio's Population Counts
Source: ACS 2020, USCIS, OHIO SOS\
Associated Files: "5-ohio.R"

To calculate the number of immigrants and total number of naturalized citizens in Ohio, ACS 2020 data was used. There is slight discrepancy between the ACS number of newly naturalized and the USCIS number of newly naturalized. To calculate the number of newly naturalized immigrants from 2016-2020, USCIS data was used. The number of eligible voters was taken from Ohio's secretary of state.

To calculate number of eligible citizens, acs data was subsetted to those who were not yet a citizen, but have been in the US for over five years.

To calculate hte pre-tax wage contribution, all entries coded 99999 (NA) /99998 (Missing) were removed.

To calculate age breakdowns, I used excel to sum up the newly naturalized population by age group for FY 2016-2020 published by USCIS.


## 2.2) What counties are they most prevalent in? For those counties, what regions are they from?
Source: ACS, UN, USCIS\
Associated File: "5-ohio.R"

I took the dataset outputted from Section 1.1.1 in the general report, selected the "Ohio" column and summarised across region names to get the total number of new naturalizations for each region.

Next I needed to identify for each region, which counties within Ohio were naturalized citizens most commonly found. To get the breakdown by region, I merged the UN regions with ACS data based on reported birthplace in the ACS data (BPLD). I then aggregated by the COUNTYFIPS variable and region name to calculate the number of newly naturalized citizens for each county.

## 2.3) Occupational Prevalence Among Immigrants
Source: ACS 2020\
Associated file: "5-ohio.R"

Because the variable OCC emcompasses very granular occupations, I categorized them based on the groups listed on the IPUMs website. I merged these categories into the ACS data using the OCC variable and got the total counts per occupational category.
