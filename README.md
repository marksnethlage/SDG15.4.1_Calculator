#GMBA SDG 15.4.1 Calculator

## Purpose:
This project contains a set of scripts used to calculate mountain biodiversity 
utilizing the GMBA mountain inventory V2.0. Contents of this project are aimed at 
improving reporting progress towards SDG 15.4.1 by disaggregating calculations 
to the mountain level 

## Setup: 

The results of this analysis are reproducible using only this repo, and all r
required input data files. To do so:

1. Clone this repository
2. Download the required data files (request access from **____**)
3. Run the desired analysis (see below for details)

While most input files are static for any analysis, some may be updated annually
Please refer to the file Input_File_Details in the home directory of this project
for more detailed information on the required data files.

## Included Analyses: 

### Intersection_Official.R :
Calculate the % coverage of mountainous Key Biodiversity Areas (KBAs) by 
Protected Areas (PAs) using the official SDG method. For any KBA that overlaps
with a GMBA polygon, the _full_ KBA is overlapped with all PAs to determine the 
% coverage. For KBAs that increase in size, each additional entry is the
additional overlap. Results are _not_ cumulative. Adapted from the Birdlife 
International script:  (https://github.com/BirdLifeInternational/kba-overlap)

#### Output: 
   + /results/finaltab_official_'YEAR_RUN'.csv" : table with % added coverage for
   each KBA
   + /results/results_country/... : folder with country-specific results
   
### Intersection_GMBA.R

#### Output: 
  + /results/finaltab_gmba_'YEAR_RUN'.csv: table with % coverage of each 
  mountainous area of each KBA as identified by the GMBA polygons
  + /results/results_mt/... : folder with mountain range-specific results

  
## Additional Scripts:

### Data_Prep_for_Visuals.R

To simplify aggregation and translate the data to show cumulative coverage for 
every year, this script reads in the results of a previous run (updated by user
in section 1.2), and outputs multiple aggregation files.

#### Output: 

  + **finaltab_cumulative.csv** : cumulative coverage by KBA for every year 
  starting with the minimum year in the result file. Each row is the total 
  coverage for a KBA in that year.
  + **finaltab_cumulative_country.csv** : aggregated to the country level 
  **by total area**
  + **finaltab_cumulative_mtrange.csv** : aggregated to the mountain range 
  **by total area**
  + **finaltab_cumulative_mtrange_country.csv** : aggregated to the mountain 
  range by country **by total area**
  + **finaltab_cumulative_country_avg.csv** : aggregated to get the average 
  coverage of KBAs within a country
  + **finaltab_cumulative_mtrange_avg.csv** : aggregated to get the average 
  coverage of KBAs within a mountain range
  + **finaltab_cumulative_mtrange_country_avg.csv** : aggregated to get the 
  average coverage of KBAs within a country
  + **finaltab_cumulative_parentrange.csv** : aggregated to the 
  intermediate range **by total area**
  + **finaltab_cumulative_parentrange_country.csv** : aggregated to the 
  intermediate range **by total area**


## Contributors: 


