#load libraries
library(tidycensus)
library(dplyr)
library(tidyverse)
library(magrittr)
library(lubridate)
library(sf)

#load variables from 2010 ACS tables
v_10 <- load_variables(2010,"acs5",cache=T) %>% mutate(table=str_sub(name,1,6))
v_10s <- load_variables(2010,"acs5/subject",cache=T) %>% mutate(table=str_sub(name,1,6))

#input your personal census api key. it can be obtained here: https://api.census.gov/data/key_signup.html
census_api_key("c7d47a02c2964d2e2597465d0be8d19a759f40b6")

#census tract assignments
ntas_tracts <-read_csv("ntas_tracts.csv")

#get 2010 tract-level population data of census tracts in Queens
population <-get_acs(geography = "county",
                     year = 2010, table =c("B01003"),
                     state = "NY", county = c("Queens"),
                     survey = "acs5",
                     geometry = TRUE, output="wide")

# get 2010 tract-level household data of census tracts in Queens
households <- get_acs(geography = "county",
                      year = 2010, table = c("B11012"),
                      state = "NY", county = c("Queens"),
                      survey = "acs5",
                      geometry = TRUE, output="wide")


#sum population and household data by the NTA
ntas_pop_hh_summary <- population %>%
  summarize(population = sum(B01003_001E))

hh_summary <- households %>%
  summarize(households = sum(B11012_001E))

#save output to csv
write.csv(ntas_pop_hh_summary, "ntas_pop_hh_2010_summary.csv")

#get 2010 male/female data of census tracts in Queens
ntas_sex <- get_acs(geography = "county",
                    year = 2010, table = c("B01001"),
                    state = "NY", county = c("Queens"),
                    survey = "acs5",
                    geometry = TRUE, output="wide")

#sum male/female data by library service area
ntas_sex_m_f_summary <- ntas_sex %>%
  summarize(total = sum(B01001_001E),
            male = sum(B01001_002E)/total,
            female = sum(B01001_026E)/total)

#save output to csv
write.csv(ntas_sex_m_f_summary, "ntas_sex_m_f_2010.csv")

#get 2010 tract-level race data for census tracts in Queens
race <- get_acs(geography = "county",
                year = 2010, table = c("B02001"),
                state = "NY", county = c("Queens"),
                survey = "acs5",
                geometry = TRUE, output="wide")

#group race data by library service area
ntas_race_summary <- race %>%
  summarize(total = sum(B02001_001E),
            white = sum(B02001_002E)/total,
            black = sum(B02001_003E)/total,
            asian = sum(B02001_005E)/total,
            other = sum(B02001_004E, B02001_006E, B02001_007E, B02001_008E)/total)

#save to csv
write.csv(ntas_race_summary, "race_summary_2010.csv")

#get 2010 tract-level data on population hispanic or latino origin
hispanic <- get_acs(geography = "county",
                    year = 2010, table = c("B03003"),
                    state = "NY", county = c("Queens"),
                    survey = "acs5",
                    geometry = TRUE, output="wide")

#group data on hispanic/latino origin population by library service area
ntas_hispanic_summary <- hispanic %>%
  summarize(total = sum(B03003_001E),
            hispanic = sum(B03003_003E)/total)

#write to csv file
write.csv(ntas_hispanic_summary, "hispanic_summary_2010.csv")

#get 2010 tract-level data on race and ethnicity
race_ethnicity <- get_acs(geography = "county",
                          year = 2010, table = c("B03002"),
                          state = "NY", county = c("Queens"),
                          survey = "acs5",
                          geometry = TRUE, output="wide")

#group data by on race/ethnicity by library service area
ntas_ethnicity_summary <- race_ethnicity %>%
  summarize(total = sum(B03002_001E),
            white_nonhispanic = sum(B03002_003E)/total,
            black_nonhispanic = sum(B03002_004E)/total,
            asian_nonhispanic = sum(B03002_006E)/total,
            other_nonhispanic = sum(B03002_005E, B03002_007E, B03002_008E, B03002_009E)/total,
            hispanic_ethnicity= sum(B03002_012E)/total)

#write to csv file                                                              #$$$$$$$$$$$
write.csv(ntas_ethnicity_summary, "ethnicity_summary_2010.csv")

#get 2010 tract-level data on age distribution in Queens
ages <- get_acs(geography = "county",
                year = 2010, table = c("B01001"),
                state = "NY", county = c("Queens"),
                survey = "acs5",
                geometry = TRUE, output="wide")

#group data by age distributions by library service area
ntas_ages_summary <- ages %>%
  summarize(total = sum(B01001_001E),
            under_5 = sum(B01001_003E, B01001_027E)/total,
            five_to_9 = sum(B01001_004E, B01001_028E)/total,
            ten_to_14= sum(B01001_005E, B01001_029E)/total,
            fifteen_to_19 = sum(B01001_006E, B01001_030E, B01001_007E, B01001_031E)/total,
            twenty_to_29 = sum(B01001_008E, B01001_032E, B01001_009E, B01001_033E, B01001_010E, B01001_034E, B01001_011E, B01001_035E)/total,
            thirty_to_44 = sum(B01001_012E, B01001_036E, B01001_013E, B01001_037E, B01001_014E, B01001_038E)/total,
            fortyfive_to_64 = sum(B01001_015E, B01001_039E, B01001_016E, B01001_040E, B01001_017E, B01001_041E, B01001_018E, B01001_042E, B01001_019E, B01001_043E)/total,
            sixtyfive_older = sum(B01001_020E, B01001_044E, B01001_021E, B01001_045E, B01001_022E, B01001_046E, B01001_023E, B01001_047E, B01001_024E, B01001_048E, B01001_025E, B01001_049E)/total)

#write age data output to csv
write.csv(ntas_ages_summary, "ages_summary_2010.csv")

#group 2010 data on ages 18 and over by library service area
ntas_ages_18_plus <- ages %>%
  summarize(total = sum(B01001_001E),
            eighteen_and_over = sum(B01001_007E, B01001_031E, B01001_008E, B01001_032E, B01001_009E, B01001_033E, 
                                    B01001_010E, B01001_034E, B01001_011E, B01001_035E, B01001_012E, B01001_036E, 
                                    B01001_013E, B01001_037E, B01001_014E, B01001_038E, B01001_015E, B01001_039E, 
                                    B01001_016E, B01001_040E, B01001_017E, B01001_041E, B01001_018E, B01001_042E, 
                                    B01001_019E, B01001_043E, B01001_020E, B01001_044E, B01001_021E, B01001_045E, 
                                    B01001_022E, B01001_046E, B01001_023E, B01001_047E, B01001_024E, B01001_048E, 
                                    B01001_025E, B01001_049E)/total)

#write ages 18 and over data to csv
write.csv(ntas_ages_18_plus, "ages_18_plus_summary_2010.csv")

#get 2010 tract-level data on education levels in Queens
education <- get_acs(geography = "county",
                     year = 2010, table = c("B15002"),
                     state = "NY", county = c("Queens"),
                     survey = "acs5",
                     geometry = TRUE, output="wide")

#group data by on education levels by library service area
ntas_edu_summary <- education %>%
  summarize(total_edu = sum(B15002_001E),
            hs_diploma_hse = sum(B15002_011E, B15002_028E)/total_edu,           #B15002_011E, B15002_028E
            some_college = sum(B15002_012E, B15002_029E, B15002_013E, B15002_030E)/total_edu,
            associates_degree = sum(B15002_014E, B15002_031E)/total_edu,
            bachelors_degree = sum(B15002_015E, B15002_032E)/total_edu,
            grad_or_professional_degree = sum(B15002_016E, B15002_033E, B15002_017E, B15002_034E, B15002_018E, B15002_035E)/total_edu,
            less_than_hs = sum(B15002_003E, B15002_020E, B15002_004E, B15002_021E, B15002_005E, B15002_022E, B15002_006E, B15002_023E, B15002_007E, B15002_024E, B15002_008E, B15002_025E, B15002_009E, B15002_026E, B15002_010E, B15002_027E)/total_edu)

#write to csv
write.csv(ntas_edu_summary, "edu_levels_2010.csv")

#get 2010 Queens tract-level data for household languages
hh_languages <- get_acs(geography = "county",
                        year = 2010, table = c("B16002"),                       #B16002? Before C16002
                        state = "NY", county = c("Queens"),
                        survey = "acs5",
                        geometry = TRUE, output="wide")

#group data by on household languages by library service area
ntas_languages_summary <- hh_languages %>%
  summarize(total = sum(B16002_001E),
            english = sum(B16002_002E)/total,
            spanish = sum(B16002_003E)/total,
            other_indo_european = sum(B16002_006E)/total,
            asian_pi = sum(B16002_009E)/total,
            other = sum(B16002_012E)/total)

#write to csv 
write.csv(ntas_languages_summary, "languages_summary_2010.csv")

#get 2010 Queens tract-level data on limited english proficiency
lep <- get_acs(geography = "county",
               year = 2010, table = c("B16004"),
               state = "NY", county = c("Queens"),
               survey = "acs5",
               geometry = TRUE, output="wide")

#group LEP data by library service area
ntas_tracts_lep_summary <- lep %>%
  summarize(total = sum(B16004_001E),
            lep = sum(B16004_006E, B16004_007E, B16004_008E, B16004_011E, B16004_012E, B16004_013E, B16004_016E, B16004_017E, B16004_018E, B16004_021E, B16004_022E, B16004_023E, B16004_028E, B16004_029E, B16004_030E, B16004_033E, B16004_034E, B16004_035E, B16004_038E, B16004_039E, B16004_040E, B16004_043E, B16004_044E, B16004_045E, B16004_050E, B16004_051E, B16004_052E, B16004_055E, B16004_056E, B16004_057E, B16004_060E, B16004_061E, B16004_062E, B16004_065E, B16004_066E, B16004_067E)/total)

#write to csv
write.csv(ntas_tracts_lep_summary, "lep_summary_2010.csv")

#get 2010 Queens tract-level data on foreign-born
foreign_born <- get_acs(geography = "county",
                        year = 2010, table = c("B05012"),
                        state = "NY", county = c("Queens"),
                        survey = "acs5",
                        geometry = TRUE, output="wide")

#group data on foreign-born by library service area
ntas_tracts_foreign_born_summary <- foreign_born %>%
  summarize(total = sum(B05012_001E),
            foreign_born = sum(B05012_003E)/total)

#write to csv
write.csv(ntas_tracts_foreign_born_summary, "foreign_born_2010.csv")

#get 2010 Queens tract-level income data 
income <- get_acs(geography = "county",
                  year = 2010, table = c("B19001"),
                  state = "NY", county = c("Queens"),
                  survey = "acs5",
                  geometry = TRUE, output="wide")

#group data on income levels by library service area
ntas_tracts_income_summary <- income %>%
  summarize(total = sum(B19001_001E),
            less_than_24999 = sum(B19001_002E, B19001_003E, B19001_004E, B19001_005E)/total,
            twentyfivek_to_49999 = sum(B19001_006E, B19001_007E, B19001_008E, B19001_009E, B19001_010E)/total,
            fiftyk_to_99999 = sum(B19001_011E, B19001_012E, B19001_013E)/total,
            onehundredk_and_up = sum(B19001_014E, B19001_015E, B19001_016E, B19001_017E)/total)

#write to csv
write.csv(ntas_tracts_income_summary, "income_summary_2010.csv")

#get 2010 Queens tract-level data on poverty
povertyA <- get_acs(geography = "county",
                   year = 2010, table = c("B17020A"),
                   state = "NY", county = c("Queens"),
                   survey = "acs5",
                   geometry = TRUE, output="wide")

povertyB <- get_acs(geography = "county",
                    year = 2010, table = c("B17020B"),
                    state = "NY", county = c("Queens"),
                    survey = "acs5",
                    geometry = TRUE, output="wide")

povertyC <- get_acs(geography = "county",
                    year = 2010, table = c("B17020C"),
                    state = "NY", county = c("Queens"),
                    survey = "acs5",
                    geometry = TRUE, output="wide")

povertyD <- get_acs(geography = "county",
                    year = 2010, table = c("B17020D"),
                    state = "NY", county = c("Queens"),
                    survey = "acs5",
                    geometry = TRUE, output="wide")

povertyE <- get_acs(geography = "county",
                    year = 2010, table = c("B17020E"),
                    state = "NY", county = c("Queens"),
                    survey = "acs5",
                    geometry = TRUE, output="wide")

povertyF <- get_acs(geography = "county",
                    year = 2010, table = c("B17020F"),
                    state = "NY", county = c("Queens"),
                    survey = "acs5",
                    geometry = TRUE, output="wide")

povertyG <- get_acs(geography = "county",
                    year = 2010, table = c("B17020G"),
                    state = "NY", county = c("Queens"),
                    survey = "acs5",
                    geometry = TRUE, output="wide")

poverty_population <- get_acs(geography = "county",
                              year = 2010, table = c("B17001"),
                              state = "NY", county = c("Queens"),
                              survey = "acs5",
                              geometry = TRUE, output="wide")

#group data on poverty by nta_name census tract assignments
ntas_tracts <- ntas_tracts %>% mutate(GEOID=as.character(GEOID))
ntas_tracts_poverty <- ntas_tracts %>% left_join(povertyG, by="GEOID")  %>%
  left_join(povertyF, by="GEOID")  %>%
  left_join(povertyE, by="GEOID")  %>%
  left_join(povertyD, by="GEOID")  %>%
  left_join(povertyC, by="GEOID")  %>%
  left_join(povertyB, by="GEOID")  %>%
  left_join(povertyA, by="GEOID") %>%
  left_join(poverty_population, by="GEOID")

#group data on poverty by library service area
ntas_tracts_poverty_summary <- ntas_tracts_poverty %>%
  summarize(total = sum(B17020A_001E, B17020B_001E, B17020C_001E, B17020D_001E, B17020E_001E, B17020F_001E, B17020G_001E, na.rm = TRUE),
            poverty = sum(B17020A_002E, B17020B_002E, B17020C_002E, B17020D_002E, B17020E_002E, B17020F_002E, B17020G_002E, na.rm = TRUE)/total,
            poverty_pop = sum(B17001_002E, na.rm = TRUE))


#write to csv
write.csv(ntas_tracts_poverty_summary, "poverty_summary_2010.csv")



#get 2010 queens-wide denominators
#population ages 5 and older
pop_5_and_older <-get_acs(geography = "county",
                          year = 2010, table = c("B16004"),
                          state = "NY", county = c("Queens"),
                          survey = "acs5",
                          geometry = TRUE, output="wide")

#get 2010 Queens tract-level data on unemployment rate
unemployment <- get_acs(geography = "county",
                        year = 2010, table = c("S2301_C04"),
                        state = "NY", county = c("Queens"),
                        survey = "acs5",
                        geometry = TRUE, output="wide")

#group data on poverty by library service area
ntas_tracts_unemployment_summary <- unemployment %>%
  summarize(unemployment_rate = mean(S2301_C04_001E, na.rm = TRUE))

#write to csv
write.csv(ntas_tracts_unemployment_summary, "unemployment_2010.csv")

#get 2010 Queens tract-level data on median household income
median_hh_income <- get_acs(geography = "county",
                            year = 2010, table = c("B19013"),
                            state = "NY", county = c("Queens"),
                            survey = "acs5",
                            geometry = TRUE, output="wide")

#group data on median hh income by library service area
ntas_tracts_median_hh_income_summary <- median_hh_income %>%
  summarize(median_hh_income = weighted.mean(B19013_001E, na.rm = TRUE)) 

#write to csv
write.csv(ntas_tracts_median_hh_income_summary, "median_hh_income_2010.csv")

#get 2010 Queens tract-level data on avg hh size
avg_hh_size <- get_acs(geography = "county",
                       year = 2010, table = c("S1101_C01"),
                       state = "NY", county = c("Queens"),
                       survey = "acs5",
                       geometry = TRUE, output="wide")

#group data on avg hh size by library service area
ntas_tracts_avg_hh_size_summary <- avg_hh_size %>%
  summarize(avg_hh_size = mean(S1101_C01_002E, na.rm = TRUE)) 

#write to csv
write.csv(ntas_tracts_avg_hh_size_summary, "avg_hh_size_2010.csv")

#get 2010 Queens tract-level data on hh under 18 y.o./h.h., any 60 years and over, hh living alone
hh_and_families <- get_acs(geography = "county",
                           year = 2010, table = c("S1101_C01"),
                           state = "NY", county = c("Queens"),
                           survey = "acs5",
                           geometry = TRUE, output="wide")

#group data on hh under 18 yo by library service area
ntas_tracts_hh_under_18_summary <- hh_and_families %>%
  summarize(hh_under_18_yo = mean(S1101_C01_010E, na.rm = TRUE), hh_over_60_yo = mean(S1101_C01_011E, na.rm = TRUE), hh_living_alone = mean(S1101_C01_012E, na.rm = TRUE)) 

#write to csv file                                                              #$$$$$$$$$$$
write.csv(ntas_tracts_hh_under_18_summary, "hh_under_18_2010.csv")

#Get Queens tract-level data on hh with children
hh_with_children <- get_acs(geography = "county",
                            year = 2010, table = c("B23007"),
                            state = "NY", county = c("Queens"),
                            survey = "acs5",
                            geometry = TRUE, output="wide")

households <- get_acs(geography = "county",
                      year = 2010, table = c("B11012"),
                      state = "NY", county = c("Queens"),
                      survey = "acs5",
                      geometry = TRUE, output="wide")

#group data on hh with children by library service area
ntas_tracts_hh_with_children_summary <- hh_with_children %>%
  summarize(total = sum(B23007_001E),
            total_hh = sum(B11012_001E),
            hh_with_children = sum(B23007_002E)/total_hh)

#write to csv file
write.csv(ntas_tracts_hh_with_children_summary, "hh_with_children_2010.csv")


#get 2010 Queens tract-level data on renter occupied housing                         #$$$$$$$$$$$
ro_housing <- get_acs(geography = "county",
                      year = 2010, table = c("B25003"),
                      state = "NY", county = c("Queens"),
                      survey = "acs5",
                      geometry = TRUE, output = "wide")

#group data on renter occupied housing by library service area                  
ntas_tracts_ro_housing_summary <- ro_housing %>%
  summarize(total = sum(B25003_001E),
            renter_occupied_housing = sum(B25003_003E)/total)

#write to csv file                                                              
write.csv(ntas_tracts_ro_housing_summary, "hh_ro_housing_2010.csv")

#get 2010 Queens tract-level data on non-family hh                                   
non_family_hh <- get_acs(geography = "county",
                         year = 2010, table = c("B11016"),
                         state = "NY", county = c("Queens"),
                         survey = "acs5",
                         geometry = TRUE, output = "wide")

#group data on non-family hh by library service area
ntas_tracts_non_family_hh_summary <- non_family_hh %>%
  summarize(total = sum(B11016_001E), 
            non_family_households = sum(B11016_009E)/total)

#write to csv file
write.csv(ntas_tracts_non_family_hh_summary, "non_family_hh_2010.csv")

#get 2010 Queens tract-level data on single family headed hh
single_family_hh <- get_acs(geography = "county",
                            year = 2010, table = c("B11003"),
                            state = "NY", county = c("Queens"),
                            survey = "acs5",
                            geometry = TRUE, output = "wide")

#group data on single family headed hh by library service area
ntas_tracts_single_family_hh_summary <- single_family_hh %>%
  summarize(total = sum(B11003_001E),
            single_family_headed_households = sum(B11003_008E)/total)

#write to csv file
write.csv(ntas_tracts_single_family_hh_summary, "single_family_headed_hh_2010.csv")

#get 2010 Queens tract-level data on hh with internet access
hh_with_internet <- get_acs(geography = "county",
                            year = 2010, table = c("B28002"),
                            state = "NY", county = c("Queens"),
                            survey = "acs5",
                            geometry = TRUE, output = "wide")

#get 2010 Queens tract-level data on hh with computers
hh_with_computer <- get_acs(geography = "county",
                            year = 2010, table = c("B28001"),
                            state = "NY", county = c("Queens"),
                            survey = "acs5",
                            geometry = TRUE, output = "wide")

#group data on hh with internet access and hh with computers by nta_name census tract assignments
ntas_internet_computers <- ntas_tracts %>% left_join(hh_with_computer, by="GEOID") %>%
  left_join(hh_with_internet, by="GEOID")

#group data on hh with internet access and hh with computers by library service area
internet_computer_summary <- ntas_internet_computers %>%
  group_by(`nta_name`) %>%
  summarize(total_internet = sum(B28002_001E),
            broadband = sum(B28002_004E)/total_internet,
            dialup_other = sum(B28002_003E, B28002_012E)/total_internet,
            no_internet = sum(B28002_013E)/total_internet,
            total_computers = sum(B28001_001E),
            deskop_laptop = sum(B28001_003E)/total_computers,
            smartphone = sum(B28001_005E)/total_computers,
            smartphone_only = sum(B28001_006E)/total_computers,
            tablet = sum(B28001_007E)/total_computers,
            no_computer = sum(B28001_011E)/total_computers)

#write to csv file
write.csv(internet_computer_summary, "internet_computer_2010.csv")


#population ages 25 and older


#get 2010 data on veterans
veterans <-get_acs(geography = "county",
                   year = 2010, table =c("B21001"),
                   state = "NY", county = c("Queens"),
                   survey = "acs5",
                   geometry = TRUE, output="wide")

#sum data by library service area
ntas_tracts_veterans_summary <- veterans %>%
  summarize(population = sum(B21001_001E),
            veterans = sum(B21001_002E)/population)

#write out to a csv for future use
write_csv(ntas_tracts_veterans_summary, "ntas_tracts_veteran_summary_2010.csv") #veteran_summary to veterans_summary


#get 2010 data on % of civilian noninstutitionalized population with disability
disability <-get_acs(geography = "county",
                     year = 2010, table =c("S1810"),
                     state = "NY", county = c("Queens"),
                     survey = "acs5",
                     geometry = TRUE, output="wide")

#merge nta_name data with disability data
ntas_tracts <- ntas_tracts %>% mutate(GEOID=as.character(GEOID))
ntas_tracts_disability<- ntas_tracts %>% left_join(disability, by="GEOID") 

#sum data by library service area
ntas_tracts_disability_summary <- ntas_tracts_disability %>%
  group_by(`nta_name`) %>%
  summarize(population = sum(S1810_C01_001E),                                   #S0102_C01_043?
            disability = sum(S1810_C02_001E)/population)

#write out to a csv
write_csv(ntas_tracts_disability_summary, "disability_2010.csv")

#sum data by disability type
ntas_tracts_disabilities <- ntas_tracts_disability %>%
  group_by(`nta_name`) %>%
  summarize(disability = sum(S1810_C02_001E),
            hearing = sum(S1810_C02_019E/disability),
            vision = sum(S1810_C02_029E/disability),
            cognitive = sum(S1810_C02_039E/disability),
            ambulatory = sum(S1810_C02_047E/disability),
            self_care = sum(S1810_C02_055E/disability),
            independent_living = sum(S1810_C02_063E/disability)) 

#write out to csv
write_csv(ntas_tracts_disabilities, "disability_types2_2010.csv")

#disability types in Queens
queens_disabilities <- disability %>% summarize(disability = sum(S1810_C02_001E),
                                                hearing = sum(S1810_C02_019E/disability),
                                                vision = sum(S1810_C02_029E/disability),
                                                cognitive = sum(S1810_C02_039E/disability),
                                                ambulatory = sum(S1810_C02_047E/disability),
                                                self_care = sum(S1810_C02_055E/disability),
                                                independent_living = sum(S1810_C02_063E/disability)) 
#write out to csv
write_csv(queens_disabilities, "queens_disabilities_2010.csv")

#get 2010 Queens tract-level data on worker occupations
occupations <- get_acs(geography = "county",
                       year = 2010, table = c("C24070"),
                       state = "NY", county = c("Queens"),
                       survey = "acs5",
                       geometry = TRUE, output = "wide")

#group data on worker occupations by library service area
occupations_summary <- occupations %>%
  summarize(total = sum(C24070_001E),
            agriculture_forestry_fishing_hunting_mining = sum(C24070_002E)/total,
            construction = sum(C24070_003E)/total,
            manufacturing = sum(C24070_004E)/total,
            wholesale_trade = sum(C24070_005E)/total,
            retail_trade = sum(C24070_006E)/total,
            transportation_warehousing_utilities = sum(C24070_007E)/total,
            information = sum(C24070_008E)/total,
            finance_insurance_real_estate_rental_leasing = sum(C24070_009E)/total,
            professional_scientific_management_adminstrative_waste_management_service = sum(C24070_010E)/total,
            educational_services_health_care_social_assistance = sum(C24070_011E)/total,
            arts_entertainment_recreation_accomodation_food_services = sum(C24070_012E)/total,
            other_service_except_public_administation = sum(C24070_013E)/total,
            public_administration = sum(C24070_014E)/total)

#write to csv file
write_csv(occupations_summary, "occupations_2010.csv")

#get 2010 Queens tract-level data on SNAP recipients
snap_recipients <- get_acs(geography = "county",
                           year = 2010, table = c("S2201"),
                           state = "NY", county = c("Queens"),
                           survey = "acs5",
                           geometry = TRUE, output = "wide")

#group data on SNAP recipients by library service area
snap_recipients_summary <- snap_recipients %>%
  summarize(total = sum(S2201_C01_001E),
            SNAP_recipients = sum(S2201_C02_001E)/total)

#write to csv file
write_csv(snap_recipients_summary, "snap_recipients_2010.csv")

#get Queens tract-level data of birth countries of foreign-born Queens residents
birth_countries <- get_acs(geography = "county",
                           year = 2010, table = c("B05006"),
                           state = "NY", county = c("Queens"),
                           survey = "acs5",
                           geometry = TRUE, output = "wide")

#group data on birth countries by library service area
birth_countries_summary <- birth_countries %>%
  summarize(total = sum(B05006_001E),
            ireland = sum(B05006_008E)/total,
            denmark = sum(B05006_009E)/total,
            norway = sum(B05006_010E)/total,
            sweden = sum(B05006_011E)/total,
            england = sum(B05006_006E)/total,
            scotland = sum(B05006_007E)/total,
            other_uk_countries = sum(B05006_005E)/total,
            other_north_european_countries = sum(B05006_012E)/total,
            austria = sum(B05006_014E)/total,
            belgium = sum(B05006_015E)/total,
            france = sum(B05006_016E)/total,
            germany = sum(B05006_017E)/total,
            netherlands = sum(B05006_018E)/total,
            switzerland = sum(B05006_019E)/total,
            other_west_european_countries = sum(B05006_020E)/total,
            greece = sum(B05006_022E)/total,
            italy = sum(B05006_023E)/total,
            portugal = sum(B05006_024E)/total,
            #azores_islands = sum(B05006_025E)/total,
            spain = sum(B05006_025E)/total,
            other_south_european_countries = sum(B05006_026E)/total,
            albania = sum(B05006_028E)/total,
            belarus = sum(B05006_029E)/total,
            bulgaria = sum(B05006_030E)/total,
            croatia = sum(B05006_031E)/total,
            czechoslovakia = sum(B05006_032E)/total,
            hungary = sum(B05006_033E)/total,
            latvia = sum(B05006_034E)/total,
            lithuania = sum(B05006_035E)/total,
            north_macedonia = sum(B05006_036E)/total,
            moldova = sum(B05006_037E)/total,
            poland = sum(B05006_038E)/total,
            romania = sum(B05006_039E)/total,
            russia = sum(B05006_040E)/total,
            ukraine = sum(B05006_041E)/total,
            bosnia_and_herzegovina = sum(B05006_042E)/total,
            serbia = sum(B05006_044E)/total,
            yugoslavia_including_serbia = sum(B05006_043E)/total,
            other_east_european_countries = sum(B05006_045E)/total,
            china = sum(B05006_050E)/total,
            hong_kong = sum(B05006_051E)/total,
            taiwan = sum(B05006_052E)/total,
            japan = sum(B05006_053E)/total,
            korea = sum(B05006_054E)/total,
            other_east_asian_countries = sum(B05006_055E)/total,
            afghanistan = sum(B05006_057E)/total,
            bangladesh = sum(B05006_058E)/total,
            india = sum(B05006_059E)/total,
            iran = sum(B05006_060E)/total,
            kazakhstan = sum(B05006_061E)/total,
            nepal = sum(B05006_062E)/total,
            pakistan = sum(B05006_063E)/total,
            sri_lanka = sum(B05006_064E)/total,
            uzbekistan = sum(B05006_065E)/total,
            other_south_central_asian_countries = sum(B05006_066E)/total,
            burma = sum(B05006_072E)/total,
            cambodia = sum(B05006_068E)/total,
            indonesia = sum(B05006_069E)/total,
            laos = sum(B05006_070E)/total,
            malaysia = sum(B05006_071E)/total,
            philippines = sum(B05006_073E)/total,
            singapore = sum(B05006_074E)/total,
            thailand = sum(B05006_075E)/total,
            vietnam = sum(B05006_076E)/total,
            other_south_east_asian_countries = sum(B05006_077E)/total,
            armenia = sum(B05006_088E)/total,
            iraq = sum(B05006_079E)/total,
            israel = sum(B05006_080E)/total,
            jordan = sum(B05006_081E)/total,
            kuwait = sum(B05006_082E)/total,
            lebanon = sum(B05006_083E)/total,
            saudi_arabia = sum(B05006_084E)/total,
            syria = sum(B05006_085E)/total,
            turkey = sum(B05006_087E)/total,
            yemen = sum(B05006_086E)/total,
            other_west_asian_countries = sum(B05006_089E)/total,
            eritrea = sum(B05006_093E)/total,
            ethiopia = sum(B05006_094E)/total,
            kenya = sum(B05006_095E)/total,
            other_east_african_countries = sum(B05006_096E)/total,
            cameroon = sum(B05006_098E)/total,
            other_middle_african_countries = sum(B05006_099E)/total,
            egypt = sum(B05006_101E)/total,
            morocco = sum(B05006_102E)/total,
            sudan = sum(B05006_103E)/total,
            other_north_african_countries = sum(B05006_104E)/total,
            south_african = sum(B05006_106E)/total,
            other_south_african_countries = sum(B05006_107E)/total,
            cabo_verde = sum(B05006_109E)/total,
            ghana = sum(B05006_110E)/total,
            liberia = sum(B05006_111E)/total,
            nigeria = sum(B05006_112E)/total,
            sierra_leone = sum(B05006_113E)/total,
            other_west_african_countries = sum(B05006_114E)/total,
            australia = sum(B05006_118E)/total,
            new_zealand_and_other_australasia_countries = sum(B05006_119E)/total,
            fiji = sum(B05006_120E)/total,
            bahamas = sum(B05006_125E)/total,
            barbados = sum(B05006_126E)/total,
            cuba = sum(B05006_127E)/total,
            dominica = sum(B05006_128E)/total,
            dominican_republic = sum(B05006_129E)/total,
            grenada = sum(B05006_130E)/total,
            haiti = sum(B05006_131E)/total,
            jamaica = sum(B05006_132E)/total,
            st_vincent_and_the_grenadines = sum(B05006_133E)/total,
            trinidad_and_tobago = sum(B05006_134E)/total,
            west_indies = sum(B05006_135E)/total,
            other_caribbean_countries = sum(B05006_136E)/total,
            belize = sum(B05006_139E)/total,
            costa_rica = sum(B05006_140E)/total,
            el_salvador = sum(B05006_141E)/total,
            guatemala = sum(B05006_142E)/total,
            honduras = sum(B05006_143E)/total,
            mexico = sum(B05006_138E)/total,
            nicaragua = sum(B05006_144E)/total,
            panama = sum(B05006_145E)/total,
            other_central_american_countries = sum(B05006_146E)/total,
            argentina = sum(B05006_148E)/total,
            bolivia = sum(B05006_149E)/total,
            brazil = sum(B05006_150E)/total,
            chile = sum(B05006_151E)/total,
            colombia = sum(B05006_152E)/total,
            equador = sum(B05006_153E)/total,
            guyana = sum(B05006_154E)/total,
            peru = sum(B05006_155E)/total,
            uruguay = sum(B05006_156E)/total,
            venezuela = sum(B05006_157E)/total,
            other_south_american_countries = sum(B05006_158E)/total,
            canada = sum(B05006_160E)/total,
            other_north_american_countries = sum(B05006_161E)/total)

#write to csv file
write_csv(birth_countries_summary, "birth_countries_summary_2010.csv")

#Load 2015 ACS tables
#v_15 <- load_variables(2015,"acs5",cache=T) %>% mutate(table=str_sub(name,1,6))
#v_15s <- load_variables(2015,"acs5/subject",cache=T) %>% mutate(table=str_sub(name,1,6))

#get 2010 Queens tract-level data on hh languages
hh_languages_2010 <- get_acs(geography = "county",
                             year = 2010, table = c("B16001"),
                             state = "NY", county = c("Queens"),
                             survey = "acs5",
                             geometry = TRUE, output = "wide")

#group data on hh languages by library service area
hh_languages_2010_summary <- hh_languages_2010 %>%
  summarize(total = sum(B16001_001E),
            spanish= sum(B16001_003E)/total,
            french= sum(B16001_006E)/total,
            french_creole= sum(B16001_009E)/total,
            italian= sum(B16001_012E)/total,
            portugese= sum(B16001_015E)/total,
            german = sum(B16001_018E)/total,
            yiddish = sum(B16001_021E)/total,
            scandinavian = sum(B16001_027E)/total,
            greek = sum(B16001_030E)/total,
            russian = sum(B16001_033E)/total,
            polish = sum(B16001_036E)/total,
            serbo_croatian = sum(B16001_039E)/total,
            armenian = sum(B16001_045E)/total,
            persian = sum(B16001_048E)/total,
            gujarati = sum(B16001_051E)/total,
            hindi = sum(B16001_054E)/total,
            urdu = sum(B16001_057E)/total,
            other_indic_languages = sum(B16001_060E)/total,
            other_indo_european_languages = sum(B16001_063E)/total,
            chinese = sum(B16001_066E)/total,
            japanese = sum(B16001_069E)/total,
            korean = sum(B16001_072E)/total,
            cambodian = sum(B16001_075E)/total,
            thai = sum(B16001_081E)/total,
            laotian = sum(B16001_084E)/total,
            vietnamese = sum(B16001_087E)/total,
            tagalog = sum(B16001_093E)/total,
            hungarian = sum(B16001_105E)/total,
            arabic = sum(B16001_108E)/total,
            hebrew = sum(B16001_111E)/total,
            african_languages = sum(B16001_114E)/total,
            other_west_germanic_languages = sum(B16001_024E)/total,
            other_slavic_languages = sum(B16001_042E)/total,
            other_asian_languages = sum(B16001_090E)/total,
            other_pacific_island_languages = sum(B16001_096E)/total,
            other_unspecified_languages = sum(B16001_117E)/total,
            hmong = sum(B16001_078E)/total)

#write to csv file
write_csv(hh_languages_2010_summary, "hh_languages_2010.csv")


#getting 2010 denominators
denom_pop <- get_acs(geography = "tract",
                     year = 2010, table = c("B01003"),
                     state = "NY", county = c("Queens"),
                     survey = "acs5",
                     geometry = TRUE, output = "wide")

denom_pop_5_and_over <- get_acs(geography = "tract",
                                year = 2010, table = c("B16004"),
                                state = "NY", county = c("Queens"),
                                survey = "acs5",
                                geometry = TRUE, output = "wide")

denom_pop_25_and_over <- get_acs(geography = "tract",
                                 year = 2010, table = c("B15002"),
                                 state = "NY", county = c("Queens"),
                                 survey = "acs5",
                                 geometry = TRUE, output = "wide")

#denom_pop_poverty <- get_acs(geography = "tract",
                             #year = 2010, table = c("B17020"),
                             #state = "NY", county = c("Queens"),
                             #survey = "acs5",
                             #geometry = TRUE, output = "wide")

denom_hh <- get_acs(geography = "tract",
                    year = 2010, table = c("B11012"),
                    state = "NY", county = c("Queens"),
                    survey = "acs5",
                    geometry = TRUE, output = "wide")

denom_family_hh <- get_acs(geography = "tract",
                           year = 2010, table = c("B11003"),
                           state = "NY", county = c("Queens"),
                           survey = "acs5",
                           geometry = TRUE, output = "wide")

denom_perc_of_pop_18_and_over <- get_acs(geography = "tract",
                                         year = 2010, table = c("B21001"),
                                         state = "NY", county = c("Queens"),
                                         survey = "acs5",
                                         geometry = TRUE, output = "wide")

#denom_total_civilian_noninstitutionalized_population <- get_acs(geography = "tract",
                                                                #year = 2010, table = c("S1810"),
                                                                #state = "NY", county = c("Queens"),
                                                                #survey = "acs5",
                                                                #geometry = TRUE, output = "wide")

#group by nta_name
ntas_denom <- ntas_tracts %>% 
  #left_join(denom_total_civilian_noninstitutionalized_population, by="GEOID") %>%
  left_join(denom_perc_of_pop_18_and_over, by="GEOID") %>%
  left_join(denom_family_hh, by="GEOID") %>%
  left_join(denom_hh, by="GEOID") %>%
  #left_join(denom_pop_poverty, by="GEOID") %>%
  left_join(denom_pop_25_and_over, by="GEOID") %>%
  left_join(denom_pop_5_and_over, by="GEOID") %>%
  left_join(denom_pop, by="GEOID")

#group by library service area
denom_summary <- ntas_denom %>%
  group_by(`nta_name`) %>%
  summarize(population = sum(B01003_001E),
            population_5_and_over = sum(B16004_001E),
            population_25_and_over = sum(B15002_001E),
            #population_poverty = sum(B17020_001E),
            households = sum(B11012_001E),
            family_households = sum(B11003_001E),
            perc_of_pop_18_and_over = sum(B21001_001E),
            #total_civ_noninstitutionalized_pop = sum(S1810_C01_001E)
            )

#write to csv file
write_csv(denom_summary, "denom_2010.csv")

