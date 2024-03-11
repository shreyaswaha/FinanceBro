library(dplyr)
library(tidyverse)
library(openintro)

census_data = openintro::census
head(census_data)
tail(census_data)
#Select
select(census_data, census_year, state_fips_code, total_family_income, age, sex, total_personal_income)
select(census_data, where(is.factor))
arrange(census_data, state_fips_code)
rename(census_data, state = state_fips_code)
census_data = mutate(census_data, total_income = total_family_income + total_personal_income)
