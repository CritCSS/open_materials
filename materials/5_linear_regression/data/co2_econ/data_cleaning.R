library(readr)
library(tidyverse)
library(openxlsx)
library(tidymodels)
library(GGally)

#CO2 emissions 
co2_emissions_tonnes_per_person <- read_csv("materials/5_linear_regression_I/data/co2_econ/co2_emissions_tonnes_per_person.csv") %>% 
  #select(country, starts_with("20")) %>% 
  select(country, "2017") %>% 
  rename("co2_emissions" = "2017")

#income
income_ppp <- read.xlsx("materials/5_linear_regression_I/data/co2_econ/income_per_person_gdppercapita_ppp_inflation_adjusted.xlsx")%>%
  #select(country, starts_with("20")) %>% 
  select(country, "2017") %>% 
  rename("income_ppp" = "2017")

#industry as % of gdp
industry <- read.xlsx("materials/5_linear_regression_I/data/co2_econ/industry_percent_of_gdp.xlsx")%>%
  #select(country, starts_with("20")) %>% 
  select(country, "2017") %>% 
  rename("industry" = "2017")

#life_expectancy (years)
life_expectancy <- read.xlsx("materials/5_linear_regression_I/data/co2_econ/life_expectancy_years.xlsx")%>%
  #select(country, starts_with("20")) %>% 
  select(country, "2017") %>% 
  rename("life_expectancy" = "2017")


#agriculture as % of gdp
agriculture <- read.xlsx("materials/5_linear_regression_I/data/co2_econ/agriculture_percent_of_gdp.xlsx")%>%
  #select(country, starts_with("20")) %>% 
  select(country, "2017") %>% 
  rename("agriculture" = "2017")


#population perceiving climate change as a serious threat 
cc_vser <- read.xlsx("materials/5_linear_regression_I/data/co2_econ/cc_vser_t_per.xlsx") %>% 
  select(country, "2018") %>% 
  rename("cl_change_vser" = "2018")

#democracy index
demox_eiu <- read.xlsx("materials/5_linear_regression_I/data/co2_econ/demox_eiu.xlsx") %>% 
  select(country, "2017") %>% 
  rename("democracy" = "2017")


#poverty
poverty <- read.xlsx("materials/5_linear_regression_I/data/co2_econ/poverty_percent_people_below_550_a_day.xlsx")%>%
  #select(country, starts_with("20"))
  select(country, "2017") %>% 
  rename("poverty" = "2017")

#ratio girls/boys in schools
girls <- read.xlsx("materials/5_linear_regression_I/data/co2_econ/ratio_of_girls_to_boys_in_primary_and_secondary_education_perc.xlsx")%>%
  #select(country, starts_with("20"))
  select(country, "2017") %>% 
  rename("girls_boys" = "2017")


#percentage of women in parliament
wn_bothhouses <- read.xlsx("materials/5_linear_regression_I/data/co2_econ/wn_bothhouses_c.xlsx")%>%
  #select(country, starts_with("20"))
  select(country, "2017") %>% 
  rename("women_parliament" = "2017")



###########join#######

base <- co2_emissions_tonnes_per_person %>% 
  left_join(.,income_ppp,by = "country") %>% 
  left_join(.,agriculture,by = "country") %>% 
  left_join(.,cc_vser,by = "country") %>% 
  left_join(.,demox_eiu,by = "country") %>% 
  #left_join(.,poverty,by = "country") %>% 
  #left_join(.,girls,by = "country") %>% 
  left_join(.,wn_bothhouses,by = "country") %>% 
  left_join(.,industry,by = "country") %>% 
  left_join(.,life_expectancy,by = "country") 


colSums(is.na(base))

base_v2 <- na.omit(base) %>% 
  mutate(k_bin = case_when(grepl("k", income_ppp, fixed = TRUE) ~ 1,
                           TRUE ~ 0)) %>% 
  mutate(income_ppp = case_when(k_bin == 1 ~ as.numeric(paste0(substr(income_ppp, 1, nchar(income_ppp)-1)))*1000,
                                TRUE ~ as.numeric(paste0(income_ppp)))) %>% 
  select(-k_bin)
  

ggpairs(base_v2 %>% select(-country))

#####prueba####

lm_spec <- linear_reg() %>%
  #set_mode("regression") %>%
  set_engine("lm")

lm_fit <- lm_spec %>%
  fit_xy(
    x = base_v2 %>% select(income_ppp),
    y = base_v2 %>% pull(co2_emissions)
  )

lm_fit %>% 
  pluck("fit") %>%
  summary()



lm_fit <- lm_spec %>%
  fit_xy(
    x = base_v2 %>% select(income_ppp,life_expectancy,cl_change_vser,democracy,agriculture,industry),
    y = base_v2 %>% pull(co2_emissions)
  )

lm_fit %>% 
  pluck("fit") %>%
  summary()


saveRDS(base_v2, "materials/5_linear_regression_I/data/co2_econ/co2_income.RDS")


