library(pacman)
p_load( tidyverse ,
        dplyr ,
        broom ,
        haven ,
        reshape2 ,
        tictoc ,
        openxlsx )

setwd("D:/Economics/Projects/macro-development")

church_census_1980_2010 = read_dta("Longitudinal Religious Congregations and Membership File, 1980-2010 (County Level).DTA") %>%
  mutate( adherent_rate = adherent/totpop) %>%
  select( - congreg ) %>%
  rename( countyfips = fipsmerg ,
          countyname = cntynm ,
          state = stateab ) %>%
  mutate_at( .vars = c("countyfips" ,
                       "year" ,
                       "totpop" , 
                       "grpcode" , 
                       "adherent" , 
                       "reltrad" , 
                       "family" ,
                       "adherent_rate") ,
             .funs = as.numeric
  ) %>%
  mutate_at( .vars = c("countyname" ,
                       "state" , 
                       "grpname" ) ,
             .funs = as.character
  ) %>%
  select( -c("NOTE_MIS" , 
             "NOTE_COM" , 
             "NOTE_MEA" ))

church_census_1980_2010 = church_census_1980_2010 %>%
  filter( adherent != is.na(1) )

state_fips = readRDS("D:/Economics/Data/CPS/state_fips.rds") %>%
  rename( statefips = state ,
          state = abb )

county_1980_2010 = state_fips %>%
  merge( church_census_1980_2010 ) %>%
  select( -c(name,
             state) 
          ) %>%
  rename( state = statefips )

county_1980_2010$reltrad = factor( x = county_1980_2010$reltrad , 
                                   levels = c( 1 , 
                                               2 , 
                                               3 ,
                                               4 , 
                                               5 , 
                                               6 ) ,
                                   labels = c("Evangelical Protestant" ,
                                              "Mainline Protestant"	,
                                              "Catholic" ,
                                              "Other" ,
                                              "Orthodox" ,
                                              "Black Protestant"
                                              ) 
                                   )

county_pc_df = county_1980_2010 %>%
  filter( reltrad == "Mainline Protestant" |
          reltrad == "Catholic" ) %>%
  group_by( year ,
            state , 
            countyfips ,
            countyname ,
            reltrad) %>%
  summarise( adherent = sum(adherent) ,
             adherent_rate = sum(adherent_rate)) 
county_pc_df$catholic[is.na(county_pc_df$catholic)] = 0

county_pc_adh_r = county_1980_2010 %>%
  filter( reltrad == "Mainline Protestant" |
            reltrad == "Catholic" ) %>%
  group_by( year ,
            state , 
            countyfips ,
            countyname ,
            reltrad) %>%
  summarise( adherent_rate = sum(adherent_rate) )  %>%
  dcast( formula = year + state + countyfips + countyname ~ reltrad , value.var = "adherent_rate" ) %>%
  rename( catholic_rate = Catholic , 
          protestant_rate = "Mainline Protestant" )
county_pc_adh_r$catholic[is.na(county_pc_adh_r$catholic)] = 0

county_pc_df = county_pc_adh %>% merge(county_pc_adh_r)

saveRDS( county_pc_df ,
         "county_pc_df.rds")

#county_1971 = readRDS("church_census_1971.rds") %>%
#  separate( variable , 
#            into = c("church" , 
#                     "type" , 
#                     "overflow") , 
#            sep ="_") %>%
#  filter( overflow == "A" |
#          type == "A"
#          ) %>%
#  select( -c(type, 
#             overflow , 
#             count , 
#             totmem , 
#             church ) ) %>%
#  rename( adherent = totadh ,
#          grpname = religid )

## 1980 

rm(list=ls())

library(ipumsr)

ddi = read_ipums_ddi("D:/Economics/Data/Census/usa_00002.xml")

qcew = readRDS("qcew_decennial_1980_2010.rds") %>%
  rename( fips = countyfips )

us_county_census = bind_rows( us_census_1980 , 
                       us_census_1990 ,
                       us_census_2000 ,
                       us_census_2010 ) %>%
  rename( year = YEAR , 
          state = STATEFIP ) %>%
  mutate( black_rate = black_rate*100 )

county_pc_df = readRDS("D:/Economics/Projects/macro-development/county_pc_df.rds")

county_pc_census_merged = merge( county_pc_df , us_county_census ) %>%
  mutate( protestant_rate = (protestant / population)*100 ,
          catholic_rate = (catholic / population)*100
          )

wd_home = getwd()

setwd("D:/Economics/Data/QCEW/sic.1980.annual.by_area/")
qcew_xlsx = list.files( pattern = ".csv" )
qcew_1980_bind = function(i){
  qcew_i = read.csv( i , 
                     header = TRUE) %>%
    filter( own_code == 0 ) %>%
    select( year , 
            area_fips , 
            annual_avg_estabs_count , 
            annual_avg_emplvl , 
            annual_avg_wkly_wage ) %>%
    mutate_all( as.numeric ) %>%
    mutate( stateind = area_fips / 1000 ,
            unknownind = (area_fips - 999)/1000 ,
            state_ind = ifelse( stateind == round(stateind) , 
                                1  ,
                                0) , 
            unk_ind = ifelse( unknownind == round(unknownind) ,
                              1 , 
                              0)
            ) %>%
    filter( state_ind == 0 &
              unk_ind == 0 ,
            annual_avg_emplvl > 0 ) %>%
    select( - c(state_ind , unk_ind , stateind , unknownind) )
    
}
qcew_1980 = map_dfr( qcew_xlsx , 
                     qcew_1980_bind) %>%
  rename( countyfips = area_fips , 
          establishments = annual_avg_estabs_count , 
          employment = annual_avg_emplvl , 
          wage_weekly = annual_avg_wkly_wage )

setwd("D:/Economics/Data/QCEW/")
qcew_1990 = read.xlsx("allhlcn90.xlsx") %>%
  filter(Area.Type=="County" &
          Own == 0 ) %>%
  select( Area.Code ,
          Year , 
          Annual.Average.Establishment.Count , 
          Annual.Average.Employment ,
          Annual.Average.Weekly.Wage ) %>%
  rename( countyfips = Area.Code , 
          year = Year , 
          establishments = Annual.Average.Establishment.Count , 
          employment = Annual.Average.Employment ,
          wage_weekly = Annual.Average.Weekly.Wage)

qcew_2000 = readRDS("D:/Economics/Data/QCEW/qcew_99_19.rds") %>% 
  filter( Year == 2000 &
          Area.Type=="County" &
          Own == 0 &
          month == 7 ) %>%
  select( Area.Code ,
          Year , 
          Establishment.Count , 
          employment ,
          Average.Weekly.Wage ) %>%
  rename( countyfips = Area.Code , 
          year = Year , 
          establishments = Establishment.Count , 
          wage_weekly = Average.Weekly.Wage)

qcew_2010 = readRDS("D:/Economics/Data/QCEW/qcew_99_19.rds") %>% 
  filter( Year == 2010 &
          Area.Type=="County" &
          Own == 0 &
          month == 7) %>%
  select( Area.Code ,
          Year , 
          Establishment.Count , 
          employment ,
          Average.Weekly.Wage ) %>%
  rename( countyfips = Area.Code , 
          year = Year , 
          establishments = Establishment.Count , 
          wage_weekly = Average.Weekly.Wage)

setwd(wd_home)

qcew = rbind( qcew_1980,
              qcew_1990 ,
              qcew_2000 ,
              qcew_2010 )

saveRDS( qcew , "qcew_decennial_1980_2010.rds")

# load-up religion data 
religion_rates_2010_df = read_dta( "C:/Users/No One/Downloads/U.S. Religion Census Religious Congregations and Membership Study, 2010 (County File).DTA") %>%
  select( fips ,
          stcode ,
          POP2010 ,
          mprtadh ,
          mprtrate ,
          cathadh , 
          cathrate ) %>%
  mutate_all( .funs = as.numeric ) %>%
  rename( state = stcode ,
          population = POP2010 ,
          protestant = mprtadh ,
          protestant_rate = mprtrate ,
          catholic = cathadh , 
          catholic_rate = cathrate ) 
religion_rates_2010_df$catholic[is.na(religion_rates_2010_df$catholic)] = 0 

religion_rates_2010_df = religion_rates_2010_df%>%
  mutate( prot_rate = (protestant / population)*100 ,
          cath_rate = (catholic / population )*100 ,
          logpop = log(population ))


# load-up education data

countyeduc_2000 = read.xlsx( xlsxFile = "educ-1970-2000.xlsx") %>%
  melt( id=c("fips",
             "countyname") ,
        value.name = "educ" ,
        variable.name = "year" ) %>%
  mutate( fips = as.numeric(fips) ,
          year = as.numeric(year) ,
          year = 1960 + 10*year )


countyeduc_2010 = read.xlsx( xlsxFile = "educ-2010-small.xlsx") %>%
  mutate( year = 2010 ) %>%
  merge( countyeduc_2000 %>%
           filter( year == 2000 ) %>%
           select( fips , 
                   countyname) )

countyeduc = countyeduc_2000 %>% rbind(countyeduc_2010)

# load up population data
  
  
county_pop_1990_2000 = read.xlsx("census-county-population-1990-2000.xlsx") %>%
  select(-state)%>%
  mutate( fips = as.numeric(fips)) %>%
  melt( id=c("fips", "countyname") ,
        value.name = "population" ,
        variable.name = "year" )

county_pop_1980 = read.xlsx("census-county-population-1980-UNMERGED.xlsx") %>%
  group_by( year , 
            fips ) %>%
  summarise(population = sum(population) ) %>%
  merge( county_pop_1990_2000 %>%
           filter( year == 1990) %>%
           select( -c(population,
                      year)  )
  )

county_pop_2010 = religion_rates_2010_df %>%
  select( fips ,
          population
          ) %>%
  mutate( year = 2010 
          ) %>%
  merge( county_pop_1990_2000 %>%
           filter( year == 1990
                   ) %>%
           select( -c(population,
                      year)  
                   ) 
         )

# hispanic population

esp_pop_1980 = read.xlsx("hispanic_1980.xlsx") %>%
  mutate( year = 1980 )

esp_pop_90 = read_dta("C:/Users/No One/Downloads/stch1990.dta")  %>%
  filter( ethnic == 2 ) %>%
  mutate( st = state ,
          yr = year) %>%
  group_by( yr , 
            st , 
            county ) %>%
  summarise( hispanic = sum(pop) ) %>%
  mutate( fips = as.numeric(county) , 
          state = as.numeric(st) ,
          year = as.numeric(yr) )

esp_pop_1990 = cbind( year = esp_pop_90$year , 
                    state = esp_pop_90$state , 
                    fips = esp_pop_90$fips ,
                    hispanic = esp_pop_90$hispanic) %>%
  data.frame()

rm(esp_pop_90)

esp_pop_1980 = esp_pop_1990 %>%
  select( state , 
          fips ) %>%
  merge( esp_pop_1980 )
  

esp_pop_2000_2010 = read_dta("C:/Users/No One/Downloads/popcounty5yagesrh2000_2010.dta") %>%
  filter( origin == 2 &
            sex == 0 &
            race == 0 ) %>%
  select(-c(origin,
            sex,
            race) ) %>%
  group_by( state, 
            county ) %>%
  summarise( population_2000 = mean(estimatesbase2000) , 
             population_2010 = mean(census2010pop) 
             ) %>%
  melt( id = c("state" , 
               "county" ) ,
        value.name = "hispanic" ,
        variable.name = "year" ) %>%
  mutate_at( .vars = c("county" , "state") ,
             .funs = as.numeric ) %>%
  rename(fips = county )

esp_pop_2000_2010$year = esp_pop_2000_2010$year %>%
  recode( 'population_2000' = 2000 , 
          'population_2010' = 2010 )
  
esp_pop = esp_pop_1980 %>%
  bind_rows( esp_pop_1990 , 
             esp_pop_2000_2010 )

# overall county stats

county_pop_df = county_pop_1980 %>%
  rbind( county_pop_1990_2000 ,
         county_pop_2010 )

census_county_df = countyeduc  %>%
  filter( year > 1970 ) %>%
  merge( county_pop_df )

census_county_and_hispanic = census_county_df %>%
  merge( esp_pop ) %>%
  mutate( hispanic_rate = 100*(hispanic/population))
saveRDS(census_county_and_hispanic , "county_pop_with_hispanic.rds")
  
relig_educ_county_2010 = religion_rates_2010_df %>%
  merge( countyeduc_2010 ) 
relig_educ_county_2010$catholic_rate[is.na(relig_educ_county_2010$catholic_rate)] = 0

county_pc_df = readRDS("county_pc_df.rds") %>%
  rename( fips = countyfips ) %>%
  merge( county_pop_df )  %>%
  mutate( prot_rate = 100*(protestant/population) , 
          cath_rate = 100*(catholic/population) ,
          )

county_pc_df_2010_comparison = county_pc_df %>% 
  filter( year == 2010 ) %>%
  rename( comp_population = population , 
          comp_protestant = protestant ,
          comp_catholic = catholic 
          ) %>%
  mutate( imp_prot = 0.59*comp_protestant ,
          imp_prot_rate = 100*(imp_prot/comp_population)
          ) %>%
  select( -c(prot_rate , cath_rate) ) %>%
  merge(relig_educ_county_2010 ) 

saveRDS(county_pc_df_2010_comparison , "protestant_measure_comparison.rds")

# use a regression of 2010 "correct" protestant measurement to obtain a scaling factor

conv = lm( data = county_pc_df_2010_comparison ,
    formula = protestant ~ comp_protestant )$coef[2]

county_religion_census = county_pc_df %>%
  mutate( protestant_imp = conv*protestant , 
          prot_imp_rate = 100*(protestant_imp/population)) %>%
  merge( census_county_and_hispanic )

qcew = readRDS("D:/Economics/Projects/dev-macro/qcew_decennial_1980_2010.rds") %>%
  mutate( fips = as.numeric(countyfips) )

census_qcew_df = merge( county_religion_census , qcew )

# capital income variable

county_capital_inc = read.xlsx("county_capital_inc.xlsx") %>%
  select(-c(code_desc , 
            unit)) %>%
  melt( id=c("fips" ,
             "countyname") ,
        value.name = "capital_inc" ,
        variable.name = "year") %>%
  mutate_at( .vars = c( "year" , "capital_inc" , "fips") ,
             .funs = as.numeric) %>%
  filter( capital_inc != is.na(1) ) %>%
  mutate( year = 1970 + 10*year ,
          capital = capital_inc*1000 )

# final data set

data_df = merge( census_qcew_df , county_capital_inc %>% select(-countyname)) %>%
  mutate( k_per_capita = capital/population )

saveRDS( data_df , "protestant_america_project_data.rds")

