
require(tidyverse)
require(readxl)


# produce file for strategic review data

cols_needed <- c(1,57,65,73)


# 1. GENERATE LIVES SAVED DATA TARGET VS RESULTS ####

# 
# get the median mixture target for 2022


ls_targets <-
# hiv
full_join(    

  full_join(  
  read_excel("data/allModelledKPIs_3Mar2017.xlsx", sheet = "HIV KPI 1 e", skip = 4, range= "B5:BV105")  %>%
  select(cols_needed) %>% 
  rename(iso3=1,
         hiv_lives_saved_proj_2017_2022_l=2,
         hiv_lives_saved_proj_2017_2022_m=3,
         hiv_lives_saved_proj_2017_2022_u=4) 
  
  , 
    
# TB
  read_excel("data/allModelledKPIs_3Mar2017.xlsx", sheet = "TB KPI 1 e", skip = 4,range= "B5:BV120")  %>%
  select(cols_needed) %>% 
  rename(iso3=1,
         tb_lives_saved_proj_2017_2022_l=2,
         tb_lives_saved_proj_2017_2022_m=3,
         tb_lives_saved_proj_2017_2022_u=4) %>% 
  mutate(across(c(2:4), as.double))  # converts character to numeric for later rounding
  
),  

   # malaria
  read_excel("data/allModelledKPIs_3Mar2017.xlsx", sheet = "Malaria KPI 1 e", skip = 4,range= "B5:BV71")  %>%
    select(cols_needed) %>% 
    rename(iso3=1,
           malaria_lives_saved_proj_2017_2022_l=2,
           malaria_lives_saved_proj_2017_2022_m=3,
           malaria_lives_saved_proj_2017_2022_u=4)  %>% 
  mutate(across(c(2:4), as.double))
  
) 
  
# ls_targets %>% mutate(across(where(is.numeric),fns=format(., digits=8, nsmall=2)))

# ls_targets %>% mutate(across(where(is.numeric),fns=formatC(., format = "f", digitsls_targets %>% mutate(across(where(is.numeric),fns=formatC(., format = "f", digits=8, drop0trailing = FALSE)))

ls_targets %>% mutate(across(where(is.double),fns=format(round((.),3),nsmall=3)))

format(round(ls_targets$hiv_lives_saved_proj_2017_2022_l, 1), nsmall = 1)


write_csv(ls_targets %>%
            
            mutate(across(where(is.double),fns=format(round((.),3),nsmall=3)))
            
          ,"ls_targets_rounded.csv")




ls_actual <-

full_join(
full_join(
# HIV
read_excel("data/KPI1_livessaved_results_2023-01-25_vf.xlsx", sheet = "full_country_results") %>% 
  select(iso3,hiv_deaths_averted_goals_aim_CF_2015_KPI) %>%
  group_by(iso3) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm=TRUE))) %>% 
  rename(hiv_lives_saved_result_2017_2021= hiv_deaths_averted_goals_aim_CF_2015_KPI),


# TB
read_excel("data/KPI1_livessaved_results_2023-01-25_vf.xlsx", sheet = "full_country_results") %>% 
  select(iso3,tb_deaths_averted_hivneg_WHO_2000) %>%
  group_by(iso3) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm=TRUE))) %>% 
  rename(tb_lives_saved_result_2017_2021= tb_deaths_averted_hivneg_WHO_2000)),

# malaria
read_excel("data/KPI1_livessaved_results_2023-01-25_vf.xlsx", sheet = "full_country_results") %>% 
  select(iso3,mal_deaths_averted_CF_WHO_2000) %>%
  group_by(iso3) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm=TRUE))) %>% 
  rename(malaria_lives_saved_result_2017_2021= mal_deaths_averted_CF_WHO_2000)
) %>% 
  filter(iso3!="GlobalFund")


kpi_1a <- 
  full_join(ls_targets,ls_actual)


write.csv(kpi_1a,"kpi_1a.csv", row.names = FALSE)


# 2. GENERATE INCIDENCE REDUCTION DATA TARGET VS RESULTS ####


# use na= Nan to replace NaN with blank and so get a numeric read in directly from readxl
inc_rdn_targets <-
  # hiv
  full_join(    
    
    full_join(  
      read_excel("data/allModelledKPIs_3Mar2017.xlsx", sheet = "HIV KPI 1 c", skip = 4, range= "B5:BV105", na="NaN")  %>%
        select(cols_needed) %>% 
        rename(iso3=1,
               hiv_inc_rdn_proj_2022_per_l=2,
               hiv_inc_rdn_proj_2022_per_m=3,
               hiv_inc_rdn_proj_2022_per_u=4) 
      , 
      
      # TB
      read_excel("data/allModelledKPIs_3Mar2017.xlsx", sheet = "TB KPI 1 c", skip = 4,range= "B5:BV120", na="NaN")  %>%
        select(cols_needed) %>% 
        rename(iso3=1,
               tb_inc_rdn_proj_2022_per_l=2,
               tb_inc_rdn_proj_2022_per_m=3,
               tb_inc_rdn_proj_2022_per_u=4) 
      # %>% 
      # mutate(across(where(is.numeric),fns=as.double))
    ),  
    
    # malaria
    read_excel("data/allModelledKPIs_3Mar2017.xlsx", sheet = "Malaria KPI 1 c", skip = 4,range= "B5:BV71", na="NaN")  %>%
      select(cols_needed) %>% 
      rename(iso3=1,
             mal_inc_rdn_proj_2022_per_l=2,
             mal_inc_rdn_proj_2022_per_m=3,
             mal_inc_rdn_proj_2022_per_u=4) 
    # %>% 
      # mutate(across(where(is.numeric),fns=as.double))
  ) %>% 
  filter(iso3!="GlobalFund")

# # since the incidence reduction targets were framed with negative values as increases invert them
# also divide them by 100 to express them as values between 0-1
inc_rdn_targets <-
inc_rdn_targets %>% 
  mutate_at(vars(contains("inc_rdn_proj")), list(~. * -1)) %>% 
  mutate_at(vars(contains("inc_rdn_proj")), list(~. /100)) 


# read in the incidence targets ####

inc_rdn_results <-

  full_join(
    full_join(
      read_excel("data/KPI1 gap analysis 2023_02_01_ph_SPH.xlsx", sheet = "summary",skip = 2, col_names = TRUE) %>% 
        select(Iso3,'% change in HIV incidence rate from 2015-2021') %>%
        rename(hiv_inc_result_2015_2021_per= '% change in HIV incidence rate from 2015-2021'), 
      
      
      read_excel("data/KPI1 gap analysis 2023_02_01_ph_SPH.xlsx", sheet = "summary", skip = 2,col_names = TRUE) %>% 
        select(Iso3,'% change in TB incidence rate from 2015 - 2021') %>%
        rename(tb_inc_result_2015_2021_per= '% change in TB incidence rate from 2015 - 2021')
    ),
    
    
    read_excel("data/KPI1 gap analysis 2023_02_01_ph_SPH.xlsx", sheet = "summary", skip = 2,col_names = TRUE) %>% 
      select(Iso3,'% change in Malaria incidence rate from 2015-2021') %>%
      rename(malaria_inc_result_2015_2021_per= '% change in Malaria incidence rate from 2015-2021')) %>% 
  rename(iso3=Iso3)%>% 
  filter(iso3!="GlobalFund")


kpi_1b <- 
  full_join(inc_rdn_targets,inc_rdn_results)



kpi_1 <- full_join(kpi_1a,kpi_1b) %>% 
  # replace NA substites zeros for both NA and NaN so this deals with the NaNs
  mutate(across(everything(), ~replace_na(.x, NA)))



wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb,"KPI1a_analysis")
openxlsx::writeDataTable(wb,"KPI1a_analysis", 
                         kpi_1 %>% 
                           select(iso3,contains("lives_saved")) %>% 
                           filter(!is.na(hiv_lives_saved_proj_2017_2022_l)
                                  |!is.na(hiv_lives_saved_proj_2017_2022_m)
                                  |!is.na(hiv_lives_saved_proj_2017_2022_u)
                                  | !is.na(tb_lives_saved_proj_2017_2022_l)
                                  | !is.na(tb_lives_saved_proj_2017_2022_m)
                                  | !is.na(tb_lives_saved_proj_2017_2022_u)
                                  | !is.na(malaria_lives_saved_proj_2017_2022_l)
                                  | !is.na(malaria_lives_saved_proj_2017_2022_m)
                                  | !is.na(malaria_lives_saved_proj_2017_2022_u)
                                  | !is.na(hiv_lives_saved_result_2017_2021) 
                                  | !is.na(tb_lives_saved_result_2017_2021) 
                                  | !is.na(malaria_lives_saved_result_2017_2021))
                         , startCol = 1, startRow = 1)

openxlsx::addWorksheet(wb,"KPI1b_analysis")
openxlsx::writeDataTable(wb,"KPI1b_analysis", kpi_1 %>%  
                           select(iso3,contains("inc")) %>% 
                         filter(!is.na(hiv_inc_rdn_proj_2022_per_l)
                                |!is.na(hiv_inc_rdn_proj_2022_per_m)
                                |!is.na(hiv_inc_rdn_proj_2022_per_u)
                                |!is.na(tb_inc_rdn_proj_2022_per_l) 
                                |!is.na(tb_inc_rdn_proj_2022_per_m)
                                |!is.na(tb_inc_rdn_proj_2022_per_u) 
                           | !is.na(mal_inc_rdn_proj_2022_per_l)
                           | !is.na(mal_inc_rdn_proj_2022_per_m)
                           | !is.na(mal_inc_rdn_proj_2022_per_u)
                           | !is.na(hiv_inc_result_2015_2021_per) 
                           | !is.na(tb_inc_result_2015_2021_per)
                           | !is.na(malaria_inc_result_2015_2021_per))
                         , startCol = 1, startRow = 1)

openxlsx::saveWorkbook(wb,paste0("KPI1_SR_data_",Sys.Date(),".xlsx"), overwrite = TRUE)





