
require(tidyverse)

# produce file for strategic review 


# 1. GENERATE LIVES SAVED DATA TARGET VS RESULTS ####

# 
# get the median mixture target for 2022


ls_targets <-
# hiv
full_join(    

  full_join(  
  read_excel("data/allModelledKPIs_3Mar2017.xlsx", sheet = "HIV KPI 1 e", skip = 4, range= "B5:BV105")  %>%
  select(1,65) %>% 
  rename(iso3=1,hiv_lives_saved_tgt_2017_2022=2)
  , 
    
  
# TB
  read_excel("data/allModelledKPIs_3Mar2017.xlsx", sheet = "TB KPI 1 e", skip = 4,range= "B5:BV120")  %>%
  select(1,65) %>% 
  rename(iso3=1,tb_lives_saved_tgt_2017_2022=2) %>% 
  mutate(tb_lives_saved_tgt_2017_2022=as.double(tb_lives_saved_tgt_2017_2022))
),  

   # malaria
  read_excel("data/allModelledKPIs_3Mar2017.xlsx", sheet = "Malaria KPI 1 e", skip = 4,range= "B5:BV71")  %>%
    select(1,65) %>% 
    rename(iso3=1,malaria_lives_saved_tgt_2017_2022=2) %>% 
    mutate(malaria_lives_saved_tgt_2017_2022=as.double(malaria_lives_saved_tgt_2017_2022))
)
  








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

inc_rdn_targets <-
  # hiv
  full_join(    
    
    full_join(  
      read_excel("data/allModelledKPIs_3Mar2017.xlsx", sheet = "HIV KPI 1 c", skip = 4, range= "B5:BV105")  %>%
        select(1,65) %>% 
        rename(iso3=1,hiv_inc_rdn_tgt_2022_per=2) %>% 
        mutate(hiv_inc_rdn_tgt_2022_per=as.double(hiv_inc_rdn_tgt_2022_per))
      , 
      
      # TB
      read_excel("data/allModelledKPIs_3Mar2017.xlsx", sheet = "TB KPI 1 c", skip = 4,range= "B5:BV120")  %>%
        select(1,65) %>% 
        rename(iso3=1,tb_inc_rdn_tgt_2022_per=2) %>% 
        mutate(tb_inc_rdn_tgt_2022_per=as.double(tb_inc_rdn_tgt_2022_per))
    ),  
    
    # malaria
    read_excel("data/allModelledKPIs_3Mar2017.xlsx", sheet = "Malaria KPI 1 c", skip = 4,range= "B5:BV71")  %>%
      select(1,65) %>% 
      rename(iso3=1,malaria_inc_rdn_tgt_2022_per=2) %>% 
      mutate(malaria_inc_rdn_tgt_2022_per=as.double(malaria_inc_rdn_tgt_2022_per))
  ) %>% 
  filter(iso3!="GlobalFund")


inc_rdn_actual <-

  full_join(
    full_join(
      # HIV
      read_excel("data/KPI1 gap analysis 2023_02_01_ph_SPH.xlsx", sheet = "summary",skip = 2, col_names = TRUE) %>% 
        select(Iso3,'% change in HIV incidence rate from 2015-2021') %>%
        rename(hiv_inc_result_2015_2021_per= '% change in HIV incidence rate from 2015-2021'),
      
      
      # TB
      read_excel("data/KPI1 gap analysis 2023_02_01_ph_SPH.xlsx", sheet = "summary", skip = 2,col_names = TRUE) %>% 
         select(Iso3,'% change in TB incidence rate from 2015 - 2021') %>%
         rename(tb_inc_result_2015_2021_per= '% change in TB incidence rate from 2015 - 2021')
      ),
    
    # malaria
    read_excel("data/KPI1 gap analysis 2023_02_01_ph_SPH.xlsx", sheet = "summary", skip = 2,col_names = TRUE) %>% 
      select(Iso3,'% change in Malaria incidence rate from 2015-2021') %>%
      rename(malaria_inc_result_2015_2021_per= '% change in Malaria incidence rate from 2015-2021')
  ) %>% 
  rename(iso3=Iso3)%>% 
  filter(iso3!="GlobalFund")


kpi_1b <- 
  full_join(inc_rdn_targets,inc_rdn_actual)



kpi_1 <- full_join(kpi_1a,kpi_1b) %>% 
  # replace NA substites zeros for both NA and NaN so this deals with the NaNs
  mutate(across(everything(), ~replace_na(.x, NA)))

kpi_1 <- 
  # filter out cases where all the following are missing | functions as AND
kpi_1 %>% 
               


# since the incidence reducton targets were framed with negative values as increases invert them
kpi_1 <-
  kpi_1 %>% 
  mutate_at(vars(contains("inc_rdn_tgt")), list(~. * -1))


# write.csv(kpi_1,"kpi_1.csv", row.names = FALSE)


wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb,"KPI1a_analysis")
openxlsx::writeDataTable(wb,"KPI1a_analysis", 
                         kpi_1 %>% 
                           select(iso3,contains("lives_saved")) %>% 
                           filter(!is.na(hiv_lives_saved_tgt_2017_2022) 
                                  | !is.na(tb_lives_saved_tgt_2017_2022)
                                  | !is.na(malaria_lives_saved_tgt_2017_2022) 
                                  | !is.na(hiv_lives_saved_result_2017_2021) 
                                  | !is.na(tb_lives_saved_result_2017_2021) 
                                  | !is.na(malaria_lives_saved_result_2017_2021))
                         , startCol = 1, startRow = 1)

openxlsx::addWorksheet(wb,"KPI1b_analysis")
openxlsx::writeDataTable(wb,"KPI1b_analysis", kpi_1 %>%  
                           select(iso3,contains("inc")) %>% 
                         filter(!is.na(hiv_inc_rdn_tgt_2022_per)
                                |!is.na(tb_inc_rdn_tgt_2022_per) 
                           | !is.na(malaria_inc_rdn_tgt_2022_per) 
                           | !is.na(hiv_inc_result_2015_2021_per) 
                           | !is.na(tb_inc_result_2015_2021_per)
                           | !is.na(malaria_inc_result_2015_2021_per))
                         , startCol = 1, startRow = 1)

openxlsx::saveWorkbook(wb,paste0("KPI1_SR_data",Sys.Date(),".xlsx"), overwrite = TRUE)





