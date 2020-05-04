require(dplyr)
require(lubridate)
require(data.table)
require(ggplot2)
library(plm)
library(stargazer)


data <- read.csv("../covid-19-germany-gae-master 4/cases-rki-by-state.csv") %>% 
        dplyr::rename(time=time_iso8601)

data_long <- data %>% reshape2::melt( id=c("time")) %>% 
                     dplyr::rename(cases = value, state = variable) %>% 
                     dplyr::filter(state!="sum_cases") %>% 
                     dplyr::mutate(state = stringr::str_replace(state, "DE.", ""),
                                   time = lubridate::ymd_hms(time))

data_long_enh <- data_long %>% dplyr::group_by(state) %>% dplyr::do({
  data <- data.frame(.)

  if(unique(data$state == "BY")){
    data <- data %>% dplyr::mutate(phase = ifelse(time < lubridate::ymd_hms("2020-03-21 00:00:00"), "baseline", "treatment")) %>% 
      dplyr::mutate(days_since_lockdown = ifelse(phase=="baseline", 0, difftime(time, lubridate::ymd_hms("2020-03-21 00:00:00"), unit="days")))
  }else{
    data <- data %>% dplyr::mutate(phase = ifelse(time < lubridate::ymd_hms("2020-03-23 00:00:00"), "baseline", "treatment")) %>% 
      dplyr::mutate(days_since_lockdown = ifelse(phase=="baseline", 0, difftime(time, lubridate::ymd_hms("2020-03-23 00:00:00"), unit="days")))
  }
  
  data$lag_cases <- dplyr::lead(data$cases)
  data <- data %>% dplyr::mutate(lag_cases=ifelse(is.na(lag_cases), NA, lag_cases))
  data %>% dplyr::mutate(day_increase = lag_cases-cases) 
}) 


data_long_enh <- data_long_enh %>% dplyr::mutate(policy = case_when(state %in% c("BY", "SN", "SL") & phase == "treatment" ~ 1,
                                                                   TRUE ~ 0),
                                                 by = ifelse(state=="BY", 1, 0),
                                                 ski = case_when(state %in% c("BY", "HH", "BW") ~ 1,
                                                                 TRUE ~ 0),
                                                 reference = case_when(state %in% c("BY", "SN", "SL")  ~ 1,
                                                                                TRUE ~ 0)) %>% dplyr::mutate(istreatment = ifelse(phase=="baseline", 0, 1)) %>% 
                                   dplyr::mutate(week = lubridate::week(time)) 


data_long_enh$w99 <- statar::winsorise(data_long_enh$day_increase, probs = c(0.005, 0.995), verbose = TRUE)
data_long_enh$w975 <- statar::winsorise(data_long_enh$day_increase, probs = c(0.0125, 0.9875), verbose = TRUE)

df_plm <- plm::pdata.frame(data.frame(data_long_enh %>% dplyr::filter(!is.na(lag_cases)) ), c('state', 'time'))


#data_long_enh %>% dplyr::group_by(state) %>% dplyr::summarise(min = min(time), max = max(time)) %>% View()


reg <- plm::plm(day_increase ~ phase  + phase:policy,
                data=df_plm , effect = "twoways", model = "within")

lmtest::coeftest(reg, vcov.=function(x) vcovHC(x, method="arellano", type="sss", cluster="group"))

