############################################
############# Libraries ###################
############################################
library(tidyverse)
library(survival)
library(survminer)
library(dplyr)
library(gtools)
library(scales)
library(metR)
library(viridis)

############################################
############# Parameters ###################
############################################
K = #TODO # Time horizon
proba_group0 = #TODO #Probability of belonging to group 0 at time zero
hazard_ratio_group0 = #TODO # Hazard ratio treatment versus control for group 0 at time point k
hazard_ratio_group1 = #TODO # Hazard ratio treatment versus control for group 1 at time point k

######################################################################################
############# Compute HR and RR for difference risks under control ###################
######################################################################################
#Test for different risks under control in the two subgroups
risk_to_test0 = data.frame(
  x0 = 1 - exp(log(seq(0.01,0.95,length.out =100))/K)
)
risk_to_test1 = data.frame(
  x1 =1 - exp(log(seq(0.01,0.95,length.out =100))/K)
)
risk_to_test = full_join(risk_to_test0, risk_to_test1, by = character())
results = NULL

for (i in 1:nrow(risk_to_test)){

  baseline_hazard_group0 = rep(risk_to_test[i,]$x0, K) # Hazard for group 0 : k-th parameter is hazard at time point (must be of size K)
  baseline_hazard_group1 = rep(risk_to_test[i,]$x1, K) # Hazard for group 1: k-th parameter is hazard at time point (must be of size K)
  
  df_hand_computation = data.frame(
    hazard_ratio_group0 = hazard_ratio_group0,
    hazard_ratio_group1 = hazard_ratio_group1,
    hazard_group0_control = baseline_hazard_group0,
    hazard_group1_control = baseline_hazard_group1) %>%
    mutate(hazard_group0_treatment = hazard_group0_control*hazard_ratio_group0) %>%
    mutate(hazard_group1_treatment = hazard_group1_control*hazard_ratio_group1) %>%
    mutate(survival_group0_treatment = cumprod(1-hazard_group0_treatment)) %>%
    mutate(survival_group1_treatment = cumprod(1-hazard_group1_treatment)) %>%
    mutate(survival_group0_control = cumprod(1-hazard_group0_control)) %>%
    mutate(survival_group1_control = cumprod(1-hazard_group1_control)) %>%
    mutate(survival_wp_treatment = survival_group0_treatment*proba_group0 + 
                                   survival_group1_treatment*(1-proba_group0)) %>%
    mutate(survival_wp_control = survival_group0_control*proba_group0 +
                                 survival_group1_control*(1-proba_group0)) %>%
    mutate(hazard_wp_control= 1 - survival_wp_control/lag(survival_wp_control, default = 1)) %>%  
    mutate(hazard_wp_treatment= 1 - survival_wp_treatment/lag(survival_wp_treatment, default = 1)) %>%
    mutate(hazard_ratio_wp = hazard_wp_treatment/hazard_wp_control) %>%
    mutate(prop_group0_control = lag(survival_group0_control, default = 1)*
             proba_group0/lag(survival_wp_control, default = 1)) %>%
    mutate(prop_group0_treatment = lag(survival_group0_treatment, default = 1)*
             proba_group0/lag(survival_wp_treatment, default = 1)) %>%
    mutate(risk_ratio_wp = (1-survival_wp_treatment)/(1-survival_wp_control))
  
  df_hand_computation_useful = df_hand_computation %>%
    tail(1) %>% #Keep last time point only
    mutate(risk_group0_control_percent = 100*(1-survival_group0_control), 
           risk_group1_control_percent = 100*(1-survival_group1_control)) %>%
    select(risk_group0_control_percent,risk_group1_control_percent,
           hazard_ratio_wp, risk_ratio_wp)
  
  results = results %>% bind_rows(df_hand_computation_useful)
  
}

#Compute relative differences between HR and RR
results = results %>%
  mutate(risk_group0_control_percent = as.numeric(risk_group0_control_percent),
         risk_group0_control_percent = as.numeric(risk_group0_control_percent),
         hazard_ratio_wp = as.numeric(hazard_ratio_wp)) %>%
  mutate(difference_between_hr_and_rr = abs(hazard_ratio_wp - risk_ratio_wp)/risk_ratio_wp)
