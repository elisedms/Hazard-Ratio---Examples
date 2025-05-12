############################################
############# Libraries ###################
############################################
library(tidyverse)

############################################
############# Parameters ###################
############################################
K = #TODO #Time horizon
proba_group0 = #TODO #Probability of belonging to group 1 (HR-positive) at time zero

baseline_hazard_group0 = #TODO # Hazard for group 0 : k-th parameter is hazard at time point (must be of size K)
baseline_hazard_group1 = #TODO # Hazard for group 1: k-th parameter is hazard at time point (must be of size K)
hazard_ratio_group0 = #TODO # Hazard ratio treatment versus control for group 0 at time point k
hazard_ratio_group1 = #TODO # Hazard ratio treatment versus control for group 1 at time point k

#######################################################################################
#######Compute all survival measures for the K first time points #######################
#######################################################################################

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
  mutate(hazard_ratio_wp = hazard_wp_treatment/hazard_wp_control)%>%
  mutate(prop_group0_control = lag(survival_group0_control, default = 1)*
           proba_group0/lag(survival_wp_control, default = 1)) %>%
  mutate(prop_group0_treatment = lag(survival_group0_treatment, default = 1)*
           proba_group0/lag(survival_wp_treatment, default = 1))

print(df_hand_computation)
