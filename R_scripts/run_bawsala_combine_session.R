# Run cleavage-level model on combined 1st and 2nd Tunisian parliamentary sessions
# Code for When National Unity Governments are neither National, United, nor Governments: The Case of Tunisia
# by Robert Kubinec 

require(idealstan)
require(bayesplot)
require(dplyr)
require(tidyr)
require(ggplot2)
require(lubridate)
require(stringr)
require(forcats)
require(xtable)
require(textclean)

# load data

all_data <- readRDS('data/combine_sessions.rds')

# run 2-group stationary model

arp_ideal_data <- id_make(score_data = all_data,
                          outcome="clean_votes",
                          person_id="legis_names",
                          item_id="law_unique",
                          time_id="law_date",
                          group_id="bloc",
                          group_cov = ~change*bloc,
                          miss_val="4")

estimate_all <- id_estimate(arp_ideal_data,use_vb = T,
                            use_groups = T,
                            restrict_ind_high="Islamists",
                            restrict_ind_low = "Secularists",
                            model_type=4,
                            vary_ideal_pts = 'AR1',
                            time_sd=.25,
                            fixtype='vb_partial',
                            tol_rel_obj=0.0001)

saveRDS(estimate_all,'data/estimate_all_2groups_ar_vb.rds')

# run 2-group random-walk model

arp_ideal_data <- id_make(score_data = all_data,
                          outcome="clean_votes",
                          person_id="legis_names",
                          item_id="law_unique",
                          time_id="law_date",
                          group_id="bloc",
                          miss_val="4")

estimate_all_rw <- id_estimate(arp_ideal_data,use_vb = T,
                            use_groups = T,
                            restrict_ind_high="Islamists",
                            restrict_ind_low = "Secularists",
                            model_type=4,
                            vary_ideal_pts = 'random_walk',
                            time_sd=.2,
                            fixtype='vb_partial',
                            tol_rel_obj=0.0001)

saveRDS(estimate_all_rw,'data/estimate_all_2groups_rw_vb.rds')



