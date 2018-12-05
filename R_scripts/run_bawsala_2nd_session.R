# Run party-level model on 2nd Tunisian parliamentary session
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


# load data
all_data <- readRDS('data/combine_sessions.rds')

# only select second session

all_data <- filter(all_data,law_date>ymd('2015-01-01'))

# create covariate for carthage agreement

all_data <- mutate(all_data, change=ifelse(!is.na(this_legis_bloc2) & this_legis_bloc2=='Bloc Al Horra du Mouvement Machrouu Tounes',
                                           as.numeric(law_date>date_change),0))

# switch bloc back to what the individual party names

all_data <- mutate(ungroup(all_data),
                   bloc=case_when(is.na(this_legis_bloc2)~this_legis_bloc,
                                  law_date<date_change~this_legis_bloc,
                                  law_date>=date_change~this_legis_bloc2),
                   bloc=textclean::replace_non_ascii(iconv(bloc,from='LATIN1','UTF-8')),
                   bloc=recode(bloc,
                               `Afek Tounes et l'appel des tunisiens A l'A(C)tranger`="Afek Tounes et l'appel des tunisiens l'tranger",
                               `Bloc DA(C)mocrate`="Bloc Social-Dmocrate",
                               `Bloc Social-DA(C)mocrate`="Bloc Social-Dmocrate",
                               `FrontEPopulaire`="Front Populaire"))

all_data$bloc <-  recode(all_data$bloc,
                         `Afek Tounes et l'appel des tunisiens l'tranger`="Afek Tounes",
                         `Bloc Social-Dmocrate`="Social-Démocrate",
                         `Bloc Al Horra du Mouvement Machrouu Tounes`="Horra",
                         `Mouvement Ennahdha`="Nahda",
                         `Mouvement Nidaa Tounes`='Nidaa Tounes',
                         `Alliance Dmocratique`="Alliance Démocratique",
                         `AllA(C)geance A la Patrie`="Allégeance A la Patrie")

# we need to complete the data for horra and others who aren't in every time point
# this is for group covariate plotting
all_data <- all_data %>% complete(law_date,nesting(bloc,change),fill=list(change=0))

# first run an AR(1) model with covariates

arp_ideal_data <- id_make(score_data = all_data,
                          outcome="clean_votes",
                          person_id="legis_names",
                          item_id="law_unique",
                          time_id="law_date",
                          group_id="bloc",
                          group_cov=~change*bloc,
                          miss_val="4")



estimate_all <- id_estimate(arp_ideal_data,use_vb = T,
                            use_groups = T,
                            restrict_ind_high= "Nahda",
                            restrict_ind_low="Front Populaire",
                            model_type=4,
                            restrict_var = T,
                            restrict_var_high = 0.25,
                            vary_ideal_pts = 'AR1',
                            time_sd=1,
                            fixtype='vb_partial',
                            tol_rel_obj=0.0001)



saveRDS(estimate_all,'data/estimate_all_ar1_vb.rds')



arp_ideal_data <- id_make(score_data = all_data,
                          outcome="clean_votes",
                          person_id="legis_names",
                          item_id="law_unique",
                          time_id="law_date",
                          group_id="bloc",
                          miss_val="4")

estimate_all_rw <- id_estimate(arp_ideal_data,use_vb = T,
                               use_groups = T,
                               restrict_ind_high= "Nahda",
                               restrict_ind_low="Front Populaire",
                               model_type=4,
                               restrict_var = T,
                               restrict_var_high = 0.1,
                               vary_ideal_pts = 'random_walk',
                               time_sd=1,
                               fixtype='vb_partial',niters=1000,
                               tol_rel_obj=0.0001)



saveRDS(estimate_all_rw,'data/estimate_all_rw_vb.rds')




