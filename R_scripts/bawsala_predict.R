# Create predictive validity checks from Appendix A
# Code for When National Unity Governments are neither National, United, nor Governments: The Case of Tunisia
# by Robert Kubinec 

library(idealstan)
require(bayesplot)
require(dplyr)
require(tidyr)
require(ggplot2)
require(lubridate)
require(stringr)
require(forcats)
require(xtable)

arp_ar1 <- readRDS('data/estimate_all_ar1_vb.rds')
arp_rw <- readRDS('data/estimate_all_rw_vb.rds')
group2_ar1 <- readRDS('data/estimate_all_2groups_ar_vb.rds')
group2_rw <- readRDS('data/estimate_all_2groups_rw_vb.rds')

comb_data <- readRDS('data/combine_sessions.rds')

# see how long it takes to do predictions

# if saved predictions don't exist, run this command (will take some time)
# all_pred <- id_post_pred(group2_rw)
all_pred <- readRDS('data/all_pred.rds')
# now calculate using apply function

all_data <- readRDS('data/combine_sessions.rds')

correct <- apply(all_pred,1,function(col) {
  col==as.numeric(all_data$clean_votes)
})

# overall plot
id_plot_ppc(group2_rw,all_pred) + scale_x_discrete(labels=c('No','Abstain','Yes',"Abstain"))

ggsave('overall_pred.png')

plot_pred <- bind_cols(select(ungroup(all_data),law_date,legis_names,clean_votes),as_data_frame(correct)) %>% 
  gather(key = iter,value=predicted,-law_date,-legis_names,-clean_votes)

all_sum <- plot_pred %>% 
  mutate(Type=factor(clean_votes,labels=c('No',
                                          'Abstain',
                                          'Yes',
                                          'Absent'))) %>% 
  group_by(law_date) %>% 
  mutate(law_n=n()) %>% 
  group_by(law_date,Type,iter) %>% 
  # need to create percentage imiprovement over baseleine
  summarize(perc_correct_iter=(mean(predicted) - (n()/law_n[1]))/(n()/law_n[1])) %>% 
  group_by(law_date,Type) %>% 
  mutate(perc_correct=median(perc_correct_iter),
            perc_correct_high=quantile(perc_correct_iter,.95),
            perc_correct_low=quantile(perc_correct_iter,.05))

ggplot(all_sum,aes(y=perc_correct,
             x=law_date)) +
  geom_ribbon(aes(ymin=perc_correct_low,
                  ymax=perc_correct_high,
                  fill=Type),alpha=0.5) +
  stat_smooth(aes(y=perc_correct_iter)) +
  geom_line(aes(linetype=Type)) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank()) +
  geom_vline(xintercept=ymd('2014-12-2'),linetype=2) +
  xlab('') +
  ylab('Percent Improvement Over Null Model') +
  facet_wrap(~Type,scales='free_y') +
  geom_hline(yintercept = 100,linetype=2) + 
  guides(fill='none',linetype='none')

ggsave('obs_vs_abs_pred.png')

# correlation between different types of votes
# drop those that don't have all 4 vote categories

all_sum <- group_by(all_sum,law_date) %>% 
  mutate(n_cat=length(unique(Type)))

yes <- filter(all_sum,Type=="Yes",n_cat==4) %>% pull(perc_correct)
no <- filter(all_sum,Type=="No",n_cat==4) %>% pull(perc_correct)
abstain <- filter(all_sum,Type=="Abstain",n_cat==4) %>% pull(perc_correct)
absent <- filter(all_sum,Type=="Absent",n_cat==4) %>% pull(perc_correct)

cor(cbind(yes,no,abstain,absent))

all_sum_votes <- plot_pred %>% 
  mutate(Type=factor(clean_votes,labels=c('No',
                                          'Abstain',
                                          'Yes',
                                          'Absent'))) %>% 
  group_by(law_date,Type,iter) %>% 
  summarize(perc_correct_iter=mean(predicted)) %>% 
  group_by(law_date,Type) %>% 
  mutate(perc_correct=median(perc_correct_iter),
            perc_correct_high=quantile(perc_correct_iter,.95),
            perc_correct_low=quantile(perc_correct_iter,.05))

ggplot(all_sum_votes,aes(y=perc_correct,
                   x=law_date)) +
  geom_ribbon(aes(ymin=perc_correct_low,
                  ymax=perc_correct_high,
                  fill=Type),alpha=0.5) +
  geom_line(aes(linetype=Type)) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank()) +
  xlab('') +
  ylab('Percent Correctly Predicted') +
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(~Type)

ggsave('all_vote_types_pred.png')

# overall predictive validity table 

prop.table(table(all_data$clean_votes))
group_by(all_sum_votes,Type) %>% summarize(median_cor=median(perc_correct_iter),
                                     high_cor=quantile(perc_correct_iter,.95),
                                     low_cor=quantile(perc_correct_iter,.05))

