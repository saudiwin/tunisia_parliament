# Load models and create ideal point plots and discrimination tables/plots
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

arp_ar1 <- readRDS('data/estimate_all_ar1_vb.rds')
arp_rw <- readRDS('data/estimate_all_rw_vb.rds')
group2_ar1 <- readRDS('data/estimate_all_2groups_ar_vb.rds')
group2_rw <- readRDS('data/estimate_all_2groups_rw_vb.rds')

# basic descriptives

# move plot to bawsala plot

all_data <- readRDS('data/combine_sessions.rds')

select(all_data,law_unique,law_date) %>% 
  distinct %>% 
  ggplot(aes(x=law_date)) + geom_histogram(fill='grey',
                                           colour=NA) + 
  theme_minimal() + xlab('') + ylab('Number of Roll Call Votes') +
  theme(panel.grid=element_blank()) +
  geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
             linetype=2) +
  annotate(geom='text',x=ymd('2016-07-30'),y=450,label='Carthage Agreement') +
  ggtitle('Legislative Activity in the Tunisian Parliament')
ggsave('bill_density.png')

# test with linear model

# need ideal point scores

all_scores <- summary(group2_rw,aggregate=F) %>% 
  group_by(Time_Point) %>% 
  summarize(polar=median(Ideal_Points[Group=='Islamists']) - median(Ideal_Points[Group=='Secularists']),
            polar_high=quantile(Ideal_Points[Group=='Islamists'],.95) - quantile(Ideal_Points[Group=='Secularists'],.95),
            polar_low=quantile(Ideal_Points[Group=='Islamists'],.05) - quantile(Ideal_Points[Group=='Secularists'],.05)) %>% 
  left_join(all_data,by=c(Time_Point='law_date'))

all_scores %>% 
  distinct(polar,Time_Point,.keep_all = T) %>% 
  ggplot(aes(y=polar,x=Time_Point)) +
  geom_line(linetype=2,size=1) +
  geom_ribbon(aes(ymin=polar_high,
                  ymax=polar_low),
              fill='grey80',
              alpha=0.5) +
  ylab('Difference Between Islamists and Secularists') +
  xlab('') +
  theme(panel.grid=element_blank(),
        panel.background = element_blank())

ggsave('diff_over_time.png')

all_scores_dist <- distinct(all_scores,polar,Time_Point,law_unique) %>% 
  count(polar,Time_Point)

summary(lm(n~polar,data=all_scores_dist))

# plot relative approval of project (aid) finance

all_data <- mutate(all_data,
                   proj_finance=grepl(x=law_unique,
                                      pattern='financement du projet') & grepl(x=law_unique,
                                                                               pattern='accord'))

all_data %>% group_by(law_date) %>% 
  summarize(mean_proj=mean(proj_finance)) %>% 
  ggplot(aes(y=mean_proj,
             x=law_date)) +
  geom_col() + 
  scale_y_continuous(labels=scales::percent) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank()) +
  xlab('') + 
  ylab('Percentage of Votes') +
  geom_vline(xintercept = ymd('2015-02-01'),linetype=2) +
  annotate('text',
           x=ymd('2013-12-01'),
           y=.25,
           label='2015 National Unity Government Formed')

ggsave('vote_finance.png')

 # ARP AR1 ---------------------------------------------------

# need to reverse the scale because the ID strategy ends up reversing itself
# for compatibility with the RW model

id_plot_legis_dyn(arp_ar1,person_plot=F,use_ci = F,plot_text = F) +
  geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
             linetype=2) +
   scale_y_continuous(labels=c('More\nIslamist','6','3','0.0','-3','-6','More\nSecular'),
                      breaks=c(10,6,3,0.0,-3,-6,-10)) +
  guides(colour='none') +
  facet_wrap(~group_id) 
  #ggtitle('Stationary Party Ideal Points for Tunisian National Representative Assembly (ARP)')


ggsave('party_over_time_ar1_vb.png')

id_plot_cov(arp_ar1) + 
  scale_x_continuous(labels=c('More\nIslamist','5','0.0','-5','More\nSecular'),
                     breaks=c(8,5,0.0,-5,-8)) 
  # ggtitle('Effect of Carthage Agreement on Party-level Ideal Points',
  #         subtitle = 'Based on Rollcall Vote Data from the Tunisian National Representative Assembly (ARP)')

ggsave('id_plot_cov_arp_all.png')

id_plot_cov(arp_ar1,filter_cov = c('change:blocHorra','change'))

ggsave('id_plot_cov_arp_horra.png')

id_plot_legis_var(arp_ar1,person_labels = F)

ggsave('id_plot_var_arp_ar1.png')


# ARP rw ------------------------------------------------------------------

id_plot_legis_dyn(arp_rw,person_plot=F,use_ci = F,
                  highlight=c('Nahda','Nidaa Tounes','Front Populaire','Horra')) +
  geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
             linetype=2) +
  scale_y_continuous(labels=c('More\nSecular','-1.5','0.0','1.5','More\nIslamist'),
                     breaks=c(-2.5,-1.5,0.0,1.5,2.5)) +
  guides(colour='none') 
  #ggtitle('Random-Walk Party Ideal Points for Tunisian National Representative Assembly (ARP)')

ggsave('party_over_time_rw_vb.png')

id_plot_legis_dyn(arp_rw,person_plot=F,use_ci = T,plot_text = F) +
  geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
             linetype=2) +
  scale_y_continuous(labels=c('More\nSecular','-1.5','0.0','1.5','More\nIslamist'),
                     breaks=c(-2.5,-1.5,0.0,1.5,2.5)) +
  guides(colour='none') + 
  facet_wrap(~group_id) 
  #ggtitle('Random-Walk Party Ideal Points for Tunisian National Representative Assembly (ARP)')

ggsave('party_over_time_panel_rw_vb.png')

id_plot_legis_var(arp_rw,person_labels = F)

ggsave('id_plot_var_arp_rw.png')



# 2 groups RW -------------------------------------------------------------


id_plot_legis_dyn(group2_rw,
                  group_color=F,person_plot=F,text_size_label=8) +
  geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
             linetype=2) +
  geom_vline(aes(xintercept=lubridate::ymd('2014-10-26')),
             linetype=3) +
  geom_vline(aes(xintercept=lubridate::ymd('2013-10-23')),
             linetype=4) +
  annotate(geom='text',x=ymd('2016-07-30'),y=0.9,label=' Carthage Agreement') +
  annotate(geom='text',x=ymd('2014-12-2'),y=0.65,label='New Parliament\nSession') +
  annotate(geom='text',x=ymd('2013-10-23'),y=.7,label='Troika\nNegotiations') +
  scale_y_continuous(labels=c('More\nSecular','0.0','0.5','More\nIslamist'),
                     breaks=c(0.0,0.5,1.0,1.5),
                     limits=c(-0.25,1.5)) +
  scale_color_discrete(guide='none') + 
  scale_x_date(date_breaks = '1 year',
               date_labels='%Y')

ggsave('party_over_time_2groups_1mo_rw.png')


# 2 groups AR1 ------------------------------------------------------------

id_plot_legis_dyn(group2_ar1,
                  group_color=F,person_plot=F,text_size_label=8) +
  geom_vline(aes(xintercept=lubridate::ymd('2016-07-30')),
             linetype=2) +
  geom_vline(aes(xintercept=lubridate::ymd('2014-10-26')),
             linetype=3) +
  geom_vline(aes(xintercept=lubridate::ymd('2013-10-23')),
             linetype=4) +
  annotate(geom='text',x=ymd('2016-07-30'),y=0.2,label=' Carthage Agreement') +
  annotate(geom='text',x=ymd('2014-12-2'),y=1.8,label='New Parliament\nSession') +
  annotate(geom='text',x=ymd('2013-10-23'),y=.7,label='Anti-Islamist\nProtests') +
  scale_y_continuous(labels=c('More\nSecular','0.0','0.5','More\nIslamist'),
                     breaks=c(-0.5,0.0,1,2)) +
  scale_color_discrete(guide='none') + 
  scale_x_date(date_breaks = '1 year',
               date_labels='%Y')

ggsave('party_over_time_2groups_1mo_ar.png')

# covariate plot

id_plot_cov(group2_ar1) +
  ggtitle('Effect of 2014 Election on Party-level Ideal Points',
          subtitle = 'Based on Rollcall Vote Data from 1st and 2nd Sessions of Tunisian Parliament') +
  scale_x_continuous(labels=c('More\nSecular','-0.25','0.0','0.25','More\nIslamist'),
                     breaks=c(-0.5,-0.25,0.0,0.25,0.5)) 

ggsave('id_plot_cov_2groups.png')

# calculate interaction for Islamists

cov_iter <- rstan::extract(group2_ar1@stan_samples,'legis_x')

median(cov_iter[[1]][,3] + cov_iter[[1]][,4])
quantile(cov_iter[[1]][,3] + cov_iter[[1]][,4],.95)
quantile(cov_iter[[1]][,3] + cov_iter[[1]][,4],.05)

median(cov_iter[[1]][,3])
quantile(cov_iter[[1]][,3],.95)
quantile(cov_iter[[1]][,3],.05)


# bill discrim ------------------------------------------------------------



# pull out 2-group bill discrimination parameters 

all_params <- summary(group2_rw,pars='items')

# plot item midpoints across categories

all_params %>% 
  filter(grepl(pattern = 'Discrimination',x=`Item Type`)) %>% 
  ggplot(aes(x=`Posterior Median`)) +
  geom_density(aes(fill=`Item Type`),colour=NA,alpha=0.5) +
  scale_fill_brewer(name='Parameter\nType') +
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) +
  xlab('Ideal Point Scale') +
  scale_x_continuous(labels=c('More\nSecular','0.0','More\nIslamist'),
                     breaks=c(-2.5,0.0,2.5)) +
  ylab('Density') +
  ggtitle('Discrimination Values of Bills for Combined Tunisian Parliament',
          subtitle = 'Higher Distance from Zero Indicates Higher Bill Polarization')

ggsave('combined_discrim_density.png')

all_out_top <- all_params %>% 
  filter(grepl(x=`Item Type`, pattern='Non-Inflated Discrimination')) %>% 
  arrange(desc(`Posterior Median`)) %>% 
  slice(1:25) %>% 
  select(Vote='Parameter',
    `Low Interval`="Low Posterior Interval",
    `Discrimination Score`="Posterior Median",
    `High Interval`="High Posterior Interval") %>% 
  xtable(.)
print(all_out_top,type='latex',file='discrim_bill_combined_high.tex')

all_out_bottom <- all_params %>% 
  filter(grepl(x=`Item Type`, pattern='Non-Inflated Discrimination')) %>% 
  arrange(`Posterior Median`) %>% 
  slice(1:25) %>% 
  select(Vote='Parameter',
         `Low Interval`="Low Posterior Interval",
         `Discrimination Score`="Posterior Median",
         `High Interval`="High Posterior Interval") %>% 
  xtable(.)

print(all_out_bottom,type='latex',file='discrim_bill_combined_low.tex')

# Calculate average discrimination of items over time

bill_sum <- left_join(all_params,group2_ar1@score_data@score_matrix,by=c(Parameter='item_id'))

require(ggridges)

bill_plot <- bill_sum %>% 
  distinct(Parameter,time_id,`Posterior Median`,`Item Type`) %>% 
  mutate(Discrimination=`Posterior Median`) %>% 
  filter(grepl(x=`Item Type`,
               pattern='Discrimination'))

  ggplot(bill_plot,aes(x=Discrimination,
             y=time_id,
             group=time_id)) +
  geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03,
                      colour='white',
                      alpha=0.8,
                      aes(fill=`Item Type`)) +
    ylab('') +
    scale_fill_brewer(palette='Paired') +
    guides(fill='none') +
    scale_x_continuous(labels=c('More\nIslamist','2','0.0','-2','More\nSecular'),
                       breaks=c(4,2,0.0,-2,-4)) +
    xlab('Bill Discrimination Values') +
    theme_ridges(grid=FALSE) +
    ggtitle('Density of Tunisian Parliament Bill Discrimination Over Time') +
    facet_wrap(~`Item Type`) +
    theme(strip.background = element_blank())
  
ggsave('bill_plot_density.png')
    

bill_plot %>% 
  mutate(avg_sq_discrim=abs(Discrimination^2)) %>% 
  ggplot(aes(y=avg_sq_discrim,x=time_id)) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank()) +
  ylab('Absolute Discrimination') +
  geom_point(alpha=0.2) +
  stat_summary(colour='red',fun.data='mean_cl_normal',size=.6) +
  ggtitle('Absolute Discrimination of Bills From Both Tunisian Parliaments',
          subtitle='Red Points Show Average Discrimination by Time Point') +
  facet_wrap(~`Item Type`) +
  xlab('') +
  theme(strip.background = element_blank())

ggsave('bill_plot_discrim_avg.png')
  
# pull out ARP bill discrimination parameters 

# all_params <- summary(estimate_all_rw,pars='items')
# just_discrim <- filter(all_params,grepl(pattern = 'sigma_reg_free',x=parameters)) %>% 
#   mutate(abs_score=abs(posterior_median),
#          index=as.numeric(str_extract(parameters,'[0-9]+'))) %>% 
#   arrange(desc(abs_score))
# group_ids <- select(estimate_all_rw@score_data@score_matrix,item_id) %>% 
#   mutate(index=as.numeric(item_id)) %>% 
#   distinct
# 
# just_discrim <- left_join(just_discrim,group_ids,'index')
# 
# all_out <- xtable(select(just_discrim,
#                          Vote='item_id',
#                          `Discrimination Score`="posterior_median",
#                          `Standard Deviation (Error)`="posterior_sd"))
# print(all_out,type='latex',file='discrim_bill.tex')


