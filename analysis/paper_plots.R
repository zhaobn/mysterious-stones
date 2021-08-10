
# Libraries
library(tidyverse)
library(ggpubr)
library(GA)
library(mclust)
rm(list = ls())
theme_set(theme_classic())

# Data
load('../data/mturk/mturk_main.Rdata')
load('../data/gen_labeled.Rdata')
load('../data/labels.Rdata')

load('../data/results/gammas/dpa_2.Rdata')
load('ind_ndp_fits.Rdata')

#### Gen data ####
# Violin plot of cronbach's alpha
a<-gen_labeled %>%
  mutate(rule_change_cond=ifelse(rule_change_cond=='Rule edge(A)', 'Rule 1', 'Rule 2')) %>%
  ggplot(aes(x=rule_change_cond, y=cronbach_alpha, fill=condition)) +
  geom_violin() +
  facet_grid(~fix_cond, switch = "x", scales = "free_x", space = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "none") +
  labs(x='', y='', title='') +
  scale_x_discrete(position = "top") +
  scale_fill_brewer(palette="Paired")
a

# Cronbach alpha ~ similarity
b<-gen_labeled %>%
  mutate(condition=gsub('A', 'B', condition)) %>%
  ggplot(aes(x=total_diff, y=cronbach_alpha, shape=condition, color=condition)) +
  geom_point(size=2) +
  geom_smooth(method = "lm", fill=NA) +
  scale_color_brewer(palette="Paired") +
  labs(x='Dissimilarity', y='', title='') +
  theme(legend.title = element_blank(),
        legend.position="top") 
b

# New plots
x<-gen_labeled %>%
  select(condition, trial, ends_with('diff'), cronbach_alpha) %>%
  pivot_longer(cols=ends_with('diff'), names_to='diff', values_to='value')

# Cronbach alpha ~ role similarity
c<-x %>%
  filter(diff %in% c('agent_diff', 'recipient_diff')) %>%
  mutate(diff=ifelse(diff=='agent_diff', 'agent', 'recipient')) %>%
  ggplot(aes(x=value, y=cronbach_alpha, color=diff, shape=diff)) +
  geom_point(size=2) + labs(x='') +
  scale_x_continuous(breaks = c(0,1,2)) +
  geom_smooth(method = "lm", fill=NA) +
  labs(x='Dissimilarity', y='', title='') +
  theme(legend.title = element_blank(), legend.position="top") +
  scale_color_brewer(palette="Paired")
c

# Cronbach alpha ~ variation similarity
d<-x %>%
  filter(diff %in% c('fixed_diff', 'varied_diff')) %>%
  mutate(diff=ifelse(diff=='fixed_diff', 'fixed', 'varied')) %>%
  ggplot(aes(x=value, y=cronbach_alpha, color=diff, shape=diff)) +
  geom_point(size=2) + labs(x='') +
  scale_x_continuous(breaks = c(0,1,2)) +
  geom_smooth(method = "lm", fill=NA) +
  labs(x='Difference', y='', title='') +
  theme(legend.title = element_blank()) +
  scale_color_brewer(palette="Paired")
d

# Put together
ggarrange(a, b, c, d, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

ggarrange(a, b, c, 
          labels = c("A", "B", "C"), widths=c(1.5,1,1),
          ncol = 3)


#### Self-reports ####
comp_data<-comp_data %>%
  mutate(rule_change_cond=ifelse(rule_change=='edge', 'Rule 1', 'Rule 2')) %>%
  mutate(rule_type=as.character(rule_type)) %>%
  mutate(rule_type=ifelse(grepl('tacit', rule_type), 'tacit', rule_type)) %>%
  mutate(rule_type=factor(rule_type, levels=c('specific', 'fuzzy', 'tacit'))) %>%
  mutate(cause=ifelse(cause=='categorized', 'localized', cause)) %>%
  mutate(localization=categorization)


a<-ggplot(comp_data, aes(x=rule_change_cond, fill=rule_type)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(~fix_cond, switch = "x", scales = "free_x", space = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside") +
  labs(x='', y='', title='Prediction specification') +
  theme(legend.title = element_blank(), legend.position = 'bottom') +
  scale_fill_brewer(palette="Paired")
a

b<-ggplot(comp_data, aes(x=rule_change_cond, fill=localization)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(~fix_cond, switch = "x", scales = "free_x", space = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside") +
  labs(x='', y='', title='Causal law localization') +
  theme(legend.title = element_blank(), legend.position = 'bottom') +
  scale_fill_brewer(palette="Paired")
b


ggarrange(a,b,ncol=2, labels = c('A', 'B'))


c<-comp_data %>%
  mutate(localization=as.character(categorization)) %>%
  mutate(
    cat=ifelse(categorization!='universal', 'categorized', 'universal'),
    rule_t=as.character(rule_type),
    rut=ifelse(grepl('tacit', rule_t), 'tacit', rule_t),
    cause_effect=paste(rut, cat, sep=', ')) %>%
  mutate(cause_effect=factor(cause_effect, levels = c(
    'specific, universal',
    'specific, categorized',
    "fuzzy, universal",
    "fuzzy, categorized",
    "tacit, universal",
    "tacit, categorized"
  ))) %>%
  ggplot(aes(x=rule_change_cond, fill=cause_effect)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(~fix_cond, switch = "x", scales = "free_x", space = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside") +
  labs(x='', y='', title='Cause-effect types') +
  theme(legend.title = element_blank(), legend.position = 'bottom') +
  guides(fill=guide_legend(nrow=3,byrow=TRUE)) +
  scale_fill_brewer(palette="Paired")
c

ggarrange(ggarrange(a,b,nrow=2, labels = c('A', 'B')), 
          c, labels=c('', 'C'), ncol=2)


#### Model results ####
# Sensitivity
alphas<-c(1:10, 2^(4:10))
betas<-c(seq(0,1,.1), 2^(1:10))
read_ll<-function(df, colname, default=0) {
  fits<-matrix(nrow=length(alphas), ncol=length(betas))
  for (i in 1:length(alphas)) {
    for (j in 1:length(betas)) {
      ll_col<-filter(df, alpha==alphas[i], beta==betas[j])
      ll_val<-if (nrow(ll_col)>0) ll_col[, colname] else default
      fits[i,j]<-if (ll_val > 0) -ll_val else ll_val
    }
  }
  return(fits)
}
persp3D(log(alphas), log(betas), read_ll(dpa_grid_2, 'raw_ll', 0),
        theta=20, phi=20, expand=1, 
        xlab="log(alpha)", ylab="log(beta)", zlab="", main='', col.palette=gray.colors, border='black')

# 2D heatmap
# ggplot(dpa_grid_2, aes(log(alpha), log(beta), fill= raw_ll)) + geom_tile()















