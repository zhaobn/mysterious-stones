
library(dplyr)
library(ggplot2)
library(ggExtra)

load('../data/results/individual_best_fits.Rdata')
load('../data/labels.Rdata')
load('ind_ndp_fits.Rdata')

ind<-do.call(rbind.data.frame, individual_best_fits)
ind$ix<-rownames(ind)
rownames(ind)<-NULL
ind<-ind %>%
  mutate(ix=as.numeric(as.character(substr(ix, 2, nchar(ix))))) %>%
  select(ix, alpha, beta, gamma, base, raw_ll, fitted_ll, id) %>%
  left_join(labels, by='ix') %>%
  select(ix, condition, alpha, beta, gamma, base, raw_ll, fitted_ll, id, categorization) %>%
  mutate(
    fix=if_else(condition %in% c('A1', 'A3'), 'A', 'R'),
    fix_cond=if_else(condition %in% c('A1', 'A3'), 'Fix A', 'Fix R'),
    rule_change=if_else(condition %in% c('A1','A2'), 'edge', 'shade'),
    rule_change_cond=if_else(condition %in% c('A1','A2'), 'Rule edge(A)', 'Rule shade(A)'))

# Gamma parameters
ggplot(ind, aes(x=gamma)) + geom_density()
ggplot(ind, aes(x=gamma)) + geom_bar() + 
  scale_x_continuous(breaks=c(0,.25,.5,.75,1))

# Gamma per condition
ind %>%
  group_by(fix_cond, rule_change_cond, condition) %>%
  summarise(gamma=mean(gamma)) %>%
  ggplot(aes(x=rule_change_cond, y=gamma, fill=condition)) +
  geom_bar(stat='identity') +
  facet_grid(~fix_cond, switch = "x", scales = "free_x", space = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside") +
  labs(x='', y='', title='Mean gamma') +
  theme(legend.position = "none") +
  geom_text(aes(label=round(gamma, 2)), position=position_dodge(width=0.9), vjust=-0.25) +
  ylim(0, .7) +
  scale_fill_brewer(palette='Paired')

# Gamma per categorization
ind %>%
  mutate(
    categorization=ifelse(categorization!='universal', 'C', 'U'),
    condition=factor(condition, levels=c('A1','A3','A2','A4'))) %>%
  group_by(categorization, condition) %>%
  summarise(gamma=mean(gamma)) %>%
  ggplot(aes(x=categorization, y=gamma, fill=categorization)) +
  geom_bar(stat='identity') +
  facet_grid(~condition) +
  scale_fill_brewer(palette='Paired') +
  labs(x='', y='Mean gamma')


# Curious: fitted likelihood per condition??
ind %>%
  ggplot(aes(x=fitted_ll, color=condition)) +
  geom_density()

# Curious: b? - not very helpful
ind %>%
  ggplot(aes(x=base, color=condition)) +
  geom_density()

# Alpha per condition
ind %>%
  ggplot(aes(x=alpha, color=condition)) +
  geom_density()

library(RColorBrewer)
library(colorRamps)
getPalette = colorRampPalette(brewer.pal(9, "Blues"))

ind %>%
  mutate(alpha=as.factor(alpha)) %>%
  ggplot(aes(x=alpha, fill=alpha)) +
  geom_bar(stat='count', position = 'dodge') +
  facet_grid(fix_cond~rule_change_cond) +
  scale_fill_manual(values = rev(getPalette(20))) +
  theme(legend.position = 'none')

# NOPE
ind %>%
  mutate(gamma=as.factor(gamma)) %>%
  ggplot(aes(x=gamma, fill=gamma)) +
  geom_bar(stat='count', position = 'dodge') +
  facet_grid(fix_cond~rule_change_cond) +
  scale_fill_manual(values = rev(getPalette(10))) +
  theme(legend.position = 'none')


ind %>%
  group_by(fix_cond, rule_change_cond, condition) %>%
  summarise(alpha=mean(alpha, na.rm=T)) %>%
  ggplot(aes(x=rule_change_cond, y=alpha, fill=condition)) +
  geom_bar(stat='identity') +
  facet_grid(~fix_cond, switch = "x", scales = "free_x", space = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside") +
  labs(x='', y='', title='Mean alpha') +
  theme(legend.position = "none") +
  geom_text(aes(label=round(alpha)), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_brewer(palette='Paired')

# Beta per condition
ind %>%
  ggplot(aes(x=beta, color=condition)) +
  geom_density()

ind %>%
  mutate(beta=as.factor(beta)) %>%
  ggplot(aes(x=beta, fill=beta)) +
  geom_bar(stat='count', position = 'dodge') +
  facet_grid(fix_cond~rule_change_cond) +
  scale_fill_manual(values = rev(getPalette(30))) +
  theme(legend.position = 'none')

ind %>%
  group_by(fix_cond, rule_change_cond, condition) %>%
  summarise(beta=mean(beta, na.rm=T)) %>%
  ggplot(aes(x=rule_change_cond, y=beta, fill=condition)) +
  geom_bar(stat='identity') +
  facet_grid(~fix_cond, switch = "x", scales = "free_x", space = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside") +
  labs(x='', y='', title='Mean beta') +
  theme(legend.position = "none") +
  geom_text(aes(label=round(beta)), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_brewer(palette='Paired')

# alpha, beta, gamma
ind %>%
  mutate(fitted_gamma=factor(gamma,levels=c(0, .25, .5, .75, 1))) %>%
  ggplot(aes(x=alpha, y=beta, color=fitted_gamma)) +
  geom_point() +
  labs(x='fitted alpha', y='fitted beta') +
  scale_color_brewer(type='div', palette=6) +
  theme_bw()


ind %>%
  mutate(l_alpha=log(alpha), l_beta=log(beta), 
         fitted_gamma=factor(gamma,levels=c(0, .25, .5, .75, 1))) %>%
  ggplot(aes(x=l_alpha, y=l_beta, color=fitted_gamma)) +
  geom_point() +
  labs(x='log(alpha)', y='log(beta)') +
  scale_color_brewer(type='div', palette=6) +
  theme_bw()

ind %>%
  mutate(l_alpha=log(alpha), l_beta=log(beta), 
         fitted_gamma=factor(gamma,levels=c(0, .25, .5, .75, 1))) %>%
  ggplot(aes(x=l_alpha, y=l_beta, color=fitted_gamma)) +
  geom_point() +
  labs(x='log(alpha)', y='log(beta)') +
  scale_color_brewer(type='div', palette=6) +
  theme_bw() +
  facet_grid(fix_cond~rule_change_cond)


ind %>%
  group_by(condition) %>%
  summarise(
    mean_alpha=mean(alpha),
    mean_beta=mean(beta),
    mean_gamma=mean(gamma)) %>%
  pivot_longer(cols=starts_with('mean'), names_to='param', values_to='mean') %>%
  mutate(param=gsub('mean_', '', param)) %>%
  filter(param!='gamma') %>%
  mutate(condition=factor(condition, levels=c('A1','A3','A2','A4'))) %>%
  ggplot(aes(x=condition, y=mean, fill=condition)) +
  geom_bar(stat='identity') +
  scale_fill_brewer(palette='Paired') +
  theme(legend.position = 'none') +
  facet_grid(~param)


# Plot 3D
library(rgl)
log_ind<-ind %>%
  mutate(l_alpha=log(alpha), l_beta=log(beta)) %>%
  mutate(l_beta=ifelse(l_beta==-Inf, -2.5, l_beta))

plot3d(log_ind$l_alpha, log_ind$l_beta, log_ind$gamma, 
       type="s", size=1, lit=TRUE, 
       main = "Fitted parameter")

# Alphas per condition
ind %>% ggplot(aes(x=alpha, color=condition)) + geom_density() 
log_ind %>%
  ggplot(aes(x=l_alpha, color=condition)) +
  geom_density() +
  xlab('log(alpha)')

# Betas per condition
ind %>% ggplot(aes(x=beta, color=condition)) + geom_density() 
log_ind %>%
  ggplot(aes(x=l_beta, color=condition)) +
  geom_density() +
  xlab('log(beta)')

# Play with marginal
# classic plot :
p <- ggplot(log_ind, aes(x=l_alpha, y=l_beta, color=condition)) +
  geom_point() +
  theme(legend.position="none")

# with marginal histogram
p1 <- ggMarginal(p, type="histogram")

# marginal density
p2 <- ggMarginal(p, type="density")

# marginal boxplot
p3 <- ggMarginal(p, type="boxplot")

p<-p + facet_grid(~condition)
ggMarginal(p, type="boxplot")



# Compare with random baseline (BIC) 
rand_bic_val=-2*(log(1/20)*16)

ind$dp_bic=4*log(16)-2*ind$fitted_ll
ind$rand_bic=rand_bic_val

ind %>% 
  ggplot(aes(x=reorder(as.character(ix), dp_bic), y=dp_bic, fill=dp_bic<rand_bic_val)) +
  geom_bar(stat='identity') +
  geom_hline(yintercept=rand_bic_val) +
  labs(x='', y='', title='DP model BIC per individual') +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        legend.position='none')


# Add ndp fits
ind_bics<-ind_ndp_fits %>%
  select(ix, ndp_raw_ll=raw_ll, ndp_base=base, ndp_fitted_ll=fitted_ll) %>%
  left_join(ind, by='ix') %>%
  select(ix, rand_bic, 
         dp_raw_ll=raw_ll, dp_fitted_ll=fitted_ll, dp_bic,
         ndp_raw_ll, ndp_fitted_ll)

ind_bics$ndp_bic=1*log(16)-2*ind_bics$ndp_fitted_ll

ind_bics %>% filter(ndp_bic<rand_bic)
ind_bics %>% filter(ndp_bic<dp_bic)

# Mix plot
ind_bics %>%
  mutate(m_bic=ifelse(dp_bic<ndp_bic, dp_bic, ndp_bic)) %>%
  mutate(bic_label=ifelse(rand_bic_val<m_bic, 'DP < Rand',
                          ifelse(dp_bic<ndp_bic, 'DP', 'Universal'))) %>%
  mutate(bic_label=factor(bic_label, levels=c('Universal', 'DP', 'DP < Rand'))) %>%
  ggplot(aes(x=reorder(as.character(ix), m_bic), y=m_bic, fill=bic_label)) +
  geom_bar(stat='identity') +
  geom_hline(yintercept=rand_bic_val) +
  labs(x='', y='', title='Best model BIC per individual') +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        legend.title=element_blank()) +
  scale_fill_manual(values=c("red4", "royalblue4", "#999999"))


# Try clustering best-DP fitted parameters
dp_fitted<-ind_bics %>% filter(dp_bic<rand_bic, dp_bic<ndp_bic) %>% pull(ix)
dp_best<-ind %>% filter(ix %in% dp_fitted)

dp_best %>%
  mutate(l_alpha=log(alpha), l_beta=log(beta), 
         fitted_gamma=factor(gamma,levels=c(0, .25, .5, .75, 1))) %>%
  ggplot(aes(x=l_alpha, y=l_beta, color=fitted_gamma)) +
  geom_point() +
  labs(x='log(alpha)', y='log(beta)') +
  scale_color_brewer(type='div', palette=6) +
  theme_bw()

dp_best %>%
  mutate(l_alpha=log(alpha), l_beta=log(beta), 
         fitted_gamma=factor(gamma,levels=c(0, .25, .5, .75, 1))) %>%
  ggplot(aes(x=l_alpha, y=l_beta, color=fitted_gamma)) +
  geom_point() +
  labs(x='log(alpha)', y='log(beta)') +
  scale_color_brewer(type='div', palette=6) +
  theme_bw() +
  facet_grid(fix_cond~rule_change_cond)


# Play with mclust
library(mclust)
data(diabetes)
class <- diabetes$class
X <- diabetes[,-1]
clPairs(X, class)

Y<-dp_best %>% select(alpha, beta, gamma, base)
Z<-dp_best %>% mutate(l_alpha=log(alpha),l_beta=log(beta)) %>% 
  mutate(l_beta=ifelse(l_beta==-Inf, -5, l_beta)) %>%
  select(l_alpha, l_beta, gamma)
clPairs(Y)
clPairs(Z)

BIC<-mclustBIC(Y)
plot(BIC)
summary(BIC)

mod1 <- Mclust(Y, x = BIC)
summary(mod1, parameters = TRUE)
plot(mod1, what = "classification")

# Try plot on log scale
dp_best$mod1<-mod1$classification
ggplot(dp_best, aes(x=factor(mod1))) + geom_dotplot()

dp_best %>%
  ggplot(aes(x=log(alpha), y=log(beta), 
             color=factor(gamma,levels=c(0, .25, .5, .75, 1)))) +
  geom_point() +
  scale_color_brewer(type='div', palette=6) +
  facet_wrap(~mod1) +
  labs(color='Gamma')

dp_best %>%
  ggplot(aes(x=log(alpha), y=log(beta), 
             color=base)) +
  geom_point() +
  facet_wrap(~mod1) +
  labs(color='Base')

# People in cluster 1 and 3
dp_best %>%
  filter(mod1==1|mod1==3) %>%
  ggplot(aes(x=log(alpha), y=log(beta), 
             color=factor(gamma), shape=factor(mod1))) +
  geom_point() +
  facet_grid(fix_cond~rule_change_cond)


# Clustering vs likelihood? Quite widely-spread
ggplot(dp_best, aes(x=reorder(ix, -fitted_ll), y=-fitted_ll, 
                    fill=factor(mod1))) +
  geom_bar(stat='identity', position='dodge') +
  theme(axis.text.x = element_blank(), axis.ticks.x=element_blank()) +
  labs(x='', y='') +
  facet_wrap(~mod1)

# Plot clusters against conditions
ggplot(dp_best, aes(x=rule_change_cond, fill=factor(mod1))) +
  geom_bar(stat='count', position='fill') +
  facet_grid(~fix_cond, switch = "x", scales = "free_x", space = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside") +
  labs(x='', y='', title='Clusters per condition', fill='Cluster') +
  scale_fill_brewer(palette='Set1')

# # Clustering vs self-report label??? Nothing interesting
# clust_label<-dp_best %>% left_join(rule_cat, by='ix') %>%
#   select(ix, mod1, condition=condition.x, cat, rut, cause_effect) %>%
#   filter(!is.na(cause_effect))
# 
# ggplot(clust_label, aes(x=cat, fill=factor(mod1))) +
#   geom_bar(stat='count', position='fill')
# 
# ggplot(clust_label, aes(x=condition, fill=factor(mod1))) +
#   geom_bar(stat='count', position='fill')
# 
# ggplot(clust_label, aes(x=rut, fill=factor(mod1))) +
#   geom_bar(stat='count', position='fill')

# # Cluster based on log value - NO
# log_BIC<-mclustBIC(Z)
# mod2 <- Mclust(Z, x = log_BIC)
# summary(mod2, parameters = TRUE)
# plot(mod2, what = "classification")
# 
# dp_best$mod2=mod2$classification
# dp_best %>%
#   ggplot(aes(x=log(alpha), y=log(beta), color=factor(gamma,levels=c(0, .25, .5, .75, 1)))) +
#   geom_point() +
#   labs(x='log(alpha)', y='log(beta)', color='Gamma') +
#   scale_color_brewer(type='div', palette=6) +
#   theme_bw() +
#   facet_wrap(~mod2)








