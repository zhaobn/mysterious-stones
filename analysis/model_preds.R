
library(dplyr)
library(ggplot2)

load('../data/hdp.Rdata')
load('../data/mturk/mturk_main.Rdata')
load('../data/gen_labeled.Rdata')

# Configure constants
ALPHAS<-c(1:10, 2^(4:10))
BETAS<-c(seq(0,1,.1), 2^(1:10))
GAMMAS<-c(0, .25, .5, .75, 1)
GROUP_SIZE=count(df.sw, condition)

# Helper functions ####
softmax_trial<-function(td, t) {
  td$to_exp<-exp(td$prob*t)
  td$soft<-td$to_exp/sum(td$to_exp)
  return(select(td, group, trial, object, prob, soft))
}
read_model_preds<-function(alpha_val, beta_val, gamma_val, b_val) {
  id<-model_refs %>% filter(alpha==alpha_val, beta==beta_val, gamma==gamma_val) %>% pull(id)
  df<-model_preds[[id]]
  if (typeof(b_val)=='double') {
    added<-softmax_trial(filter(df, group=='A1', trial==1), b_val)
    for (c in paste0('A', 1:4)) {
      for (i in 1:16) {
        if (!(c=='A1'&i==1)) added<-rbind(added, softmax_trial(filter(df, group==c, trial==i), b_val))
      }
    }
    df<-added %>% select(group, trial, object, prob=soft)
  }
  df$condition=df$group
  df<-df %>% left_join(GROUP_SIZE, by='condition')
  df$count<-round(df$prob*df$n)
  return(select(df, group, trial, result=object, prob, count))
}
get_cronbach_alpha<-function(vec) {
  sx<-var(c(1, rep(0,19)))
  k=sum(vec)
  sy<-var(vec)
  return(k/(k-1)*(1-(k*sx)/sy))
} 
get_model_cas<-function(alpha, beta, gamma, base='') {
  preds<-read_model_preds(alpha, beta, gamma, base)
  # Calculate Cronbach alpha
  consistency<-expand.grid(
    condition=paste0('A',1:4), trial=1:16, cronbach_alpha=NA, 
    stringsAsFactors = F
  ) %>% 
    arrange(condition, trial)
  for (i in 1:nrow(consistency)) {
    cond=consistency[i,'condition']
    tid=consistency[i, 'trial']
    count_vec<-preds %>% filter(group==cond, trial==tid) %>% pull(count)
    consistency[i, 'cronbach_alpha'] = get_cronbach_alpha(count_vec)
  }
  extra<-gen_labeled %>%
    select(condition, trial, total_diff, fix, fix_cond, rule_change, rule_change_cond)
  df<-consistency %>% left_join(extra, by=c('condition', 'trial'))
  return(df)
}
#######################

# Get model predictions
model_fits %>% arrange(raw_ll) %>% head()

get_model_cas(1, .1, .5) %>%
  ggplot(aes(x=total_diff, y=cronbach_alpha, color=condition)) +
  geom_point() +
  geom_smooth(method='lm', fill=NA) +
  scale_color_brewer(palette="Paired")

# Analysis
# Plot some canonical values
#params=data.frame(alpha=c(9, 1, 128, 1, 128), beta=c(256, .1, .1, 128, 128))
params=data.frame(alpha=rep(c(1,1024),each=2), beta=rep(c(0,1024), n=2))
params$param=paste0('alpha=', params$alpha, ',beta=', params$beta)

to_plot<-data.frame(
  alpha=numeric(0), beta=numeric(0), gamma=numeric(0), param=character(0),
  condition=character(0), trial=numeric(0), 
  cronbach_alpha=numeric(0), total_diff=numeric(0)
)
for (i in 1:nrow(params)) {
  alp=params[i, 'alpha']
  bet=params[i, 'beta']
  par=params[i, 'param']
  for (gam in GAMMAS) {
    df<-get_model_cas(alp, bet, gam) %>%
      mutate(alpha=alp, beta=bet, gamma=gam, param=par) %>%
      select(alpha, beta, gamma, param, condition, trial, cronbach_alpha, total_diff)
    to_plot<-rbind(to_plot, df)
  }
}

ggplot(to_plot, aes(x=total_diff, y=cronbach_alpha, color=condition)) +
  geom_point() +
  geom_smooth(method='lm', fill=NA) +
  scale_color_brewer(palette="Paired") +
  facet_grid(gamma~param) +
  ylim(0,1)

# Plot dpa dpr with alpha = 1024, beta = 0
rbind(
  read_model_preds(1024, 0, 1, '') %>% mutate(gamma=1),
  read_model_preds(1024, 0, 0, '') %>% mutate(gamma=0)
) %>%
  ggplot(aes(x=result, y=trial, fill=prob)) + geom_tile() +
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  facet_grid(gamma~group)


# What about non-dp model's cronbach_alpha
non_dp<-ce_preds %>%
  mutate(condition=group) %>%
  left_join(GROUP_SIZE, by='condition') %>%
  mutate(count=round(pred*n))

non_dp_consistency<-expand.grid(
  condition=paste0('A',1:4), trial=1:16, cronbach_alpha=NA, 
  stringsAsFactors = F
) %>% 
  arrange(condition, trial)

for (i in 1:nrow(non_dp_consistency)) {
  cond=non_dp_consistency[i,'condition']
  tid=non_dp_consistency[i, 'trial']
  count_vec<-non_dp %>% filter(group==cond, trial==tid) %>% pull(count)
  non_dp_consistency[i, 'cronbach_alpha'] = get_cronbach_alpha(count_vec)
}
extra<-gen_labeled %>%
  select(condition, trial, total_diff, fix, fix_cond, rule_change, rule_change_cond)
non_dp_consistency<-non_dp_consistency %>% left_join(extra, by=c('condition', 'trial'))


  















