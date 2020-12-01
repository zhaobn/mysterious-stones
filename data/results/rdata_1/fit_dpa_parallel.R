
library(doMC)
cl<-registerDoMC(8)

alphas<-c(.01, .05, seq(.1, 1, by=.1), seq(2,5))
betas<-c(.0001, .001, .1, .11, .15, .2, .3, .4, .5, 1, 2)
set.seed(231)

grouping<-'A'
softmax_base<-''
drop<-500
slice<-1
iter<-10000

source('gibbs.R')
source('preds.R')
load('../data/mturk/mturk_main.Rdata')
load('hypos.Rdata')

tasks<-read.csv('../data/setup/main.csv')
n_learn_obs<-length(unique((tasks%>%filter(phase=='learn'))$task))
n_gen_obs<-length(unique((tasks%>%filter(phase=='gen'))$task))

default<-expand.grid(condition=paste0('A',1:4), trial=1:16, result=all_objects, stringsAsFactors=F)
counts<-df.tw %>%
  filter(phase=='gen'&grepl('gen', sid)) %>%
  mutate(trial=as.numeric(substr(sid,8,9)), result=as.character(result)) %>%
  select(ix, condition, trial, result) %>%
  arrange(condition, trial) %>%
  group_by(condition, trial, result) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  right_join(default, by=c('condition', 'trial', 'result')) %>%
  mutate(count=ifelse(is.na(count), 0, count)) %>%
  select(group=condition, trial, object=result, count)

# Get categories & make predictions
dpa_par_raw_preds<-foreach(a=alphas) %dopar% {
  foreach(b=betas) %dopar% {
    preds<-data.frame(
      group=character(0), trial=numeric(0), object=numeric(0), 
      prob=numeric(0), type=character(0)
    )
    for (c in 1:4) {
      cond<-paste0('A', c)
      x<-run_gibbs_sampler(cond, grouping, a, b, iter, F)
      cats<-read_cats(x[[1]], base=softmax_base, burn_in=drop, thinning=slice)
      func_preds<-prep_preds(x[[2]], cond)
      preds<-rbind(preds, 
                   get_cond_preds(cond, cats, func_preds, a, b, grouping))  
    }
    preds
  }
}

# Calculate likelihood & fit softmax
dpa_par_grid <- foreach(a=1:length(alphas), .combine = rbind) %dopar% {
  foreach(b=1:length(betas), .combine = rbind) %dopar% {
    vals<-list()
    vals[['alpha']]=alphas[a]
    vals[['beta']]=betas[b]
    
    preds<-dpa_raw_preds[[a]][[b]]
    data<-preds %>% left_join(counts, by=c('group', 'trial', 'object'))
    
    vals[['raw_ll']]<- (filter(data, count > 0) %>%
      summarise(sum(log(prob)*count)))[[1]]
    
    out<-optim(par=0, fn=data_likeli, data=data, method='Brent', lower=0, upper=100)
    vals[['fitted_base']]<-out$par
    vals[['fitted_ll']]<- -out$value

    vals
  }
}

save(dpa_par_raw_preds, dpa_par_grid, file='results/dpa_par.Rdata')
# # baseline
# 101*16*log(1/20) #-4841.103







