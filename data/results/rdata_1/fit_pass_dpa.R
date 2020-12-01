
alphas<-c(.01, .05, seq(.1, 1, by=.1), seq(2, 5))
betas<-c(.0001, .001, .1, .11, .15, .2, .3, .4, .5, 1, 2)
set.seed(231)

grouping<-'A'
softmax_base<-''
drop<-500
slice<-1
iter<-10000

source('gibbs.R')
source('preds.R')
load('data/hypos.Rdata')
load('data/mturk_pass.Rdata')

df.sw<-df.sw.pass
df.tw<-df.tw.pass

tasks<-read.csv('data/main.csv')
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


# Set up result objects
dpa_pass_raw_preds<-list()
dpa_pass_grid<-data.frame(
  id=numeric(0), alpha=numeric(0), beta=numeric(0),
  raw_ll=numeric(0), fitted_base=numeric(0), fitted_ll=numeric(0)
)

n=1
for (a in alphas) {
  for (b in betas) {
    dpa_pass_grid[n,'id']<-n
    dpa_pass_grid[n,'alpha']<-a
    dpa_pass_grid[n,'beta']<-b
    
    log<-data.frame(nth=n, alpha=a, beta=b)
    write.csv(log, file='dpa_current')
    
    # Run Gibbs sampler with given parameters
    preds<-data.frame(
      group=character(0), trial=numeric(0), object=numeric(0), 
      prob=numeric(0), type=character(0)
    )
    # Get categories & make predictions
    for (c in 1:4) {
      cond<-paste0('A', c)
      x<-run_gibbs_sampler(cond, grouping, a, b, iter, F)
      cats<-read_cats(x[[1]], base=softmax_base, burn_in=drop, thinning=slice)
      func_preds<-prep_preds(x[[2]], cond)
      preds<-rbind(preds, 
                   get_cond_preds(cond, cats, func_preds, a, b, grouping))  
    }
    dpa_pass_raw_preds[[n]]<-preds

    # Compute (log) likelihoods
    data<-preds %>% left_join(counts, by=c('group', 'trial', 'object'))
    # Log likelihood
    raw_ll<-data %>%
      filter(count > 0) %>%
      summarise(sum(log(prob)*count))
    dpa_pass_grid[n,'raw_ll']<-raw_ll
    
    # Fit log-likelihood
    data_likeli<-function(b) {
      ds<-filter(data, group=='A1', trial==1) %>% mutate(softmaxed=softmax(prob, b))
      for (c in 1:4) {
        for (i in 1:16) {
          if (!(c==1 & i==1)) {
            ds<-rbind(ds,
                      filter(data, group==paste0('A', c), trial==i) %>% 
                        mutate(softmaxed=softmax(prob, b)))
          }
        }
      }
      ds<-ds%>%filter(count>0)
      ll<-sum(ds$count*log(ds$softmaxed))
      return(-ll)
    }
    
    out<-optim(par=0, fn=data_likeli, method='Brent', lower=0, upper=100)
    dpa_pass_grid[n, 'fitted_base']<-out$par
    dpa_pass_grid[n, 'fitted_ll']<-out$value

    save(dpa_pass_grid, dpa_pass_raw_preds, file='results/dpa_pass_seq.Rdata')
    n=n+1
  }
}
     
# # baseline
# 101*16*log(1/20) #-4841.103
# # sanity check with fake data
# data<-data%>%mutate(count=round(prob*30))

# 53*24*log(1/20) #-3810.571





