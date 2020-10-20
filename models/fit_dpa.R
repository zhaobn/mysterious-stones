
grouping<-'A'
softmax_base<-''
drop<-500
slice<-1

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
  mutate(count=ifelse(is.na(count), 0, count), object=as.numeric(result)) %>%
  select(group=condition, trial, object, count)

fit_dp<-function(alpha, beta) {
  preds<-data.frame(group=character(0),
                    trial=numeric(0),
                    object=numeric(0),
                    prob=numeric(0),
                    type=character(0))
  # Get categories & make predictions
  start_time<-Sys.time()
  for (c in 1:4) {
    cond<-paste0('A', c)
    x<-run_gibbs_sampler(cond, grouping, alpha, beta, 10000, F)
    cats<-read_cats(x[[1]], base=softmax_base, burn_in=drop, thinning=slice)
    func_preds<-prep_preds(x[[2]], cond)
    preds<-rbind(preds, 
                 get_cond_preds(cond, cats, func_preds, alpha, beta, grouping))  
  }
  preds$object<-as.numeric(as.character(preds$object))
  end_time<-Sys.time()
  print(end_time - start_time)
  
  # Calc loglikelihood
  data<-counts %>% 
    filter(count > 0) %>%
    left_join(preds, by=c('group', 'trial', 'object'))
  ll<-sum(log(data$prob) * data$count)
  print(ll)
  return(preds)
}
t<-fit_dp(1, 1/9)
     







