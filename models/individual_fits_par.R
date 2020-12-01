
# Libraries
library(dplyr)
library(foreach)
library(doMC)
cl<-registerDoMC(2)

# Data
load('../data/mturk/mturk_main.Rdata')
load('hdp.Rdata')

# Setup
ixes<-df.sw$ix
ids<-filter(model_refs, gamma==0|gamma==1)$id

# Prep individual data
feature_setting<-list()
feature_setting[['edges']]<-seq(3,7)
feature_setting[['shades']]<-seq(4)

all_objects<-vector()
for (e in feature_setting$edges) {
  for (s in feature_setting$shades) {
    all_objects<-c(all_objects, paste(c(e,s), collapse=''))
  }
}

get_ind_data<-function(pix) {
  data<-expand.grid(trial=1:16, result=as.character(all_objects), stringsAsFactors=F)
  cond<-filter(df.sw, ix==pix)[,'condition']
  ppt_data<-df.tw %>%
    filter(ix==pix, phase=='gen', grepl('gen', sid)) %>%
    mutate(trial=as.numeric(substr(sid,8,9)), result=as.character(result), c=1) %>%
    select(ix, condition, trial, result, c) %>%
    full_join(data, by=c('trial', 'result')) %>%
    mutate(ix=pix, condition=cond, c=ifelse(is.na(c), 0, c)) %>%
    arrange(trial, result)
  return(ppt_data)
}
ppt_data<-get_ind_data(ixes[1])
for (i in 2:length(ixes)) ppt_data<-rbind(ppt_data, get_ind_data(ixes[i]))

# Run individual fits
softmax_trial<-function(td, t) {
  td$to_exp<-exp(td$prob*t)
  td$soft<-td$to_exp/sum(td$to_exp)
  return(select(td, trial, result, prob, soft, c))
}
fit_ind<-function(pix, id) {
  ppt<-ppt_data %>% filter(ix==pix)
  preds<-model_preds[[id]] %>% filter(group==ppt$condition[1])
  to_fit<-preds %>%
    select(trial, result=object, prob) %>%
    left_join(ppt, by=c('trial', 'result'))
  raw_ll<-sum(log(to_fit$prob)*to_fit$c)
  fit_me<-function(t, data) {
    # apply softmax per trial
    s<-softmax_trial(filter(data, trial==1), t)
    for (i in 2:16) {
      s<-rbind(s, softmax_trial(filter(data, trial==i), t))
    }
    s<-s %>% filter(c>0) 
    return(-sum(log(s$soft)))
  }
  out<-optim(par=0, fn=fit_me, data=to_fit, method='Brent', lower=0, upper=1000)
  return(list(raw_ll=raw_ll, base=out$par, fitted_ll=-out$value))
}


# individual_all_fits_par<-list()
# individual_all_fits_par<-foreach(i=1:nrow(df.sw)) %:%
#   foreach(j=1:nrow(model_refs)) %dopar% {
#     fitted<-fit_ind(df.sw$ix[i], j)
#     c(as.list(model_refs[j,]), fitted)
#   }
# 
# 
# save(individual_all_fits_par, file='individual_all_fits_par.Rdata')


# save only the best fits
comb<-function(d1, d2) if (d1$fitted_ll > d2$fitted_ll) d1 else d2
opts<-list(chunkSize=2)

ind<-list()
ind<-foreach(i=1:length(ixes), .combine='rbind', .options.nws=opts) %:%
  foreach(j=1:length(ids), .combine='comb', .inorder=FALSE) %dopar% {
    params<-as.list(model_refs[ids[j],])
    fitted<-fit_ind(ixes[i], ids[j])
    data.frame(c(params, fitted))
  }
save(ind, file='../results/ind_best_fits_par.Rdata')

# test=vector('list', length = 2)
# test[[1]]<-list(a=1,b=2,l=100)
# test[[2]]<-list(a=10,b=1,l=80)
# data.frame(do.call(rbind, test))
