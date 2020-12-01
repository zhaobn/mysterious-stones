
# Libraries
library(dplyr)

# Data
load('../data/mturk/mturk_main.Rdata')
load('../data/hdp.Rdata')
load('../data/results/rdata_3/individual_best_fits.Rdata')
load('../data/results/rdata_3/individual_all_fits.Rdata')

# Setup
ixes<-df.sw$ix
ids<-(filter(model_refs, gamma>0&gamma<1))[,'id']

# individual_best_fits<-list()
# for (i in 1:nrow(df.sw)) {
#   ix<-paste0('p', as.character(df.sw$ix[[i]]))
#   individual_best_fits[[ix]]<-list(fitted_ll=-Inf)
# }
# 
# individual_all_fits<-list()
# for (i in 1:nrow(df.sw)) {
#   ix<-paste0('p', as.character(df.sw$ix[[i]]))
#   params<-vector('list', length=nrow(model_refs))
#   individual_all_fits[[ix]]<-params
# }

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

n=1
total=length(ixes)*length(ids)
for (i in 1:length(ixes)) {
  for (j in 1:length(ids)) {
    id<-ids[j]
    params<-as.list(model_refs[id,])
    log<-c(list(n=n, perc=paste0(round(n*100/total,2),'%'), nppt=i, run=j), params)
    write.csv(params, file='individual_fits_current')
    fitted<-fit_ind(ixes[i], id)
    individual_all_fits[[i]][[id]]<-c(params, fitted)
    # Store current best fit
    if (fitted[['fitted_ll']]>individual_best_fits[[i]][['fitted_ll']]) {
      individual_best_fits[[i]]<-individual_all_fits[[i]][[id]]
    }
    save(individual_all_fits, file='individual_all_fits.Rdata')
    save(individual_best_fits, file='individual_best_fits.Rdata')
    n=n+1
  }
}


# test=vector('list', length = 2)
# test[[1]]<-list(a=1,b=2,l=100)
# test[[2]]<-list(a=10,b=1,l=80)
# data.frame(do.call(rbind, test))
