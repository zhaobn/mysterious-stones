
library(dplyr)

# Read data
load('../data/mturk/mturk_main.Rdata')
load('../models/models.Rdata')

# Prep data
ind_ndp_fits<-data.frame(ix=df.sw$ix, raw_ll=NA, base=NA, fitted_ll=NA)

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

ixes<-df.sw$ix
ppt_data<-get_ind_data(ixes[1])
for (i in 2:length(ixes)) ppt_data<-rbind(ppt_data, get_ind_data(ixes[i]))

get_rawll<-function(pix) {
  ppt<-ppt_data %>% filter(ix==pix)
  cond<-ppt$condition[1]
  ndp_preds<-ce_preds %>% filter(group==cond) %>% select(trial, result=object, prob=pred)
  ppt<-ppt %>% left_join(ndp_preds, by=c('trial', 'result'))
  return(sum(log(ppt$prob)*ppt$c))
}

softmax_trial<-function(td, t) {
  td$to_exp<-exp(td$prob*t)
  td$soft<-td$to_exp/sum(td$to_exp)
  return(select(td, trial, result, prob, soft, c))
}


# Run fits
fit_ndp<-function(pix, b) {
  ppt<-ppt_data %>% filter(ix==pix)
  cond<-ppt$condition[1]
  ndp_preds<-ce_preds %>% filter(group==cond) %>% select(trial, result=object, prob=pred)
  ppt<-ppt %>% left_join(ndp_preds, by=c('trial', 'result'))
  ppt<-softmax_trial(ppt, b)
  return(-sum(log(ppt$soft)*ppt$c))
}

for (i in 1:length(ixes)) {
  ix<-ixes[i]
  out<-optim(par=0, fn=fit_ndp, pix=ix, method='Brent', lower=-100, upper=100)
  ind_ndp_fits[i, 'raw_ll']<-get_rawll(ix)
  ind_ndp_fits[i, 'base']<-out$par
  ind_ndp_fits[i, 'fitted_ll']<-out$value * -1
}

save(ind_ndp_fits, file='ind_ndp_fits.Rdata')













