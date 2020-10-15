
library(tidyverse)
library(viridis)
source('./shared.R')

# Get experiment data
tasks<-read.csv('../data/setup/main.csv')
fetch_task<-function(group_name, phase_name, trial_id, type='list', source=tasks) {
  task_data<-source%>%
    filter(condition==group_name&phase==phase_name&task==trial_id)%>%
    select(agent, recipient, result)
  if (type=='s') return(paste(task_data, collapse=',')) else return(as.list(task_data))
}

listify_task<-function(task_str) {
  if (typeof(task_str)=='list') return(task_str) else {
    task_els<-strsplit(task_str, ',')[[1]]
    if (task_els[3]==0) task_els[3]<-NA
    return(list(agent=task_els[1], recipient=task_els[2], result=task_els[3]))
  }
}

# Learning: get posterior
get_group_post<-function(group_name, source=df.hypos) {
  hypos<-source%>%select(hypo, prior)
  final_col<-paste0('post_', group_name)
  for (i in seq(6)) {
    data<-fetch_task(group_name, 'learn', i, 's')
    prior_col<-if (i==1) 'prior' else paste0('post_', i-1)
    likeli_col<-paste0('likeli_',i)
    post_col<-paste0('post_',i)
    hypos[,likeli_col]<-mapply(get_likeli, hypos$hypo, rep(data, nrow(hypos)))
    hypos[,post_col]<-hypos[,prior_col]*hypos[,likeli_col]
    hypos[,post_col]<-normalize(hypos[,post_col])
  }
  hypos[,final_col]<-hypos$post_6
  return(hypos[,c('hypo', final_col)])
}
df.post<-get_group_post('A1')
for (i in 2:4) {
  post<-get_group_post(paste0('A',i))
  df.post<-df.post%>%left_join(post,by='hypo')
}
df.hypos<-df.hypos%>%left_join(df.post, by='hypo')
save(df.hypos, file='hypos.Rdata')

# Generalization predictions
get_one_gen_pred<-function(group_name, trial_id, learn_post) {
  data<-fetch_task(group_name, 'gen', trial_id)
  post_col<-paste0('post_', group_name)
  df<-learn_post[(learn_post[,post_col]>0),]
  result<-unlist(init_dist())
  for (i in 1:nrow(df)) {
    result<-result+(unlist(causal_mechanism(df$hypo[i], data)))*df[i, post_col]
  }
  return(data.frame(group=group_name, phase='gen', trial=trial_id, 
                    object=all_objects, pred=normalize(result)))
}

# Put them together
effects_grouped=df.effects.grouped
init_result<-data.frame(group=character(0), phase=character(0), trial=numeric(0), pred=character(0), pred=numeric(0))

ce_preds<-init_result
for (i in 2:4) {
  group_name=paste0('A', i)
  learned<-df.hypos[,c('hypo', paste0('post_', group_name))]
  prediction<-get_one_gen_pred(group_name, 1, learned)
  for (j in 2:16) {
    prediction<-rbind(prediction, get_one_gen_pred(group_name, j, learned))
  }
  ce_preds<-rbind(ce_preds, prediction)
}
ce_preds<-ce_preds%>%mutate(source='causal_grouped')%>%select(group, trial, object, prob=pred, source)
save(ce_preds, file='models.Rdata')

# Sanity checks with non-grouped uni-effects
x<-unique(c(unlist(effects_grouped$shortest), unlist(effects_grouped$hypos)))
effects_ungrouped<-data.frame(hypo=x)
effects_ungrouped$hypo<-as.character(effects_ungrouped$hypo)
effects_ungrouped<-effects_ungrouped%>%filter(!(hypo==''))

effects_ungrouped$prior_raw<-mapply(pcfg_prior, effects_ungrouped$hypo)
effects_ungrouped$prior<-normalize(effects_ungrouped$prior_raw)

ug_preds<-init_result
for (i in seq(4)) {
  group_name=paste0('A', i)
  learned<-get_group_post(group_name, effects_ungrouped)
  prediction<-get_one_gen_pred(group_name, 1, learned)
  for (j in 2:5) {
    prediction<-rbind(prediction, get_one_gen_pred(group_name, j, learned))
  }
  ug_preds<-rbind(ug_preds, prediction)
}
ug_preds<-ug_preds%>%mutate(source='un_grouped')%>%select(group, trial, object, prob=pred, source)














