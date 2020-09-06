
library(dplyr)
library(ggplot2)
library(viridis)
source('./shared.R')

# Get experiment data
tasks<-read.csv('../data/pilot_setup.csv')
fetch_task<-function(group_name, phase_name, trial_id, type='list', source=tasks) {
  task_data<-source%>%
    filter(group==group_name&phase==phase_name&trial==trial_id)%>%
    select(agent, recipient, result)
  if (type=='s') return(paste(task_data, collapse=',')) else return(as.list(task_data))
}

listify_task<-function(task_str) {
  if (typeof(task_str)=='list') return(task_str) else {
    task_els<-strsplit(task_str, ',')[[1]]
    if (task_els[3]=='NA') task_els[3]<-NA
    return(list(agent=task_els[1], recipient=task_els[2], result=task_els[3]))
  }
}


# Learning: get posterior
get_group_post<-function(group_name, source=hypos_grouped) {
  hypos<-source%>%select(hypo=shortest, prior)
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
init_result<-data.frame(group=character(0), phase=character(0), trial=numeric(0), pred=character(0), pred=numeric(0))

ueffect_preds<-init_result
for (i in seq(4)) {
  group_name=paste0('A', i)
  learned<-get_group_post(group_name, effects_grouped)
  prediction<-get_one_gen_pred(group_name, 1, learned)
  for (j in 2:5) {
    prediction<-rbind(prediction, get_one_gen_pred(group_name, j, learned))
  }
  ueffect_preds<-rbind(ueffect_preds, prediction)
}

comp_preds<-init_result
for (i in seq(4)) {
  group_name=paste0('A', i)
  learned<-get_group_post(group_name, hypos_grouped)
  prediction<-get_one_gen_pred(group_name, 1, learned)
  for (j in 2:5) {
    prediction<-rbind(prediction, get_one_gen_pred(group_name, j, learned))
  }
  comp_preds<-rbind(comp_preds, prediction)
}

# plot
ppt<-df.ppt%>%mutate(source='pilot')%>%select(group, trial, object, prob=freq, source)
ue<-ueffect_preds%>%mutate(source='uni_effects')%>%select(group, trial, object, prob=pred, source)
cp<-comp_preds%>%mutate(source='cause_effects')%>%select(group, trial, object, prob=pred, source)

df<-rbind(ppt, ue, cp)
df$source<-factor(df$source, levels=c('pilot', 'cause_effects', 'uni_effects'))
ggplot(df, aes(x=object, y=trial, fill=prob)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=unique(df$trial)) + 
  scale_fill_gradient(low='white', high='#293352') +
  #scale_fill_viridis(option="E", direction=-1) + 
  theme_linedraw() +
  facet_grid(source~group)

# Try universal effects on the new design
tasks<-read.csv('../data/trial_setup.csv')
new_preds<-init_result
for (i in seq(2)) {
  group_name=paste0('A', i)
  learned<-get_group_post(group_name, effects_grouped)
  prediction<-get_one_gen_pred(group_name, 1, learned)
  for (j in 2:5) prediction<-rbind(prediction, get_one_gen_pred(group_name, j, learned))
  new_preds<-rbind(new_preds, prediction)
}
ggplot(new_preds, aes(x=object, y=trial, fill=pred)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=unique(new_preds$trial)) + 
  scale_fill_gradient(low='white', high='#293352') +
  theme_linedraw() +
  facet_grid(~group)













