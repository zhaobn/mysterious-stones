
library(dplyr)
source('./shared.R')

# Get experiment data
tasks<-read.csv('../data/trial_setup.csv')
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
    data<-fetch_task('A1', 'learn', i, 's')
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


# Test for A1
learn_a1<-get_group_post('A1')
gen_preds<-get_one_gen_pred('A1', 1, learn_a1)
for (i in seq(5)) gen_preds<-rbind(gen_preds, get_one_gen_pred('A1', i, learn_a1))

# Plot
library(ggplot2)
library(viridis)
ggplot(uni_preds, aes(x=object, y=trial, fill=pred)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=unique(uni_preds$trial)) + 
  scale_fill_viridis(option="E", direction=-1) 
# + facet_grid(data~learn_cond)

# try for universal effects
effects_a1<-get_group_post('A1', effects_grouped)
uni_preds<-get_one_gen_pred('A1', 1, effects_a1)
for (i in seq(5)) uni_preds<-rbind(uni_preds, get_one_gen_pred('A1', i, effects_a1))






