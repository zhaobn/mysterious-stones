
library(dplyr)
source('shared.R')

# Setup & global vars
alpha=1 # CRP
beta=0.1 # Dirichlet
grouping='A' # 'A': agent-only, 'AR': agent-and-recipient
cond='A1'

learned_cats<-
  cbind(a1.a.cats%>%ungroup()%>%select(starts_with('X'))%>%mutate_all(as.character),
        a1.a.cats%>%ungroup()%>%select(n))%>%
  mutate(prob=normalize(n))
learned_funcs<-a1.a.funcs

tasks<-read.csv('../data/pilot_setup.csv')
gen_tasks<-tasks%>%filter(group==cond&phase=='gen')%>%select(agent, recipient)
learn_tasks<-tasks%>%filter(group==cond&phase=='learn')%>%select(agent, recipient)

n_gen_obs<-nrow(gen_tasks)
n_learn_obs<-nrow(learn_tasks)

# One c_i makes prediction for a gen task
tid<-1
cid<-1
pid<-1

group_pred<-function(tid, group_func, group_idx) {
  task<-as.list(gen_tasks[tid,])
  preds<-causal_mechanism(group_func, task)
  
  group_size<-length(group_idx)
  if (group_size==n_learn_obs) {
    return(preds)
  } else {
    crp<-group_size/(n_learn_obs-1+alpha)
    
    group_feats<-init_feat_dist(beta)
    for (i in group_idx) {
      obs_feats<-read_feature(as.list(task_obs[i,]), grouping)
      group_feats<-as.list(unlist(group_feats)+unlist(obs_feats))
    } 
    dir_ll<-Reduce('+', Map('*', read_feature(task, grouping), group_feats))/Reduce('+',group_feats)
    
    preds<-lapply(preds, function(x) x*crp*dir_ll)
    return(preds)
  }
}

# One cat prediction
tid<-1
cid<-1
cat_source<-learned_cats
func_source<-learned_funcs

cat_pred<-function(tid, cond_groups, func_source) {
  groups<-list()
  for (g in unique(cond_groups)) groups[[g]]<-which(cond_groups==g) # indices
  
  preds<-init_dist()
  for (i in 1:length(groups)) {
    group_func<-func_source[[names(groups)[i]]]; 
    group_idx<-groups[[i]]
    preds<-Map('+', preds, group_pred(tid, group_func, group_idx))
  }
  return(normalize(preds))
}


# All cats predictions
task_pred<-function(tid, cat_source, func_source){
  preds<-init_dist()
  for (i in 1:nrow(cat_source)) {
    cond_groups<-unlist(cat_source[i, seq(n_learn_obs)])
    cond_prob<-cat_source[i, 'prob']
    cat_preds<-Map('*', cat_pred(tid, cond_groups, func_source), cond_prob)
    preds<-Map('+', preds, cat_preds)
  }
  #return(normalize(preds)) # should be good
  return(preds)
}
task_pred(1, learned_cats, learned_funcs)

# All gen tasks

# Plots

















