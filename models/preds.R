
source('shared.R')
tasks<-read.csv('../data/pilot_setup.csv')
n_learn_obs<-length(unique((tasks%>%filter(phase=='learn'))$trial))
n_gen_obs<-length(unique((tasks%>%filter(phase=='gen'))$trial))

# Helper functions ####
read_cats<-function(states_source) {
  df<-data.frame(matrix(unlist(states_source), nrow=length(states_source), byrow=T))
  # Filter out first 500 samples
  df$i<-seq(nrow(df))
  df<-df%>%filter(i>500)
  # Get unique categories
  df<-df%>%group_by(X1, X2, X3, X4, X5, X6)%>%summarise(n=n())%>%ungroup()
  cats<-cbind(df%>%select(starts_with('X'))%>%mutate_all(as.character), 
              df%>%select(n))%>%
    mutate(prob=normalize(n))
  return(cats)
}
# One c_i makes prediction for a gen task
pred_by_group<-function(cond, tid, group_func, group_idx, alpha, beta, grouping) {
  learn_tasks<-tasks%>%filter(group==cond&phase=='learn')%>%select(agent, recipient)
  gen_tasks<-tasks%>%filter(group==cond&phase=='gen')%>%select(agent, recipient)
  
  task<-as.list(gen_tasks[tid,])
  preds<-causal_mechanism(group_func, task)
  
  group_size<-length(group_idx)
  if (group_size==n_learn_obs) {
    return(preds)
  } else {
    crp<-group_size/(n_learn_obs-1+alpha)
    
    group_feats<-init_feat_dist(beta)
    for (i in group_idx) {
      obs_feats<-read_feature(as.list(learn_tasks[i,]), grouping)
      group_feats<-as.list(unlist(group_feats)+unlist(obs_feats))
    } 
    dir_ll<-Reduce('+', Map('*', read_feature(task, grouping), group_feats))/Reduce('+',group_feats)
    
    preds<-lapply(preds, function(x) x*crp*dir_ll)
    return(preds)
  }
}
# One cat prediction
pred_by_cat<-function(cond, tid, cond_groups, func_source, alpha, beta, grouping) {
  groups<-list()
  for (g in unique(cond_groups)) groups[[g]]<-which(cond_groups==g) # indices
  
  preds<-init_dist()
  for (i in 1:length(groups)) {
    group_func<-func_source[[names(groups)[i]]]; 
    group_idx<-groups[[i]]
    preds<-Map('+', preds, 
               pred_by_group(cond, tid, group_func, group_idx, alpha, beta, grouping))
  }
  return(normalize(preds))
}
# All cats predictions
pred_by_task<-function(cond, tid, cat_source, func_source, alpha, beta, grouping){
  preds<-init_dist()
  for (i in 1:nrow(cat_source)) {
    cond_groups<-unlist(cat_source[i, seq(n_learn_obs)])
    cond_prob<-cat_source[i, 'prob']
    cat_preds<-Map('*', 
                   pred_by_cat(cond, tid, cond_groups, func_source, alpha, beta, grouping), 
                   cond_prob)
    preds<-Map('+', preds, cat_preds)
  }
  #return(normalize(preds)) # should be good
  return(preds)
}

# Get predictions dataframe for a condition
get_cond_preds<-function(cond, states, funcs, alpha, beta, grouping, base='') {
  # Format prediction for one task into a dataframe
  format_task_preds<-function(tid, cat_source, func_source) {
    x<-pred_by_task(cond, tid, cat_source, func_source, alpha, beta, grouping)
    df<-data.frame(object=names(x), prob=unlist(x)); rownames(df)<-c()
    df<-df%>%mutate(object=as.character(object))%>%
      mutate(group=cond, trial=tid, type=grouping)%>%
      select(group, trial, object, prob, type)
    return(df)
  }
  
  learned_cats<-read_cats(states)
  learned_funcs<-funcs
  if (base!='') learned_cats$prob<-softmax(learned_cats$prob, base)
  
  df<-format_task_preds(1, learned_cats, learned_funcs)
  for (i in 2:n_gen_obs) df<-rbind(df, format_task_preds(i, learned_cats, learned_funcs))
  
  return(df)
}




  
  
  
















