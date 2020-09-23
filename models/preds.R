
source('shared.R')
tasks<-read.csv('../data/pilot_setup.csv')
n_learn_obs<-length(unique((tasks%>%filter(phase=='learn'))$trial))
n_gen_obs<-length(unique((tasks%>%filter(phase=='gen'))$trial))

# Helpers ####
read_cats<-function(states_source, burn_in=0, thinning=1, base='') {
  df<-data.frame(matrix(unlist(states_source), nrow=length(states_source), byrow=T))
  # Burn in: filter out first n samples
  df$i<-seq(nrow(df))
  df<-df%>%filter(i>burn_in)
  # Thinning: keep very nth sample
  df$i<-seq(nrow(df))
  df<-df%>%filter(i%%thinning==0)
  # Get unique categories
  df<-df%>%group_by(X1, X2, X3, X4, X5, X6)%>%summarise(n=n())%>%ungroup()
  cats<-cbind(df%>%select(starts_with('X'))%>%mutate_all(as.character), 
              df%>%select(n))%>%
    mutate(prob=normalize(n))
  # Apply softmax
  if (base!='') cats$prob<-softmax(cats$prob, base)
  return(cats)
}
prep_preds<-function(funcs, cond) {
  preds<-list()
  for (f in names(funcs)) {
    preds[[f]]<-list()
    for (d in 1:n_gen_obs) {
      data<-as.list(tasks%>%filter(group==cond&phase=='gen'&trial==d)%>%select(agent, recipient))
      preds[[f]][[d]]<-causal_mechanism(funcs[[f]], data)
    }
  }
  return(preds)
}

# Get predictions dataframe for a condition
get_cond_preds<-function(cond, learned_cats, func_preds, alpha, beta, grouping) {
  # Shared values
  learn_tasks<-tasks%>%filter(group==cond&phase=='learn')%>%select(agent, recipient)
  gen_tasks<-tasks%>%filter(group==cond&phase=='gen')%>%select(agent, recipient)
  
  # Functions to get predictions
  # One c_i makes prediction for a gen task
  pred_by_group<-function(tid, group_func, group_idx) {
    preds<-func_preds[[group_func]][[tid]]
    group_size<-length(group_idx)
    task<-as.list(gen_tasks[tid,])
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
  pred_by_cat<-function(tid, cond_groups) {
    groups<-list()
    for (g in unique(cond_groups)) groups[[g]]<-which(cond_groups==g) # indices
    preds<-init_dist()
    for (i in 1:length(groups)) {
      group_func<-names(groups)[i]; 
      group_idx<-groups[[i]]
      preds<-Map('+', preds, pred_by_group(tid, group_func, group_idx))
    }
    return(normalize(preds))
  }
  # All cats predictions
  pred_by_task<-function(tid){
    preds<-init_dist()
    for (i in 1:nrow(learned_cats)) {
      cond_groups<-unlist(learned_cats[i, seq(n_learn_obs)])
      cond_prob<-learned_cats[i, 'prob']
      cat_preds<-Map('*', pred_by_cat(tid, cond_groups), cond_prob)
      preds<-Map('+', preds, cat_preds)
    }
    #return(normalize(preds)) # should be good
    return(preds)
  }
  # Format prediction for one task into a dataframe
  format_task_preds<-function(tid) {
    x<-pred_by_task(tid)
    df<-data.frame(object=names(x), prob=unlist(x)); rownames(df)<-c()
    df<-df%>%mutate(object=as.character(object))%>%
      mutate(group=cond, trial=tid, type=grouping)%>%
      select(group, trial, object, prob, type)
    return(df)
  }
  
  
  # Get predictions
  df<-format_task_preds(1)
  for (i in 2:n_gen_obs) df<-rbind(df, format_task_preds(i))
  
  return(df)
}

# cats<-read_cats(x[[1]], 200, 5)
# func_preds<-prep_preds(x[[2]], 'A1')
# y<-get_cond_preds("A1", cats, func_preds, 1, .1, 'A')
  
  















