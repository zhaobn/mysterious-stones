
source('shared.R')
tasks<-read.csv('../data/pilot_setup.csv')
load('../data/effects_grouped.Rdata') # hypothesis

find_func_ref<-function(func, func_ref) {
  
}

run_gibbs_sampler<-function(cond, grouping, alpha, beta, limit, logging=T, hypo_source=df.effects.posts) {
  task_obs<-tasks%>%filter(group==cond&phase=='learn')%>%select(agent, recipient, result)
  nobs<-nrow(task_obs)
  
  hypos<-hypo_source%>%filter(group==cond)
  # non_empty<-hypos%>%filter(post_1>0&post_2>0&post_3>0&post_4>0&post_5>0&post_6>0)
  
  # initialize
  states<-list()
  func_refs<-list()
  print(paste0('Start sampling for ', limit, ' iterations:',
               'of ', cond, ' grouped by ', grouping))
  start_sampler<-Sys.time()
  # Initalization
  state<-rep('c1', nobs)
  func_refs[['c1']]<-sample(hypos$hypo, 1, prob=hypos$prior)
  
  # Pre-calculated values
  join_new<-alpha/(nobs-1+alpha)
  
  # # Debugging tests
  # state=c(paste0('c', seq(5)), 'c2')
  # for (i in 1:5) {
  #   f_col<-paste0('post_',i)
  #   funcs<-hypos[,'hypo'][hypos[,f_col]==max(hypos[,f_col])]
  #   func_refs[[state[i]]]<-sample(funcs, 1)
  # }
  # state<-paste0('c', seq(nobs))
  # funcs<-sample(non_empty$hypo, nobs, prob=non_empty$prior, replace=F)
  # for (i in seq(nobs)) func_refs[[state[i]]]<-funcs[i]
  
  n<-1
  while (n<limit) {
    # sample 1 obs
    to_update<-sample(seq(nobs), 1) # index
    obs_to_update<-as.list(task_obs[to_update,])
    obs_feats<-read_feature(obs_to_update, grouping) 
    
    cat_feat<-Map('+', obs_feats, init_feat_dist(beta))
    self_resemblance<-sum(unlist(cat_feat)*unlist(obs_feats))/sum(unlist(cat_feat))
    
    # sample new function(s) for this obs from conditional probablity
    post_col<-paste0('post_',to_update)
    funcs_pool<-hypos$hypo[which(hypos[,post_col]>0)]
    funcs_post<-hypos[which(hypos[,post_col]>0), post_col]
    
    new_funcs<-sample(funcs_pool, 3, prob=funcs_post)
    new_cats<-c()
    # whether the sampled func belongs to an existing category
    for (f in unique(new_funcs)) {
      checks<-sapply(func_refs, function(x) x==f)
      if (T %in% checks) {
        cat<-names(func_refs)[which(checks)]
      } else {
        cat<-paste0('c', length(func_refs)+1)
        func_refs[[cat]]<-f
      }
      new_cats<-c(new_cats, cat)
    }

    # Check for existing categories
    propto<-list()
    for (s in unique(state)) {
      #ob_indx<-other_idx[which(state==s)]
      ob_indx<-which(state==s)
      # Chinese restaurant process
      join_this<-length(ob_indx)/(nobs-1+alpha)
      # Dirichlet on feature similarity
      cat_feat<-init_feat_dist(beta)
      for (i in ob_indx) cat_feat<-Map('+', cat_feat, read_feature(as.list(task_obs[i,]), grouping))
      resemblance<-sum(unlist(cat_feat)*unlist(obs_feats))/sum(unlist(cat_feat))
      # Causal power
      likeli<-get_likeli(func_refs[[s]], obs_to_update)
      # Put together
      propto[[s]]<-join_this*resemblance*likeli
    }
    # Or assign a new one
    
    for (cat in new_cats) {
      if (!(cat %in% state)) {
        post_likeli<-get_likeli(func_refs[[cat]], obs_to_update)
        propto[[cat]]<-join_new*self_resemblance*post_likeli
      } 
    }
    # Sample new category
    state[to_update]<-sample(names(propto), 1, prob=unlist(propto))
    # ll<-1
    # for (i in seq(nobs)) ll<-ll*get_likeli(func_refs[[state[i]]], as.list(task_obs[i,]))
    # print(paste0(n, ': ', paste(state, collapse=','), ' | likeli: ', round(ll,4)))
    if (logging) print(paste0(n, ': ', 'sampled ', cat, ' for ', to_update,
                              ' | ', paste(state, collapse=',')))
    states[[n]]<-state
    # Go to the next iteration
    n<-n+1
  }
  done_sampler<-Sys.time()
  print(done_sampler-start_sampler)

  return(list(state=states, funcs=func_refs))
}

#x<-run_gibbs_sampler('A1', 'A', 1, .1, 100, T)










