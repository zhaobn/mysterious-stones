
source('shared.R')
tasks<-read.csv('../data/pilot_setup.csv')
load('../data/effects_grouped.Rdata') # hypothesis

run_gibbs_sampler<-function(cond, grouping, alpha, beta, limit, logging=T, hypo_source=df.effects.posts) {
  # Task setup
  task_obs<-tasks%>%filter(group==cond&phase=='learn')%>%select(agent, recipient, result)
  hypos<-hypo_source%>%filter(group==cond)
  #non_empty<-hypos%>%filter(post_1>0&post_2>0&post_3>0&post_4>0&post_5>0&post_6>0)

  # Pre-calculated values
  nobs<-nrow(task_obs)
  join_new<-alpha/(nobs-1+alpha)
  feats<-list()
  for (i in seq(nobs)) feats[[i]]<-read_feature(as.list(task_obs[i,]), grouping)
  # Helper function that calculates mean-feature similarity using feats
  dir_likeli<-function(ob_idx, cat_obs_idx) {
    cat_feat<-init_feat_dist(beta)
    for (i in cat_obs_idx) cat_feat<-Map('+', cat_feat, feats[[i]])
    found<-Map('*', cat_feat, feats[[ob_idx]])
    return(Reduce('+', found)/Reduce('+', cat_feat))
  }
  
  # initialize
  states<-list()
  func_refs<-list()
  print(paste0('Start sampling for ', limit, ' iterations:',
               'of ', cond, ' grouped by ', grouping))
  start_sampler<-Sys.time()
  
  # Initalization
  state<-rep('c1', nobs)
  func_refs[['c1']]<-sample(hypos$hypo, 1, prob=hypos$prior)
  # state<-paste0('c', seq(nobs))
  # funcs<-sample(non_empty$hypo, nobs, prob=non_empty$prior, replace=F)
  # for (i in seq(nobs)) func_refs[[state[i]]]<-funcs[i]

  n<-1
  while (n<limit) {
    # Choose which one to update
    probs<-sapply(seq(nobs), function(i) {
      obs<-which(state==state[i])
      crp<-if (length(obs)==nobs) 1 else length(obs)/(nobs-1+alpha)
      catdir<-dir_likeli(i, obs)
      likeli<-get_likeli(func_refs[[state[i]]], as.list(task_obs[i,]))
      return(crp*catdir*likeli)
    })
    if (Reduce('+', probs)==0) probs<-rep(1, nobs) else probs<-1/exp(probs)
    
    #to_update<-sample(seq(nobs), 1) # index
    to_update<-sample(seq(nobs), 1, prob=probs)
    obs_to_update<-as.list(task_obs[to_update,])
    self_resemblance<-dir_likeli(to_update, to_update)
    
    other_idx<-setdiff(seq(nobs), to_update)
    
    # sample new function(s) for this obs from conditional probablity
    post_col<-paste0('post_',to_update)
    funcs_pool<-hypos$hypo[which(hypos[,post_col]>0)]
    funcs_post<-hypos[which(hypos[,post_col]>0), post_col]
    
    new_funcs<-sample(funcs_pool, 1, prob=funcs_post)
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
      if (!(cat %in% state)) new_cats<-c(new_cats, cat)
    }
    # treat single-unique existing to-update cat as joining a new cat
    if (!(state[to_update] %in% state[other_idx])) {
      new_cats<-c(new_cats, state[to_update])
    }
    new_cats<-unique(new_cats)
    
    # Check for existing categories
    propto<-list()
    for (s in unique(state[other_idx])) {
      # Chinese restaurant process
      join_this<-length(which(state[other_idx]==s))/(nobs-1+alpha)
      # Dirichlet on feature similarity
      resemblance<-dir_likeli(to_update, setdiff(which(state==s), to_update))
      # Causal function
      likeli<-get_likeli(func_refs[[s]], obs_to_update)
      # Put together
      propto[[s]]<-join_this*resemblance*likeli
    }
    # Or assign new one(s)
    if (length(new_cats)>0) {
      for (cat in new_cats) {
        post_likeli<-get_likeli(func_refs[[cat]], obs_to_update)
        propto[[cat]]<-join_new*self_resemblance*post_likeli
      } 
    }
    # Filter out zero probs to avoid sample() error
    t<-propto[unlist(lapply(propto, function(x) x>0))]
    t<-normalize(t)
    # Sample new category
    sampled<-if (length(t)==1) names(t)[1] else sample(names(t), 1, prob=unlist(t))
    state[to_update]<-sampled
    if (logging) print(paste0(n, ': ', 'sampling ', to_update, ' | ', 
                              paste(state, collapse=',')))
    # Save everything for developing
    # Play with burn-in and thinning in the pred.R script
    # For the final version do built-in burn-in and thinning here
    states[[n]]<-state
    # Go to the next iteration
    n<-n+1
  }
  done_sampler<-Sys.time()
  print(done_sampler-start_sampler)

  return(list(state=states, funcs=func_refs))
}

# x<-run_gibbs_sampler('A1', 'AR', 1, .1, 1000, T)
# df<-data.frame(matrix(unlist(x[[1]]), nrow=length(x[[1]]), byrow=T))
# df<-df%>%group_by(X1, X2, X3, X4, X5, X6)%>%summarise(n=n())%>%ungroup()









