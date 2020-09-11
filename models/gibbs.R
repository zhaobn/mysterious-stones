
library(dplyr)
source('shared.R')

# Setup & global vars
alpha=0.5 # CRP
beta=0.1 # Dirichlet
grouping='A' # 'A': agent-only, 'AR': agent-and-recipient
cond='A2'

tasks<-read.csv('../data/pilot_setup.csv')
task_obs<-tasks%>%filter(group==cond&phase=='learn')%>%select(agent, recipient, result)
nobs<-nrow(task_obs)

hypos<-effects_grouped%>%select(hypo=shortest, prior)
for (i in seq(nobs)) {
  d<-paste(task_obs[i,], collapse=',')
  hypos[,paste0('post_',i)]<-mapply(get_likeli, hypos$hypo, rep(d, nrow(hypos)))
}

# Sampler
# initialize
states<-list()
categories<-list()
func_refs<-list()

#state=c('c1', 'c1', 'c2', 'c2', 'c3', 'c4') 
state<-paste0('c',seq(nobs))
for (s in state) {
  func_refs[[s]]<-sample(hypos$hypo, 1, prob=hypos$prior)
  categories[[s]]<-list()
  categories[[s]][[as.character(0)]]<-which(state==s)
} 

n<-1
limit<-2000
while (n<limit) {
  # sample 1 obs
  to_update<-sample(seq(nobs), 1) # index
  obs_feats<-read_feature(as.list(task_obs[to_update,]), grouping)
  
  # sample a function for this obs wrt posterior
  posterior<-hypos[,paste0('post_',to_update)]
  new_func<-sample(hypos$hypo, 1, prob=posterior)
  
  # whether the sampled func belongs to an existing category
  other_cats<-unique(state[which(state!=state[to_update])])
  if (length(other_cats)==0) other_cats=state[1]
  checks<-sapply(func_refs, function(x) x==new_func)
  is_new<-F
  if (T %in% checks) {
    cat<-names(func_refs)[which(checks)]
  } else {
    cat<-paste0('c', max(as.numeric(sub('c', '', other_cats)))+1)
    func_refs[[cat]]<-new_func
    is_new<-T
  }
  
  propto<-list()
  # Check for existing categories
  other_obs<-state[setdiff(seq(nobs), to_update)]
  for (s in other_cats) {
    ob_indx<-which(other_obs==s)
    # Chinese restaurant process
    join_this<-length(ob_indx)/(nobs-1+alpha)
    # Dirichlet on feature similarity
    cat_feat<-init_feat_dist(beta)
    for (i in ob_indx) cat_feat<-as.list(unlist(cat_feat)+unlist(read_feature(as.list(task_obs[i,]), grouping)))
    resemblance<-sum(unlist(cat_feat)*unlist(obs_feats))/sum(unlist(cat_feat))
    # Causal power
    pl<-c()
    for (i in c(ob_indx, to_update)) pl<-c(pl, get_likeli(func_refs[[s]], as.list(task_obs[i,])))
    if (0 %in% pl) likeli<-0 else likeli<-exp(sum(log(pl))/length(pl))
    # Put together
    propto[[s]]<-join_this*resemblance*likeli
  }
  # Or assign a new one
  if (!(cat %in% other_cats)) {
    join_new<-alpha/(nobs-1+alpha)
    self_resemblance<-sum(unlist(init_feat_dist(beta))*unlist(obs_feats))/sum(unlist(init_feat_dist(beta)))
    this_likeli<-get_likeli(new_func, paste(task_obs[to_update,], collapse=','))
    propto[[cat]]<-join_new*self_resemblance*this_likeli
  }
  
  # Sample new category
  state[to_update]<-sample(names(propto), 1, prob=unlist(propto))
  states[[n]]<-state
  for (s in state) {
    if (!(s %in% names(categories))) categories[[s]]<-list()
    categories[[s]][[as.character(n)]]<-which(state==s)
  }

  # Go to the next iteration
  n<-n+1
}


# Have a look
df<-data.frame(matrix(unlist(states), nrow=length(states), byrow=T))
df%>%tail(10)






