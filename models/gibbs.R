
library(dplyr)

# Setup & global vars

alpha=0.1 # CRP
beta=0.1 # Dirichlet
grouping='A' # 'A': agent-only, 'AR': agent-and-recipient

tasks<-read.csv('../data/pilot_setup.csv')
task_obs<-tasks%>%filter(group=='A1'&phase=='learn')%>%select(agent, recipient, result)
nobs<-nrow(task_obs)

hypos<-effects_grouped%>%select(hypo=shortest, prior)

obs_samples<-list()
func_refs<-list()

# Sampler
# initialize
state=c('c-1', 'c-1', 'c-2', 'c-2', 'c-3', 'c-4') #state<-paste0('c-',seq(nobs))
for (s in state) func_refs[[s]]<-sample(hypos$hypo, 1, prob=hypos$prior)

n<-1
limit<-1000
while (n<limit) {
  # sample 1 obs
  to_update<-sample(seq(nobs), 1) # index
  obs_feats<-read_feature(as.list(task_obs[to_update,]), grouping)
  
  propto<-list()
  # Check for existing categories
  for (s in unique(state[which(state!=state[to_update])])) {
    ob_indx<-which(state==s)
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
  new_s<-paste0('c-', max(as.numeric(sub('c-', '', names(propto))))+1)
  
  this_data<-paste(task_obs[to_update,], collapse=',')
  posterior<-mapply(get_likeli, hypos$hypo, rep(this_data, length(hypos$hypo)))
  new_func<-sample(hypos$hypo, 1, prob=posterior)
  func_refs[[new_s]]<-new_func
  
  join_new<-alpha/(nobs-1+alpha)
  self_resemblance<-sum(unlist(init_feat_dist(beta))*unlist(obs_feats))/sum(unlist(init_feat_dist(beta)))
  this_likeli<-get_likeli(new_func, this_data)
  
  propto[[new_s]]<-this_likeli
  
  # Sample new category
  state[to_update]<-sample(names(propto), 1, prob=unlist(propto))
  obs_samples[[n]]<-state
  
  # Go to the next iteration
  n<-n+1
}


save(obs_samples, func_refs, file='../data/gibbs-a1.Rdata')




