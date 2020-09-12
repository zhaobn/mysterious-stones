
library(dplyr)
source('shared.R')

# Setup & global vars
alpha=0.5 # CRP
beta=0.1 # Dirichlet
grouping='A' # 'A': agent-only, 'AR': agent-and-recipient
cond='A1'

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
limit<-10000
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
  
  sum<-Reduce('+', propto)
  if (sum==0) { # Start over
    next
  } else {
    # Filter out empty entries & normalize
    propto<-propto[unlist(lapply(propto, function(x) x>0))]
    propto<-lapply(propto, function(x) x/sum)
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
}

save(states, func_refs, categories, file='gibbs-a1.Rdata')

# Have a look
df<-data.frame(matrix(unlist(states), nrow=length(states), byrow=T))
df$i<-seq(nrow(df))
df%>%tail(10)

# Get posterior
# Discard first 1000 samples
samples<-lapply(categories, function(x) x[which(as.numeric(names(x))>1000)])
# Discard empty funcs
samples<-samples[unlist(lapply(samples, function(x) length(x)>0))]

fetch_cats<-function(cat_func) {
  x<-samples[[cat_func]]
  x_grouped<-list(); sizes<-c()
  cats<-unique(x)
  for (v in 1:length(cats)) {
    obs<-cats[[v]]
    s<-paste(obs, collapse=',')
    x_grouped[[s]]<-sum(unlist(lapply(x, function(x) identical(obs, x))))
    sizes<-c(sizes, length(obs))
  } 
  
  ds<-as.data.frame(unlist(x_grouped), nrow=length(x_grouped))
  colnames(ds)<-c('n'); rownames(ds)<-c()
  ds$states<-names(x_grouped)
  ds$func_ref<-cat_func
  ds$size<-sizes
  ds<-ds%>%select(func_ref, states, size, n)
  return(ds)
}

cat_post<-data.frame(func_ref=character(0), states=character(0), size=numeric(0), n=numeric(0))
for (c in names(samples)) cat_post<-rbind(cat_post, fetch_cats(c))

a1.post<-cat_post
a1.hypos<-hypos
a1.states<-states
a1.funcs<-func_refs
a1.rawcats<-categories
save(a1.post, a1.hypos, a1.states, a1.funcs, a1.rawcats, file='a1.Rdata')















