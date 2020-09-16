
library(dplyr)
source('shared.R')

# Setup & global vars
alpha=1 # CRP
beta=0.1 # Dirichlet
grouping='AR' # 'A': agent-only, 'AR': agent-and-recipient
cond='A2'


tasks<-read.csv('../data/pilot_setup.csv')
task_obs<-tasks%>%filter(group==cond&phase=='learn')%>%select(agent, recipient, result)
nobs<-nrow(task_obs)

# Prep posterior dists for speeding up
hypos<-effects_grouped%>%select(hypo=shortest, prior)
for (i in seq(nobs)) {
  d<-paste(task_obs[i,], collapse=',')
  hypos[,paste0('post_',i)]<-mapply(get_likeli, hypos$hypo, rep(d, nrow(hypos)))
}

# Save up for usage in this session
hypos_a1<-hypos
hypos_a2<-hypos


hypos<-hypos_a2  
non_empty<-hypos%>%filter(post_1>0&post_2>0&post_3>0&post_4>0&post_5>0&post_6>0)

# Sampler
# initialize
states<-list()
func_refs<-list()

# Initalization - need non-empty probs
# state<-rep('c1', nobs)
# func_refs[['c1']]<-sample(non_empty$hypo, 1, prob=non_empty$prior)
state<-paste0('c', seq(nobs))
funcs<-sample(non_empty$hypo, nobs, prob=non_empty$prior, replace=F)
for (i in seq(nobs)) func_refs[[state[i]]]<-funcs[i]

n<-1
limit<-3000
while (n<limit) {
  # sample 1 obs
  to_update<-sample(seq(nobs), 1) # index
  obs_to_update<-as.list(task_obs[to_update,])
  obs_feats<-read_feature(obs_to_update, grouping)
  # sample a function for this obs wrt posterior
  post_col<-paste0('post_',to_update)
  funcs_pool<-hypos$hypo[which(hypos[,post_col]>0)]
  funcs_post<-hypos[which(hypos[,post_col]>0), post_col]
  new_func<-sample(funcs_pool, 1, prob=funcs_post)
  # whether the sampled func belongs to an existing category
  checks<-sapply(func_refs, function(x) x==new_func)
  if (T %in% checks) {
    cat<-names(func_refs)[which(checks)]
  } else {
    cat<-paste0('c', length(func_refs)+1)
    func_refs[[cat]]<-new_func
  }
  # Check for existing categories
  propto<-list()
  other_idx<-setdiff(seq(nobs), to_update)
  for (s in unique(state[other_idx])) {
    ob_indx<-other_idx[which(state[other_idx]==s)]
    # Chinese restaurant process
    join_this<-length(ob_indx)/(nobs-1+alpha)
    # Dirichlet on feature similarity
    cat_feat<-init_feat_dist(beta)
    for (i in ob_indx) cat_feat<-as.list(unlist(cat_feat)+unlist(read_feature(as.list(task_obs[i,]), grouping)))
    resemblance<-sum(unlist(cat_feat)*unlist(obs_feats))/sum(unlist(cat_feat))
    # Causal power
    prob<-get_likeli(func_refs[[s]], obs_to_update)*hypos[which(hypos$hypo==func_refs[[s]]),'prior']
    # Put together
    propto[[s]]<-join_this*resemblance*prob
  }
  # Or assign a new one
  if (!(cat %in% state[other_idx])) {
    join_new<-alpha/(nobs-1+alpha)
    self_resemblance<-2/length(cat_feat)
    prob<-get_likeli(new_func, obs_to_update)*hypos[which(hypos$hypo==new_func),'prior']
    propto[[cat]]<-join_new*self_resemblance*prob
  }
  
  sum<-Reduce('+', propto)
  if (sum==0) { # Start over
    alert(paste0('Iter ', n, ': Zero probabilities!'))
    next
  } else {
    # Sample new category
    state[to_update]<-sample(names(propto), 1, prob=unlist(propto))
    # ll<-1
    # for (i in seq(nobs)) ll<-ll*get_likeli(func_refs[[state[i]]], as.list(task_obs[i,]))
    # print(paste0(n, ': ', paste(state, collapse=','), ' | likeli: ', round(ll,4)))
    print(paste0(n, ': ', paste(state, collapse=',')))
    states[[n]]<-state
    # Go to the next iteration
    n<-n+1
  }
}


a1.a.states<-states
a1.a.funcs<-func_refs

a1.ar.states<-states
a1.ar.funcs<-func_refs

a2.a.states<-states
a2.a.funcs<-func_refs

a2.ar.states<-states
a2.ar.funcs<-func_refs

save(a1.a.states, a1.a.funcs, a1.ar.states, a1.ar.funcs,
     a2.a.states, a2.a.funcs, a2.ar.states, a2.ar.funcs,
     file='../data/gibbs-a1a2.Rdata')


# Have a look
states<-a1.ar.states

df<-data.frame(matrix(unlist(states), nrow=length(states), byrow=T))
df$i<-seq(nrow(df))
df<-df%>%filter(i>500)

a1.a.cats<-df%>%group_by(X1, X2, X3, X4, X5, X6)%>%summarise(n=n())
a2.a.cats<-df%>%group_by(X1, X2, X3, X4, X5, X6)%>%summarise(n=n())

a1.ar.cats<-df%>%group_by(X1, X2, X3, X4, X5, X6)%>%summarise(n=n())
a2.ar.cats<-df%>%group_by(X1, X2, X3, X4, X5, X6)%>%summarise(n=n())

# Plot sample dists
library(ggplot2)

sum_cat<-function(df) {
  df<-df%>%ungroup()%>%arrange(desc(n))%>%mutate(cat=seq(nrow(df)))%>%
    # mutate(X1=as.character(X1),X2=as.character(X2), X3=as.character(X3),
    #        X4=as.character(X4),X5=as.character(X5), X6=as.character(X6))%>%
    # mutate(cat=paste0(X1,'-',X2,'-',X3,'-',X4,'-',X5,'-',X6))%>%
    select(cat, n)
  return(df)
}

x<-rbind(sum_cat(a1.a.cats)%>%mutate(group='A1_A'),
         sum_cat(a1.ar.cats)%>%mutate(group='A1_AR'),
         sum_cat(a2.a.cats)%>%mutate(group='A2_A'),
         sum_cat(a2.ar.cats)%>%mutate(group='A2_AR'))
xf<-x%>%filter(cat<11)
ggplot(xf, aes(x=cat, y=n, fill=group)) + geom_bar(position="dodge", stat="identity")

# # Try density plot
for_dense<-function(df, type) {
  dendf<-data.frame(cat=numeric(0))
  for (i in 1:nrow(df)) {
    d<-data.frame(rep(i, df[i,'n']))
    colnames(d)<-c('cat')
    dendf<-rbind(dendf, d)
  }
  dendf$group=type
  return(dendf)
}


xd<-rbind(for_dense(sum_cat(a1.a.cats), 'A1_A'),
         for_dense(sum_cat(a1.ar.cats), 'A1_AR'),
         for_dense(sum_cat(a2.a.cats), 'A2_A'),
         for_dense(sum_cat(a2.ar.cats), 'A2_AR'))
xdf<-xd%>%filter(cat<11)
ggplot(xdf, aes(x=cat, fill=group, color=group)) +
  geom_density(alpha=0.3) + 
  facet_wrap(~group)

ggplot(xd, aes(x=cat, fill=group, color=group)) +
  geom_density(alpha=0.3) + 
  facet_wrap(~group) +
  theme_minimal()

# Plot number of cats
xg<-data.frame(group=c('A1_A', 'A1_AR', 'A2_A', 'A2_AR'),
               n=c(nrow(a1.a.cats), nrow(a1.ar.cats), nrow(a2.a.cats), nrow(a2.ar.cats)))
ggplot(xg, aes(x=group, y=n, fill=group)) + geom_bar(stat='identity') +
  ylab('distinct samples')









