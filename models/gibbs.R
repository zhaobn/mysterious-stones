
library(dplyr)
source('shared.R')

# Setup & global vars
alpha=0.5 # CRP
beta=0.1 # Dirichlet
grouping='AR' # 'A': agent-only, 'AR': agent-and-recipient
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

a2.ar.funcs<-func_refs
a2.ar.states<-states
a2.ar.rawcats<-categories
a2.ar.post<-cat_post

a2.a.funcs<-func_refs
a2.a.states<-states
a2.a.rawcats<-categories
a2.a.post<-cat_post

a1.ar.funcs<-func_refs
a1.ar.states<-states
a1.ar.rawcats<-categories
a1.ar.post<-cat_post

a1.a.funcs<-func_refs
a1.a.states<-states
a1.a.rawcats<-categories
a1.a.post<-cat_post


# Try plot posterior
library(ggplot2)

x<-a1.a.post
x<-x%>%mutate(cat=paste0(func_ref, '-', states))%>%arrange(desc(n))#%>%filter(n>1)

a1_a<-data.frame(cat=character(0))
for (i in 1:nrow(x)) {
  df<-data.frame(rep(i, x$n[i]))
  colnames(df)<-c('cat')
  a1_a<-rbind(a1_a, df)
}

ggplot(a1_a, aes(x=cat)) +
  geom_density(fill="#69b3a2", color="#e9ecef") +
  ggtitle("A1-A")

ggplot(a1_a, aes(x=cat))+geom_area(stat="count")

y<-a1.ar.post
y<-y%>%mutate(cat=paste0(func_ref, '-', states))%>%arrange(desc(n))#%>%filter(n>1)

a1_ar<-data.frame(cat=character(0))
for (i in 1:nrow(y)) {
  df<-data.frame(rep(i, y$n[i]))
  colnames(df)<-c('cat')
  a1_ar<-rbind(a1_ar, df)
}

x2<-a2.a.post
x2<-x2%>%mutate(cat=paste0(func_ref, '-', states))%>%arrange(desc(n))#%>%filter(n>1)

a2_a<-data.frame(cat=character(0))
for (i in 1:nrow(x2)) {
  df<-data.frame(rep(i, x2$n[i]))
  colnames(df)<-c('cat')
  a2_a<-rbind(a2_a, df)
}

y2<-a2.ar.post
y2<-y2%>%mutate(cat=paste0(func_ref, '-', states))%>%arrange(desc(n))#%>%filter(n>1)

a2_ar<-data.frame(cat=character(0))
for (i in 1:nrow(y2)) {
  df<-data.frame(rep(i, y2$n[i]))
  colnames(df)<-c('cat')
  a2_ar<-rbind(a2_ar, df)
}

nrow(a1.a.post) #2154
sum(a1.a.post$n) #17851

nrow(a1.ar.post) #2785
sum(a1.ar.post$n) #21042

nrow(a2.a.post) #2823
sum(a2.a.post$n) #23626

nrow(a2.ar.post) #2750
sum(a2.ar.post$n) #20925


z<-rbind(mutate(a1_a, type='A1-A'), mutate(a1_ar, type='A1-AR'),
         mutate(a2_a, type='A2-A'), mutate(a2_ar, type='A2-AR'))

library(viridis)
ggplot(z, aes(x=cat, color=type, fill=type)) +
  geom_density(alpha=0) +
  scale_color_brewer(palette="Set2")
  #scale_fill_viridis(discrete=TRUE) +
  #scale_color_viridis(discrete=TRUE)

# Try plot with same cats
# Refer each cx function to a universal function index
hdx<-hypos%>%select(hypo)%>%mutate(hdx=paste0('uc', seq(nrow(hypos))))

add_ufunc<-function(target, source) {
  xf<-sapply(target$func_ref, function(x) source[[x]])
  xu<-sapply(xf, function(x) hdx[which(hdx$hypo==x), 'hdx'])
  target$u_func<-xu
  return(target)
}
ux<-add_ufunc(x, a1.a.funcs)%>%select(u_func, states, a1_a=n)
uy<-add_ufunc(y, a1.ar.funcs)%>%select(u_func, states, a1_ar=n)
ux2<-add_ufunc(x2, a2.a.funcs)%>%select(u_func, states, a2_a=n)
uy2<-add_ufunc(y2, a2.ar.funcs)%>%select(u_func, states, a2_ar=n)

all_cats<-ux%>%full_join(uy, by=c('u_func', 'states'))%>%
  full_join(ux2, by=c('u_func', 'states'))%>%
  full_join(uy2, by=c('u_func', 'states'))%>%
  replace_na(list(a1_a=0, a1_ar=0, a2_a=0, a2_ar=0))

all_cats$total<-all_cats$a1_a+all_cats$a1_ar+all_cats$a2_a+all_cats$a2_ar
# all_cats<-all_cats%>%arrange(desc(total))
# all_cats$cat<-seq(nrow(all_cats))

all_cats<-all_cats%>%arrange(u_func)
all_cats$cat<-seq(nrow(all_cats))

save(all_cats, file='cats-a.Rdata')

groups=colnames(all_cats)[3:6]
for_dense_plot<-function(name) {
  df<-data.frame(cat=character(0))
  for (i in 1:nrow(all_cats)) {
      d<-data.frame(rep(all_cats[i,'cat'], all_cats[i,name]))
      colnames(d)<-c('cat');
      df<-rbind(df, d)
  }
  df$type=name
  return(df)
}
uf<-for_dense_plot(groups[1])
for (i in 2:length(groups)) uf<-rbind(uf, for_dense_plot(groups[i]))

ggplot(uf, aes(x=cat, color=type, fill=type)) +
  geom_density(alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE)
  #scale_color_brewer(palette="Set2")







