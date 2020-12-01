
source('gibbs.R')
source('preds.R')
source('shared.R')

alpha=1024
beta=0
softmax_base=''
drop=500
slice=1
iter=10000
set.seed(123)

load('hypos.Rdata')
tasks<-read.csv('../data/setup/main.csv')
n_gen_obs<-16
n_learn_obs<-6

results<-list()
preds<-data.frame(group=character(0),
                  trial=numeric(0),
                  object=numeric(0),
                  prob=numeric(0),
                  type=character(0))
for (c in 1:4) {
  for (g in c(1, 0)) {
    cond<-paste0('A', c)
    n<-if (g=='A') c*2-1 else c*2

    x<-run_gibbs_sampler(cond, g, alpha, beta, iter, F)
    cats<-read_cats(x[[1]], base=softmax_base, burn_in=drop, thinning=slice)
    func_preds<-prep_preds(x[[2]], cond)
    y<-get_cond_preds(cond, cats, func_preds, alpha, beta, g)

    results[[n]]<-x
    preds<-rbind(preds, y)
  }
}
preds$object<-as.character(preds$object)
save(ce_preds, results, preds, file='models.Rdata')

ggplot(preds, aes(x=object, y=trial, fill=prob)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  #scale_fill_viridis(option="E", direction=-1) + 
  theme_linedraw() +
  facet_grid(type~group)


# Look ups
cats<-read_cats(x[[1]])
nrow(cats%>%filter(n>5))

cats_cp<-read_cats(results[[3]][[1]])

# Plot with baseline model & ppt data
# Get pplt data from ../analysis/behav_plot.R
ppt<-passed%>%
  select(condition, task=trial, object=result, prob) %>%
  mutate(type='ppt')
plain<-ce_preds%>%
  select(condition=group, task=trial, object, prob=pred)%>%
  mutate(type='plain')
dps<-preds%>%
  select(condition=group, task=trial, object, prob, type)
dpr<-dpr_preds%>%
  select(condition=group, task=trial, object, prob, type)

combined<-bind_rows(ppt, plain, dps, dpr)
combined$type=factor(combined$type, levels=c('plain','R', 'AR', 'A', 'ppt'))
combined$object<-as.character(combined$object)

ggplot(combined, aes(x=object, y=task, fill=prob)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  #scale_fill_viridis(option="E", direction=-1) + 
  # theme_linedraw() +
  facet_grid(type~condition)

# Compare likelihoods (before fitting parameters)

# Try DP(R)
# Run Gibbs sampler with given parameters
dpr_preds<-data.frame(
  group=character(0), trial=numeric(0), object=numeric(0), 
  prob=numeric(0), type=character(0)
)
# Get categories & make predictions
for (c in 1:4) {
  cond<-paste0('A', c)
  x<-run_gibbs_sampler(cond, 'R', 1, 1/9, iter, F)
  cats<-read_cats(x[[1]], base=softmax_base, burn_in=drop, thinning=slice)
  func_preds<-prep_preds(x[[2]], cond)
  dpr_preds<-rbind(dpr_preds, 
                   get_cond_preds(cond, cats, func_preds, 1, 1/9, 'R'))  
}
ggplot(dpr_preds, aes(x=object, y=trial, fill=prob)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  #scale_fill_viridis(option="E", direction=-1) + 
  theme_linedraw() +
  facet_grid(type~group)

results_r<-x
preds<-rbind(preds, dpr_preds)

save(ce_preds, results, results_r, preds, file='models.Rdata')


# Plot fitted results
apply_softmax<-function(data, b) {
  ds<-filter(data, group=='A1', trial==1) %>% mutate(softmaxed=softmax(prob, b))
  for (c in 1:4) {
    for (i in 1:16) {
      if (!(c==1 & i==1)) {
        ds<-rbind(ds,
                  filter(data, group==paste0('A', c), trial==i) %>% 
                    mutate(softmaxed=softmax(prob, b)))
      }
    }
  }
  return(ds)
}

dpa<-dpa_raw_preds[[174]]
dpar<-dpar_raw_preds[[156]]

dpa<-apply_softmax(dpa, 7.22) %>%
  select(group, trial, object, prob=softmaxed, type)
dpar<-apply_softmax(dpar, 8.2) %>%
  select(group, trial, object, prob=softmaxed, type)

dpr<-preds%>%filter(type=='R')

ndp<-ce_preds%>%mutate(prob=pred)%>%apply_softmax(.,3.19) %>%
  select(group, trial, object, prob=softmaxed) %>%
  mutate(type='NDP')

totals<-counts%>%
  group_by(group, trial)%>%
  summarise(n=sum(count))
ppt<-counts %>%
  left_join(totals, by=c('group', 'trial')) %>%
  mutate(prob=count/n) %>%
  select(group, trial, object, prob) %>%
  mutate(type='ppt')


all_data<-rbind(ppt, dpa, dpar, dpr, ndp)
all_data$type<-factor(all_data$type, levels=c('NDP', 'ppt', 'AR', 'A', 'R'))

ggplot(all_data, aes(x=object, y=trial, fill=prob)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  #scale_fill_viridis(option="E", direction=-1) + 
  theme_linedraw() +
  facet_grid(type~group)

# Debug
by_a<-list()
dpr_preds<-data.frame(
  group=character(0), trial=numeric(0), object=numeric(0), 
  prob=numeric(0), type=character(0)
)
for (c in 1:4) {
  cond<-paste0('A', c)
  x<-run_gibbs_sampler(cond, 1, alpha, beta, iter, F)
  cats<-read_cats(x[[1]], base=softmax_base, burn_in=drop, thinning=slice)
  # func_preds<-prep_preds(x[[2]], cond)
  # dpr_preds<-rbind(dpr_preds, 
  #                  get_cond_preds(cond, cats, func_preds, alpha, beta, 1))
  by_a[[c]]<-x
}
by_a[['preds']]<-dpr_preds

by_r<-list()
dpr_preds<-data.frame(
  group=character(0), trial=numeric(0), object=numeric(0), 
  prob=numeric(0), type=character(0)
)
for (c in 1:4) {
  cond<-paste0('A', c)
  x<-run_gibbs_sampler(cond, 0, alpha, beta, iter, F)
  # cats<-read_cats(x[[1]], base=softmax_base, burn_in=drop, thinning=slice)
  # func_preds<-prep_preds(x[[2]], cond)
  # dpr_preds<-rbind(dpr_preds, 
  #                  get_cond_preds(cond, cats, func_preds, alpha, beta, 0))
  by_r[[c]]<-x
}
by_r[['preds']]<-dpr_preds
save(by_a, by_r, file = 'debug.Rata')












