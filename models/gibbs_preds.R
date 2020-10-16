
source('gibbs.R')
source('preds.R')

alpha=1
beta=1/9
softmax_base=''
drop=500
slice=1

results<-list()
preds<-data.frame(group=character(0),
                  trial=numeric(0),
                  object=numeric(0),
                  prob=numeric(0),
                  type=character(0))
for (c in 1:4) {
  for (g in c('A', 'AR')) {
    cond<-paste0('A', c)
    n<-if (g=='A') c*2-1 else c*2

    x<-run_gibbs_sampler(cond, g, alpha, beta, 10000, F)
    cats<-read_cats(x[[1]], base=softmax_base, burn_in=drop, thinning=slice)
    func_preds<-prep_preds(x[[2]], cond)
    y<-get_cond_preds(cond, cats, func_preds, alpha, beta, g)

    results[[n]]<-x
    preds<-rbind(preds, y)
  }
}
preds$object<-as.numeric(as.character(preds$object))
save(ce_preds, results, preds, file='models.Rdata')


ggplot(preds, aes(x=object, y=trial, fill=prob)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  #scale_fill_viridis(option="E", direction=-1) + 
  theme_linedraw() +
  facet_grid(type~group)


# Look ups
cats<-read_cats(a1.a[[1]])
nrow(cats%>%filter(n>5))

cats2<-read_cats(a2.a[[1]])
nrow(cats2%>%filter(n>5))

cats_a1ar<-read_cats(a1.ar[[1]])

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

combined<-bind_rows(ppt, plain, dps)
combined$type=factor(combined$type, levels=c('plain', 'AR', 'A', 'ppt'))
ggplot(combined, aes(x=object, y=task, fill=prob)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  #scale_fill_viridis(option="E", direction=-1) + 
  # theme_linedraw() +
  facet_grid(type~condition)

# Check likelihood (before fitting parameters)
counts<-df.tw %>%
  filter(phase=='gen'&grepl('gen', sid)) %>%
  mutate(trial=as.numeric(substr(sid,8,9))) %>%
  select(ix, condition, trial, result) %>%
  arrange(condition, trial) %>%
  group_by(condition, trial, result) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  right_join(default, by=c('condition', 'trial', 'result')) %>%
  mutate(count=replace_na(count, 0))

data_likeli<-function(df) {
  data<-counts %>% 
    filter(count > 0) %>%
    select(condition, trial, object=result, count) %>%
    left_join(df, by=c('condition', 'trial', 'object'))
  return(sum(log(data$prob) * data$count))
}
data_likeli(select(ce_preds, condition=group, trial, object, prob=pred))
# -19792.37
data_likeli(preds %>% filter(type=='AR') %>% select(condition=group, trial, object, prob))
# -8943.973
data_likeli(preds %>% filter(type=='A') %>% select(condition=group, trial, object, prob))
# -8595.594



