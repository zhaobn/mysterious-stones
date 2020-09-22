
source('gibbs.R')
source('preds.R')

cond='A1'
grouping='AR'
alpha=1
beta=.1
softmax_base=''

x<-run_gibbs_sampler(cond, grouping, alpha, beta, 1000, F)
x<-a1.ar
p<-get_cond_preds(cond, x[['state']], x[['funcs']], alpha, beta, grouping, softmax_base)
cat<-read_cats(x[[1]], 0)

ggplot(p, aes(x=object, y=trial, fill=prob)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=1:5) + 
  scale_fill_gradient(low='white', high='#293352') +
  #scale_fill_viridis(option="E", direction=-1) + 
  theme_linedraw() +
  facet_grid(type~group)

a1.a<-x
a1.ar<-x
a2.a<-x
a2.ar<-x

a1.a.preds<-p
a1.ar.preds<-p
a2.a.preds<-p
a2.ar.preds<-p

a1.a.preds$source<-'A1-A'
a1.ar.preds$source<-'A1-AR'
a2.a.preds$source<-'A2-A'
a2.ar.preds$source<-'A2-AR'

a1.ppt<-df.ppt%>%filter(group=='A1')%>%mutate(type='ppt', source='A1-ppt')%>%
  select(group, trial, object, prob=freq, type, source)
a2.ppt<-df.ppt%>%filter(group=='A2')%>%mutate(type='ppt', source='A2-ppt')%>%
  select(group, trial, object, prob=freq, type, source)

a1.noncat<-ce_preds%>%filter(group=='A1')%>%mutate(type='non-cat', source='A1-base')%>%
  select(group, trial, object, prob=pred, type, source)
a2.noncat<-ce_preds%>%filter(group=='A2')%>%mutate(type='non-cat', source='A1-base')%>%
  select(group, trial, object, prob=pred, type, source)

library(ggplot2)
ggplot(rbind(a1.ppt, a2.ppt, a1.noncat, a2.noncat,
             a1.a.preds, a2.a.preds, a1.ar.preds, a2.ar.preds), 
       aes(x=object, y=trial, fill=prob)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=1:5) + 
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


