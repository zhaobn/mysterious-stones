
source('gibbs.R')
source('preds.R')

cond='A2'
grouping='A'
alpha=1
beta=.1
softmax_base=1

x<-run_gibbs_sampler(cond, grouping, alpha, beta, 10000, F)
p<-get_cond_preds(cond, x[['state']], x[['funcs']], alpha, beta, grouping, softmax_base)

a2.a<-x
p<-get_cond_preds(cond, x[['state']], x[['funcs']], alpha, beta, grouping, softmax_base)

library(ggplot2)
ggplot(p, aes(x=object, y=trial, fill=prob)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=1:5) + 
  scale_fill_gradient(low='white', high='#293352') +
  #scale_fill_viridis(option="E", direction=-1) + 
  theme_linedraw() 

# Have a look at categories
cats<-read_cats(x[[1]])







