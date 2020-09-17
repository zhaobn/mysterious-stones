
source('gibbs.R')
source('preds.R')

x<-run_gibbs_sampler('A1', 'A', 1, .1, 1000, F)
p<-get_cond_preds('A1', x[['state']], x[['funcs']], 1, .1, 'A', 1)

library(ggplot2)
ggplot(p, aes(x=object, y=trial, fill=prob)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=unique(p$trial)) + 
  scale_fill_gradient(low='white', high='#293352') +
  #scale_fill_viridis(option="E", direction=-1) + 
  theme_linedraw() 








