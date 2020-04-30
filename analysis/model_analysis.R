options("scipen" = 10)
options()$scipen

library(dplyr)
library(ggplot2)
rm(list=ls())

## Results ####################
## https://docs.google.com/document/d/1L9ak9FUX9kaTonije-w6l8hZoXSUH04XB7_gPWcdJpo/edit?usp=sharing
###############################

get_true_hypos<-function(cid, c_src=df.causes, e_src=df.effects) {
  ecol<-paste0('post_', tolower(cid))
  causes<-sum(c_src[cid])
  effects<-nrow(e_src%>%select(effects, post=!!ecol)%>%filter(post>0))
  print(cid)
  print(paste('causes:', causes))
  print(paste('effects:', effects)) 
  print(paste('total:', causes * effects))
}
get_true_hypos('A4')

# Format df.posteriors for plotting
read_pp<-function(cid, tid, src=df.posteriors) {
  pcol<-paste0(tolower(cid), '_0', tid)
  df<-src%>%select(stone, pp=!!pcol)%>%
    mutate(cond=!!cid, trial=!!tid)%>%
    select(cond, trial, stone, pp)
  return(df)
}
df.plot<-read_pp('A1', 1)
for (c in 1:4) {
  cond<-paste0('A', c)
  for (i in 1:5) {
    if (!(c==1 && i==1)) df.plot<-rbind(df.plot, read_pp(cond, i))
  }
}
save(df.causes,df.effects, df.posteriors, df.plot, file = '../models/normative_model.Rdata')

test<-df.plot%>%filter(cond=='A1')
ggplot(df.plot, aes(x=trial, y=reorder(stone, desc(stone)), fill=pp)) +
  geom_tile() +
  geom_text(aes(label=round(pp, 2))) +
  facet_wrap(~cond) + 
  scale_fill_gradient(low = "white", high = "red") +
  theme(legend.position="none") +
  labs(x='generalization trials', y='stone') + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )

# Plot test trial prediction
orig<-df.plot%>%filter(cond=='A2')
test<-test.posteriors%>%select(stone, starts_with('test_'))
colnames(test)<-c('stone', paste0('a2_0', seq(1:5)))
dt<-read_pp('A2', 1, test)
for (i in 2:5) {
  if (!(c==1 && i==1)) dt<-rbind(dt, read_pp('A2', i, test))
}
a1<-df.plot%>%filter(cond=='A1')
a1<-a1%>%mutate(type='A1')
orig<-orig%>%mutate(type='A2, non-symmetric')
dt<-dt%>%mutate(type='A2, symmetric')
test.plot<-rbind(a1, orig, dt)
save(test.causes, test.effects, test.posteriors, test.plot, file='tests.Rdata')

ggplot(test.plot, aes(x=trial, y=reorder(stone, desc(stone)), fill=pp)) +
  geom_tile() +
  geom_text(aes(label=round(pp, 2))) +
  facet_wrap(~type) + 
  scale_fill_gradient(low = "white", high = "red") +
  theme(legend.position="none") +
  labs(x='generalization trials', y='stone') + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )






