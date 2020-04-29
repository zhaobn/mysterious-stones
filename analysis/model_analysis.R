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











