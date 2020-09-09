
library(dplyr)
library(ggplot2)
library(viridis)
source('./shared.R')

# single pilot
ggplot(ce_preds, aes(x=object, y=trial, fill=pred)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=unique(ce_preds$trial)) + 
  scale_fill_gradient(low='white', high='#293352') +
  facet_grid(~group)


# multi plot
ppt<-df.ppt%>%mutate(source='pilot')%>%select(group, trial, object, prob=freq, source)
df<-rbind(ppt, ug_preds, ce_preds)
df$source<-factor(df$source, levels=c('pilot', 'un_grouped', 'causal_grouped'))

ggplot(df, aes(x=object, y=trial, fill=prob)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=unique(df$trial)) + 
  scale_fill_gradient(low='white', high='#293352') +
  #scale_fill_viridis(option="E", direction=-1) + 
  theme_linedraw() +
  facet_grid(source~group)











