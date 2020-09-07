
library(dplyr)
library(ggplot2)
library(viridis)
source('./shared.R')


# simple pilot
ggplot(ce_preds, aes(x=object, y=trial, fill=pred)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=unique(ce_preds$trial)) + 
  scale_fill_gradient(low='white', high='#293352') +
  facet_grid(~group)


# multi plot
ppt<-df.ppt%>%mutate(source='pilot')%>%select(group, trial, object, prob=freq, source)
ue<-ueffect_preds%>%mutate(source='uni_effects')%>%select(group, trial, object, prob=pred, source)
cp<-comp_preds%>%mutate(source='cause_effects')%>%select(group, trial, object, prob=pred, source)

df<-rbind(ppt, ue, cp)
df$source<-factor(df$source, levels=c('pilot', 'cause_effects', 'uni_effects'))
ggplot(df, aes(x=object, y=trial, fill=prob)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=unique(df$trial)) + 
  scale_fill_gradient(low='white', high='#293352') +
  #scale_fill_viridis(option="E", direction=-1) + 
  theme_linedraw() +
  facet_grid(source~group)



# try radar
ppt_r11<-ppt%>%filter(group=='A1'&trial==1)
ppt_r11<-as.data.frame(t(ppt_r11$prob))
colnames(ppt_r11)<-all_objects

ue_r11<-df%>%filter(group=='A1'&trial==1&source=='uni_effects')
ue_r11<-as.data.frame(t(ue_r11$prob))
colnames(ue_r11)<-all_objects

r11<-rbind(ppt_r11, ue_r11)
rownames(r11)<-c('pilot', 'uni_effects')
r11<-rbind(rep(1, length(all_objects)), rep(0, length(all_objects)), r11)

radarchart(r11, pcol=c('red', 'blue'))

# try lollipop
# Create data
value1 <- abs(rnorm(26))*2
data <- data.frame(
  x=LETTERS[1:26], 
  value1=value1, 
  value2=value1+1+rnorm(26, sd=1) 
)

# Reorder data using average? Learn more about reordering in chart #267
data <- data %>% 
  rowwise() %>% 
  mutate( mymean = mean(c(value1,value2) )) %>% 
  arrange(mymean) %>% 
  mutate(x=factor(x, x))
ggplot(data) +
  geom_segment( aes(x=x, xend=x, y=value1, yend=value2), color="grey") +
  geom_point( aes(x=x, y=value1), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=x, y=value2), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Value of Y")

ppt_l11<-ppt%>%filter(group=='A1'&trial==1)%>%select(object, ppt=prob)
ue_l11<-df%>%filter(source=='uni_effects'&group=='A1'&trial==1)%>%select(object, ue=prob)
l11<-ppt_l11%>%left_join(ue_l11, by='object')

ggplot(l11) +
  geom_segment(aes(x=object, xend=object, y=ppt, yend=ue), color="grey") +
  geom_point(aes(x=object, y=ppt), color=rgb(0.2,0.7,0.1,0.5), size=3) +
  geom_point(aes(x=object, y=ue), color=rgb(0.7,0.2,0.1,0.5), size=3) +
  coord_flip()






