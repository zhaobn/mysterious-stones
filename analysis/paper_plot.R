
library(tidyverse)

# Get ppt data
load('../data/mturk/mturk_main.Rdata')
all_objects<-vector()
for (e in 3:7) {
  for (s in 1:4) {
    all_objects<-c(all_objects, paste(c(e,s), collapse=''))
  }
}
default<-expand.grid(group=paste0('A',1:4), trial=1:16, object=all_objects, stringsAsFactors=F)

ppt<-df.tw %>%
  filter(phase=='gen'&grepl('gen', sid)) %>%
  mutate(
    group=condition,
    trial=as.numeric(substr(sid,8,9)), 
    object=as.character(result)) %>%
  group_by(group, trial, object) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  right_join(default, by=c('group', 'trial', 'object')) %>%
  mutate(count=ifelse(is.na(count), 0, count)) %>%
  arrange(group, trial, object)

totals<-ppt %>%
  group_by(group, trial) %>%
  summarise(total=sum(count)) %>%
  ungroup()

ppt_data<-ppt %>%
  left_join(totals, by=c('group', 'trial')) %>%
  mutate(freq=count/total)

# Model data
load('../models/models.Rdata')

# Sanity checks
uni<-model.uni %>% 
  left_join(ppt_data, by=c('group', 'trial', 'object'))
sum(log(uni$prob_s)*uni$count)
uni$condition=paste0('B', substr(uni$group, 2, 2))

proc<-model.proc %>%
  left_join(ppt_data, by=c('group', 'trial', 'object'))
sum(log(proc$prob_s)*proc$count)
proc$condition=paste0('B', substr(proc$group, 2, 2))

# Plot
pp<-ggplot(proc, aes(x=freq, y=prob_s)) +
  geom_point(aes(color=object, shape=condition)) +
  geom_smooth(method=lm, color='black') +
  theme_classic() +
  labs(x='freq', y='prob') + 
  ylim(0,1) + xlim(0,1) +
  theme(legend.position = 'bottom')
up<-ggplot(uni, aes(x=freq, y=prob_s)) +
  geom_point(aes(color=object, shape=condition)) +
  geom_smooth(method=lm, color='black') +
  theme_classic() +
  labs(x='freq', y='prob') +
  ylim(0,1) + xlim(0,1) +
  theme(legend.position = 'bottom')

library(ggpubr)
ggarrange(up, pp, ncol=2,
          labels=c('A','B'), 
          common.legend=TRUE, legend='bottom')












