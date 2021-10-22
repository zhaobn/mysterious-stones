
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



# Plot behavorial data vs model
all_results<-c()
for (e in 3:7) {
  for (s in 1:4) {
    all_results<-c(all_results, e*10+s)
  }
}
default<-expand.grid(condition=paste0('B', seq(4)), trial=seq(16), result=all_results)
default$condition<-as.character(default$condition)

beh<-df.tw %>%
  filter(phase=='gen'&grepl('gen', sid)) %>%
  mutate(trial=as.numeric(substr(sid,8,9)),
         condition=paste0('B',substr(condition,2,2))) %>%
  select(ix, condition, trial, result) %>%
  arrange(condition, trial) %>%
  group_by(condition, trial, result) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  right_join(default, by=c('condition', 'trial', 'result')) %>%
  mutate(count=replace_na(count, 0)) %>%
  group_by(condition, trial, result) %>%
  summarise(n = sum(count)) %>%
  mutate(freq = n / sum(n), 
         result=as.character(result), 
         data='mturk',
         # label=l,
         # fixed=if_else(condition %in% c('A1', 'A3'), 'Fixed Agent', 'Fixed Recipient'),
         # rule=if_else(condition %in% c('A1', 'A2'), 'edge(A)+1, shade(R)+1', 'shade(A)+1, edge(R)+1')
         ) %>%
  select(condition, trial, result, prob=freq, data)

ggplot(beh, aes(x=result, y=trial, fill=prob)) + geom_tile() + 
  labs(x='object', y='task') +
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  facet_grid(~condition) +
  theme_bw()


locala<-model.proc %>%
  mutate(condition=paste0('B',substr(group,2,2)), result=object, data='LoCaLa') %>%
  select(condition, trial, result, prob=prob_s, data)


ggplot(locala, aes(x=result, y=trial, fill=prob)) + geom_tile() + 
  labs(x='object', y='task') +
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  facet_grid(~condition) +
  theme_bw()


uncala<-model.uni %>%
  mutate(condition=paste0('B',substr(group,2,2)), result=object, data='UnCaLa') %>%
  select(condition, trial, result, prob=prob_s, data)


ggplot(uncala, aes(x=result, y=trial, fill=prob)) + geom_tile() + 
  labs(x='object', y='task') +
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  facet_grid(~condition) +
  theme_bw()



all_data<-rbind(beh, uncala, locala)
ggplot(all_data, aes(x=result, y=trial, fill=prob)) + geom_tile() + 
  labs(x='object', y='task') +
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  facet_grid(data~condition) +
  theme_bw()

plot_data<-rbind(beh, locala)
plot_data$data<-factor(plot_data$data, levels=c('mturk','LoCaLa'))
ggplot(plot_data, aes(x=result, y=trial, fill=prob)) + geom_tile() + 
  labs(x='', y='task') +
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  facet_grid(data~condition) +
  theme_classic() +
  theme(strip.background = element_rect(colour=NA, fill=NA),
        panel.border = element_rect(fill = NA, color = "black"),
        text = element_text(size=15),
        legend.position="bottom",
        legend.title=element_blank())

