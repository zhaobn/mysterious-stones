
library(tidyverse)

badEntries<-df.sw%>%filter(rule_ok==0)%>%pull(id)%>%as.character()

all_results<-c()
for (e in 3:7) {
  for (s in 1:4) {
    all_results<-c(all_results, e*10+s)
  }
}
default<-expand.grid(condition=paste0('A', seq(4)), trial=seq(16), result=all_results)
default$condition<-as.character(default$condition)


data<-df.tw %>%
  filter(phase=='gen'&grepl('gen', sid)) %>%
  mutate(trial=as.numeric(substr(sid,8,9))) %>%
  select(ix, condition, trial, result) %>%
  arrange(condition, trial) %>%
  group_by(condition, trial, result) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  right_join(default, by=c('condition', 'trial', 'result')) %>%
  mutate(count=replace_na(count, 0)) %>%
  group_by(condition, trial, result) %>%
  summarise(n = sum(count)) %>%
  mutate(freq = n / sum(n), result=as.character(result), label='all') %>%
  mutate(Fixed=if_else(condition %in% c('A1', 'A3'), 'Fixed Agent', 'Fixed Recipient'),
         Rule=if_else(condition %in% c('A1', 'A2'), 'edge(A)+1, shade(R)+1', 'shade(A)+1, edge(R)+1'))

ggplot(data, aes(x=result, y=trial, fill=freq)) + geom_tile() + 
  labs(x='object', y='task') +
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  facet_grid(Fixed~Rule)

# Filtered data
f_data<-df.tw %>%
  filter(phase=='gen'&grepl('gen', sid)&(!id %in% badEntries)) %>%
  mutate(trial=as.numeric(substr(sid,8,9))) %>%
  select(ix, condition, trial, result) %>%
  arrange(condition, trial) %>%
  group_by(condition, trial, result) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  right_join(default, by=c('condition', 'trial', 'result')) %>%
  mutate(count=replace_na(count, 0)) %>%
  group_by(condition, trial, result) %>%
  summarise(n = sum(count)) %>%
  mutate(freq = n / sum(n), result=as.character(result), label='ok')

b_data<-df.tw %>%
  filter(phase=='gen'&grepl('gen', sid)&(id %in% badEntries)) %>%
  mutate(trial=as.numeric(substr(sid,8,9))) %>%
  select(ix, condition, trial, result) %>%
  arrange(condition, trial) %>%
  group_by(condition, trial, result) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  right_join(default, by=c('condition', 'trial', 'result')) %>%
  mutate(count=replace_na(count, 0)) %>%
  group_by(condition, trial, result) %>%
  summarise(n = sum(count)) %>%
  mutate(freq = n / sum(n), result=as.character(result), label='bad')

ggplot(rbind(f_data, b_data), aes(x=result, y=trial, fill=freq)) + geom_tile() + 
  labs(x='object', y='task') +
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  facet_grid(label~condition)

