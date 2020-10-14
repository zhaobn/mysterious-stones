
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

# Plot data
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

ggplot(s_data, aes(x=result, y=trial, fill=freq)) + geom_tile() + 
  labs(x='object', y='task') +
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  facet_grid(Fixed~Rule)

# Filtered data
f_data<-df.tw %>%
  filter(phase=='gen'&grepl('gen', sid)&!(id %in% badEntries)) %>%
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
  mutate(freq = n / sum(n), result=as.character(result), label='ok') %>%
  mutate(Fixed=if_else(condition %in% c('A1', 'A3'), 'Fixed Agent', 'Fixed Recipient'),
         Rule=if_else(condition %in% c('A1', 'A2'), 'edge(A)+1, shade(R)+1', 'shade(A)+1, edge(R)+1'))


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
  mutate(freq = n / sum(n), result=as.character(result), label='bad') %>%
  mutate(Fixed=if_else(condition %in% c('A1', 'A3'), 'Fixed Agent', 'Fixed Recipient'),
         Rule=if_else(condition %in% c('A1', 'A2'), 'edge(A)+1, shade(R)+1', 'shade(A)+1, edge(R)+1'))


ggplot(rbind(f_data, b_data), aes(x=result, y=trial, fill=freq)) + geom_tile() + 
  labs(x='object', y='task') +
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  facet_grid(label~condition)


# Measure congruency
max_var<-var(c(1, rep(0,19)))
fc_data<-f_data %>% 
  group_by(condition, trial) %>% 
  summarise(congruency = var(freq)/max_var)

ggplot(fc_data,
       aes(x=(reorder(condition, desc(condition))), y=congruency, fill=condition)) +
  geom_boxplot() +
  coord_flip() + 
  labs(x='condition', y='', title='Congruency measure')

# statistical test
t.test(filter(fc_data, condition=='A1')%>%pull(congruency),
       filter(fc_data, condition=='A2')%>%pull(congruency), paired = F)

# measure pass-rate of checking trials
ixes<-df.tw%>%pull(ix)%>%unique()
df.checks<-data.frame(ix=numeric(0), check_trials=character(0), selection=numeric(0), answer=numeric(0))
for (i in ixes) {
  x<-df.tw%>%filter(ix==i & grepl('learn', sid))%>%arrange(sid)
  gens_sid<-x%>%filter(phase=='gen')%>%pull(sid)
  correct_answer<-
  df.checks<-rbind(df.checks, 
                   data.frame(ix=i, 
                              check_trials=gens_sid, 
                              selection=x%>%filter(phase=='gen')%>%pull(result), 
                              answer=x%>%filter(phase=='learn'&sid%in%gens_sid)%>%pull(result)))
}
df.checks<-df.checks%>%
  left_join(df.tw%>%select(ix, condition)%>%unique(), by='ix')
df.checks$correct<-as.numeric(df.checks$selection==df.checks$answer)
  
df.checks %>%
  group_by(condition) %>%
  summarise(correction=sum(correct), n=n())%>%
  mutate(correction_rate=correction/n)

# Compare correction rate for gen trials
df.sw %>%
  filter(!(id %in% badEntries)) %>%
  select(ix, correct, condition) %>%
  group_by(condition) %>%
  summarise(correct=sum(correct), n=n()) %>%
  mutate(correct_rate=correct/(n*18))

# What if you filter people by getting the check trial correct  
pass<-df.checks %>%
  group_by(ix) %>%
  summarise(correct=sum(correct)) %>%
  filter(correct==2) %>%
  pull(ix)

df.sw %>% count(condition)
df.sw %>% filter(!(id %in% badEntries)) %>% count(condition)
df.sw %>% filter(ix %in% pass) %>% count(condition)

s_data<-df.tw %>%
  filter(phase=='gen' & grepl('gen', sid) & ix %in% pass) %>%
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
  mutate(freq = n / sum(n), result=as.character(result), label='ok') %>%
  mutate(Fixed=if_else(condition %in% c('A1', 'A3'), 'Fixed Agent', 'Fixed Recipient'),
         Rule=if_else(condition %in% c('A1', 'A2'), 'edge(A)+1, shade(R)+1', 'shade(A)+1, edge(R)+1'))
  
sc_data<-s_data %>% 
  group_by(condition, trial) %>% 
  summarise(congruency = var(freq)/max_var)

ggplot(sc_data,
       aes(x=(reorder(condition, desc(condition))), y=congruency, fill=condition)) +
  geom_boxplot() +
  coord_flip() + 
  labs(x='', y='', title='Selection Congruency')

# statistical test
t.test(filter(sc_data, condition=='A1')%>%pull(congruency),
       filter(sc_data, condition=='A2')%>%pull(congruency), paired = T)
t.test(filter(sc_data, condition=='A3')%>%pull(congruency),
       filter(sc_data, condition=='A4')%>%pull(congruency), paired = T)
t.test(filter(sc_data, condition %in% c('A1', 'A3'))%>%pull(congruency),
       filter(sc_data, condition %in% c('A2', 'A4'))%>%pull(congruency), paired = T)
t.test(filter(sc_data, condition %in% c('A1', 'A2'))%>%pull(congruency),
       filter(sc_data, condition %in% c('A3', 'A4'))%>%pull(congruency), paired = T)







