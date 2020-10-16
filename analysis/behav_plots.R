
library(tidyverse)
load('../data/mturk/mturk_main.Rdata')

badEntries<-df.sw%>%filter(rule_ok==0)%>%pull(id)%>%as.character()
passed<-df.sw%>%filter(pass>0)%>%pull(id)%>%as.character()

all_results<-c()
for (e in 3:7) {
  for (s in 1:4) {
    all_results<-c(all_results, e*10+s)
  }
}
default<-expand.grid(condition=paste0('A', seq(4)), trial=seq(16), result=all_results)
default$condition<-as.character(default$condition)

# Plot data
prep_plot<-function(data, l) {
  data<-data %>%
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
    mutate(freq = n / sum(n), 
           result=as.character(result), 
           label=l,
           fixed=if_else(condition %in% c('A1', 'A3'), 'Fixed Agent', 'Fixed Recipient'),
           rule=if_else(condition %in% c('A1', 'A2'), 'edge(A)+1, shade(R)+1', 'shade(A)+1, edge(R)+1')) %>%
    select(condition, trial, result, prob=freq, label, fixed, rule)
  return(data)
}
all<-prep_plot(df.tw, 'all')
ok<-prep_plot(filter(df.tw, !(id %in% badEntries)), 'ok')
bots<-prep_plot(filter(df.tw, id %in% badEntries), 'bot')
passed<-prep_plot(filter(df.tw, id %in% passed), 'passed')

ggplot(ok, aes(x=result, y=trial, fill=prob)) + geom_tile() + 
  labs(x='object', y='task') +
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  facet_grid(fixed~rule)

ggplot(rbind(all, ok, bots, passed), aes(x=result, y=trial, fill=prob)) + geom_tile() + 
  labs(x='object', y='task') +
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  facet_grid(label~condition)


# Measure congruency
max_var<-var(c(1, rep(0,19)))
fc_data<-passed %>% 
  group_by(condition, trial) %>% 
  summarise(congruency = var(prob)/max_var)

ggplot(fc_data,
       aes(x=(reorder(condition, desc(condition))), y=congruency, fill=condition)) +
  geom_boxplot() +
  coord_flip() + 
  labs(x='condition', y='', title='Congruency measure')

# statistical test
t.test(filter(fc_data, condition=='A1')%>%pull(congruency),
       filter(fc_data, condition=='A2')%>%pull(congruency), paired = F)


# Compare correction rate for gen trials
df.sw %>%
  filter(!(id %in% badEntries)) %>%
  select(ix, correct, condition) %>%
  group_by(condition) %>%
  summarise(correct=sum(correct), n=n()) %>%
  mutate(correct_rate=correct/(n*18))

# What if you filter people by getting the check trial correct  
df.sw %>% count(condition)
df.sw %>% filter(rule_ok==1) %>% count(condition)
df.sw %>% filter(pass==1) %>% count(condition)

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
  
sc_data<-passed %>% 
  group_by(condition, trial) %>% 
  summarise(congruency = var(prob)/max_var)

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

# compare with naive model
pc_data<-ce_preds %>% 
  group_by(group, trial) %>% 
  summarise(congruency = var(pred)/max_var)
t.test(filter(pc_data, group=='A1')%>%pull(congruency),
       filter(pc_data, group=='A2')%>%pull(congruency), paired = T)
t.test(filter(pc_data, group=='A3')%>%pull(congruency),
       filter(pc_data, group=='A4')%>%pull(congruency), paired = T)






