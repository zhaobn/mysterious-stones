options("scipen" = 10)
options()$scipen

library(dplyr)
library(ggplot)
rm(list=ls())

# Print mean ± sd
ms<-function(vec, time=F) print(paste0(round(mean(vec), 2), ' ± ', round(sd(vec),2)))

# Get filtered data
df.fs<-df.sw%>%filter(rule_ok>0)
df.bs<-df.sw%>%filter(rule_ok==0)
df.ps<-df.sw%>%filter(pass>0)

# Condition sizes
df.sw%>%count(condition)
df.fs%>%count(condition)

# Demographics
df.sw%>%filter(sex=='female')%>%nrow() # 48/163 = 29%
df.fs%>%filter(sex=='female')%>%nrow() # 37/101 = 36%

ms(df.sw$age) # 36.39 ± 10.33
ms(df.fs$age) # 34.83 ± 9.63

# Task duration
ms(df.sw$task_duration/60000) # 10.48 ± 6.77
ms(df.fs$task_duration/60000) # 10.51 ± 7.37

# Mind-change
sum(df.sw$final_changed)/nrow(df.sw) # 60%
sum(df.fs$final_changed)/nrow(df.fs) # 43%
sum(df.bs$final_changed)/nrow(df.bs) # 89%
sum(df.ps$final_changed)/nrow(df.ps) # 45%
data.frame(type=c('all', 'non-bot', 'bot', 'pass'), mind_change=c(.6, .43, .89, .45)) %>%
  ggplot(aes(x=type, fill=type, y=mind_change)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=paste0(mind_change*100, '%')))
  labs(x='', y='', title='Mind-change rate')

# Correction rates
sum(df.sw$correct)/(nrow(df.sw)*18) # 29%
sum(df.fs$correct)/(nrow(df.fs)*18) # 42%
sum(df.bs$correct)/(nrow(df.bs)*18) # 7% ~ 5%
sum(df.ps$correct)/(nrow(df.ps)*18) # 7% ~ 5%

c<-bind_rows(
  df.sw %>% group_by(condition) %>% 
    summarise(correct=sum(correct),n=n())%>%mutate(type='all'),
  filter(df.sw, rule_ok<1) %>% group_by(condition) %>% 
    summarise(correct=sum(correct),n=n())%>%mutate(type='bot'),
  filter(df.sw, rule_ok>0) %>% group_by(condition) %>% 
    summarise(correct=sum(correct),n=n())%>%mutate(type='non-bot'),
  filter(df.sw, pass>0) %>% group_by(condition) %>% 
    summarise(correct=sum(correct),n=n())%>%mutate(type='passed'),
)
c$correction_rate<-c$correct/(c$n*18)
c$condition<-factor(c$condition, levels=c('A1','A2','A3','A4'))

ggplot(c, aes(x=type, y=correction_rate, fill=type)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=paste0(round(correction_rate*100),'%')), position=position_dodge(width=0.4), vjust=0.5) +
  geom_hline(yintercept = 1/20) +
  facet_wrap(~condition) +
  labs(x='', y='', title='Correction rate')
  

# Measure pass-rate of checking trials
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

pass<-df.checks %>%
  group_by(ix) %>%
  summarise(correct=sum(correct)) %>%
  filter(correct==2) %>%
  pull(ix)
df.sw<-df.sw%>%mutate(pass=if_else(ix %in% pass, 1, 0))

x<-bind_rows(
  filter(df.sw, rule_ok<1) %>% group_by(condition) %>% 
    summarise(n=n()) %>% mutate(type='bot'),
  filter(df.sw, rule_ok>0 & pass<1) %>% group_by(condition) %>% 
    summarise(n=n()) %>% mutate(type='not_pass'),
  filter(df.sw, pass>0) %>% group_by(condition) %>% 
    summarise(n=n()) %>% mutate(type='pass')) %>%
  ungroup() %>%
  mutate(condition=factor(condition, levels=c('A1','A2','A3','A4')))

ggplot(x, aes(x=condition, y=n, fill=type)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=n), position=position_stack(), vjust=-0.25) +
  scale_fill_brewer(palette="Set2") +
  theme_bw()


df.sw %>% count(condition)

# Have a look at response lables
responses<-read.csv('../data/responses_labeled.csv') %>%
  mutate(grouping=replace_na(grouping, 0)) %>%
  mutate(grouping=factor(grouping, levels=c(1,0)))
responses %>% 
  group_by(condition) %>% 
  count(reported_rule) %>%
  ggplot(aes(x=condition, y=n, fill=reported_rule)) +
  geom_bar(position="fill", stat="identity")

responses %>% 
  group_by(condition) %>% 
  count(selection_rule) %>%
  ggplot(aes(x=condition, y=n, fill=selection_rule)) +
  geom_bar(position="fill", stat="identity")

responses %>% 
  filter(features %in% c('edge', 'shade')) %>%
  group_by(condition) %>% 
  count(features) %>%
  ggplot(aes(x=condition, y=n, fill=features)) +
  geom_bar(position="fill", stat="identity")

# Grouping words
responses %>% 
  group_by(condition, grouping, pass) %>% 
  summarise(n=n()) %>%
  ggplot(aes(x=condition, y=n, fill=grouping)) +
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~pass)


# People that select different from said - are they more accurate: yes
responses %>%
  filter(selection_rule!=reported_rule) %>%
  group_by(condition) 






