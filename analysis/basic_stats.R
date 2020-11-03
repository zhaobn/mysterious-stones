options("scipen" = 10)
options()$scipen

library(dplyr)
library(ggplot)
theme_set(theme_bw())
rm(list=ls())

# Print mean ± sd
ms<-function(vec) print(paste0(round(mean(vec), 2), ' ± ', round(sd(vec),2)))

# Overall checks
df.sw.all %>% filter(bot==1) %>% nrow()
df.sw.all %>% filter(rule_like==0) %>% nrow()

df.sw.all %>% filter(rule_like==1) %>% count(condition)

# Demographics
df.sw %>% count(sex)
ms(df.sw$age)
ms(df.sw$task_duration/60000)
ms(df.sw$instructions_duration/60000)

# Accuracy
bind_rows(
  filter(df.sw.all, rule_like==0|bot==1) %>% 
    group_by(condition) %>%
    summarise(acc=sum(correct), n=n()) %>%
    mutate(acc=acc/(n*18), data='excluded'),
  filter(df.sw.all, rule_like==1) %>% 
    group_by(condition) %>%
    summarise(acc=sum(correct), n=n()) %>%
    mutate(acc=acc/(n*18), data='approved')
) %>%
  mutate(data=factor(data, levels=c('excluded', 'approved')),
         condition=factor(condition, levels=c('A1','A3','A2','A4'))) %>%
  ggplot(aes(x=data, y=acc, fill=condition)) +
  geom_bar(stat='identity', position='dodge') +
  geom_hline(yintercept=1/20) +
  geom_text(aes(2.4, 1/20, label='Random = 5%'), vjust=-.8) +
  geom_text(aes(label=paste0(round(acc*100),'%')), 
            position=position_dodge(width=.9), 
            size=4,
            vjust=-.5) +
  labs(x='', y='', title='Accuracy') +
  theme(text = element_text(size=20)) +
  scale_fill_brewer(palette="Paired")

acc_data<-df.sw %>% 
  mutate(fix=if_else(condition %in% c('A1', 'A3'), 'A', 'R'),
         self_change=if_else(condition %in% c('A1', 'A2'), 'shade', 'edge')) %>%
  select(ix, correct, fix, self_change, condition)

summary(lm(correct~condition, acc_data))
summary(lm(correct~fix + self_change, acc_data))
summary(lm(correct~fix + self_change + fix * self_change, acc_data))


# Have a look at response labels
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
rm(list=ls())

responses<-read.csv('../data/responses.csv')
responses<-responses%>%
  mutate(grouping=factor(grouping, levels=c('A', 'R', 'AR', 'none')),
         condition=factor(condition, levels=c('A1', 'A3', 'A2', 'A4')),
         features.A=factor(features.A, levels=c('edge', 'shade', 'both', 'none')),
         features.R=factor(features.R, levels=c('edge', 'shade', 'both', 'none')),
         reported_rule=factor(reported_rule, levels=c(
           'true_relative', 'true_minimal', 'partial', 'wrong', 'unclear'
         ))) 

# Grouping
responses %>%
  filter(grouping!='') %>%
  ggplot(aes(x=condition, fill=grouping)) + 
  geom_bar(stat='count', position='fill') +
  labs(x='', y='') +
  scale_fill_brewer(palette="Paired")

# Features
library(ggpubr)

A<-responses %>%
  filter(!(features.A=='')) %>%
  ggplot(aes(x=condition, fill=features.A)) +
  geom_bar(stat='count', position='fill') +
  labs(x='', y='') +
  scale_fill_manual(values=c("#E69F00", "#56B4E9", "darkblue", "#999999"))

R<-responses %>%
  filter(!(features.R=='')) %>%
  ggplot(aes(x=condition, fill=features.R)) +
  geom_bar(stat='count', position='fill') +
  labs(x='', y='') +
  scale_fill_manual(values=c("#E69F00", "#56B4E9", "darkblue", "#999999"))

ggarrange(A, R, 
          labels = c("Features of A", "Features of R"),
          ncol = 2)

# Rule types
responses %>%
  ggplot(aes(x=condition, fill=reported_rule)) +
  geom_bar(stat='count', position='fill') +
  labs(x='', y='') +
  scale_fill_brewer(palette="Spectral")

# Same stats on the passed set of data
passed<-df.sw %>% filter(pass>0) %>% pull(ix)

responses %>%
  filter(grouping!='', ix %in% passed) %>%
  ggplot(aes(x=condition, fill=grouping)) + 
  geom_bar(stat='count', position='fill') +
  labs(x='', y='') +
  scale_fill_brewer(palette="Paired")

PA<-responses %>%
  filter(!(features.A==''), ix %in% passed) %>%
  ggplot(aes(x=condition, fill=features.A)) +
  geom_bar(stat='count', position='fill') +
  labs(x='', y='') +
  scale_fill_manual(values=c("#E69F00", "#56B4E9", "darkblue", "#999999"))

PR<-responses %>%
  filter(!(features.R==''), ix %in% passed) %>%
  ggplot(aes(x=condition, fill=features.R)) +
  geom_bar(stat='count', position='fill') +
  labs(x='', y='') +
  scale_fill_manual(values=c("#E69F00", "#56B4E9", "darkblue", "#999999"))

ggarrange(PA, PR, 
          labels = c("Features of A", "Features of R"),
          ncol = 2)

responses %>%
  filter(ix %in% passed) %>%
  ggplot(aes(x=condition, fill=reported_rule)) +
  geom_bar(stat='count', position='fill') +
  labs(x='', y='') +
  scale_fill_brewer(palette="Spectral")


# Separate data
df.sw.all<-df.sw
df.tw.all<-df.tw
save(df.sw.all, df.tw.all, file='../data/mturk/mturk_all.Rdata')

df.sw<-df.sw %>% filter(rule_ok>0)
df.tw<-df.tw %>% filter(id %in% df.sw$id)
save(df.sw, df.tw, file='../data/mturk/mturk_main.Rdata')

df.sw.pass<-df.sw%>%filter(pass>0)
df.tw.pass<-df.tw%>%filter(id %in% df.sw.pass$id)
save(df.sw.pass, df.tw.pass, file='../data/mturk/mturk_pass.Rdata')



