
library(dplyr)
library(ggplot2)

# Read labels
library(googlesheets4)
labels<-read_sheet("1mmLKveAu3GGDAfQDxg87LrzH2kRwjCuyfKQr83mGwHw")
labels$initial_input<-as.character(labels$initial_input)
save(labels, file='../data/labels.Rdata')

load('../data/mturk/mturk_all.Rdata')
summary(df.sw.all)

df.sw %>% filter(token=='JIE9BMZJ') %>% select(correct)
df.tw %>% filter(ix==190) %>% select(tid, agent, recipient, result)

# Get all free responses
res<-df.sw.all %>% select(ix, token, condition, initial_input)
write.csv(res, file='../data/responses/all_responses.csv', )



df.sw.all<-df.sw.all %>%
  select(setdiff(names(df.sw.all), c('bot', 'rule_like')))
df.sw.all<-df.sw.all %>%
  left_join(select(labels, token, bot, rule_like), by='token')

save(df.sw.all, df.tw.all, file='../data/mturk/mturk_all.Rdata')

df.sw<-df.sw.all%>%filter(rule_like==1)
df.tw<-df.tw.all%>%filter(ix %in% df.sw$ix)
save(df.sw, df.tw, file='../data/mturk/mturk_main.Rdata')

df.sw.pass<-df.sw%>%filter(pass==1)
df.tw.pass<-df.tw%>%filter(ix %in% df.sw.pass$ix)
save(df.sw, df.tw, file='../data/mturk/mturk_pass.Rdata')


# Analyze label data
labels<-labels %>%
  filter(rule_like==1) %>%
  mutate(fix=if_else(condition %in% c('A1', 'A3'), 'A', 'R'),
         fix_cond=if_else(condition %in% c('A1', 'A3'), 'Fix A', 'Fix R'),
         rule_change=if_else(condition %in% c('A1','A2'), 'edge', 'shade'),
         rule_change_cond=if_else(condition %in% c('A1','A2'), 'Rule edge(A)', 'Rule shade(A)')) %>%
  mutate(rule_type=factor(rule_type, 
           levels=c(
             'true_relative', 'true_constant', 'vague',
             'sophisticated', 'partial', 'incompatible')),
         categorization=factor(categorization, levels=c('A', 'R', '')))

# Rule type
ggplot(labels, aes(x=rule_change_cond, fill=rule_type)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(~fix_cond, switch = "x", scales = "free_x", space = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside") +
  labs(x='', y='', title='Rule type') +
  scale_fill_brewer(palette="Paired")

# test<-labels
# test$true_constant = test$rule_type=='true_constant' 
# glm(true_constant~fix+self_change+fix*self_change, 
#     filter(test, rule_like==1),
#     family='binomial') %>% 
#   summary()
glm(rule_type=='true_constant'~fix+rule_change, 
    filter(labels, rule_like==1),
    family='binomial') %>% 
  summary()

glm(rule_type=='true_constant'~fix+rule_change+fix*rule_change, 
    filter(labels, rule_like==1),
    family='binomial') %>% 
  summary()

glm(rule_type %in% c('true_relative', 'true_constant')~fix+rule_change,
    filter(labels, rule_like==1),
    family='binomial') %>% 
  summary()

glm(rule_type %in% c('true_relative', 'true_constant')~fix+rule_change+fix*rule_change,
    filter(labels, rule_like==1),
    family='binomial') %>% 
  summary()

glm(rule_type %in% c('true_relative', 'true_constant')~fix+self_change,
    filter(labels, rule_like==1),
    family='binomial') %>% 
  summary()

glm(rule_type %in% c('true_relative', 'true_constant')~fix+self_change+fix*self_change,
    filter(labels, rule_like==1),
    family='binomial') %>% 
  summary()

glm(rule_type %in% c('vague', 'partial', 'sophisticated')~fix+self_change+fix*self_change,
    filter(labels, rule_like==1),
    family='binomial') %>% 
  summary()


glm(rule_type=='vague'~fix+self_change, 
    filter(labels, rule_like==1),
    family='binomial') %>% 
  summary()

# Rule cats
labels <- labels %>%
  mutate(rule_type=as.character(rule_type)) %>%
  mutate(rule_cat=case_when(
    grepl('true', rule_type) ~ 'true_rules',
    rule_type=='incompatible' ~ rule_type,
    TRUE ~ 'other'
  )) %>%
  mutate(rule_cat=factor(rule_cat, levels=c('true_rules', 'other', 'incompatible')))

ggplot(labels, aes(x=rule_change_cond, fill=rule_cat)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(~fix_cond, switch = "x", scales = "free_x", space = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside") +
  labs(x='', y='', title='Rule type') +
  scale_fill_brewer(palette="Paired")

glm(rule_cat=='true_rules'~fix+rule_change, 
    filter(labels, rule_like==1),
    family='binomial') %>% 
  summary()

glm(rule_cat=='true_rules'~fix+rule_change+fix*rule_change, 
    filter(labels, rule_like==1),
    family='binomial') %>% 
  summary()

# # Or
# labels <- labels %>%
#   mutate(rule_type=as.character(rule_type)) %>%
#   mutate(rule_cat2=case_when(
#     grepl('true', rule_type)|rule_type=='sophisticated' ~ 'accurate',
#     rule_type=='vague' ~ rule_type,
#     TRUE ~ 'incompatible'
#   )) %>%
#   mutate(rule_cat2=factor(rule_cat2, levels=c('accurate', 'vague', 'incompatible')))
# 
# ggplot(filter(labels, rule_like==1), aes(x=condition, fill=rule_cat2)) +
#   geom_bar(stat='count', position='fill') +
#   labs(x='', y='', title='Rule cat') +
#   scale_fill_manual(values=c("#E69F00", "#56B4E9", "#999999"))
# 
# glm(rule_cat2=='vague'~fix+self_change, 
#     filter(labels, rule_like==1),
#     family='binomial') %>% 
#   summary()


# Categorization
ggplot(labels, aes(x=rule_change_cond, fill=categorization)) + 
  geom_bar(stat="count", position="fill") +
  facet_grid(~fix_cond, switch = "x", scales = "free_x", space = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside") +
  labs(x='', y='', title='Rule type') +
  scale_fill_brewer(palette="Paired")
  
m1<-glm(categorization!=''~fix+rule_change, 
    filter(labels, rule_like==1),
    family='binomial') 

glm(categorization~fix+self_change, 
    filter(labels, rule_like==1),
    family='binomial') 


m2<-glm(categorization!=''~fix+self_change+fix*self_change, 
    filter(labels, rule_like==1),
    family='binomial') 


anova(m1, m2)


# Rule_type + categorization
mix<-labels %>%
  filter(rule_like==1 & rule_type!='incompatible') %>%
  mutate(mix_type=case_when(
    categorization!='' & grepl('true', rule_type) ~ 'redundant',
    categorization!='' & rule_type=='vague' ~ 'complex',
    categorization=='' & grepl('true', rule_type) ~ 'exact',
    categorization=='' & rule_type=='vague' ~ 'broad',
    T ~ '')) %>%
  filter(mix_type!='') %>%
  mutate(mix_type=factor(mix_type, levels=c('exact', 'redundant', 'broad', 'complex')))

ggplot(data = mix, aes(x=rule_change_cond, fill = mix_type)) + 
  geom_bar(stat = "count", position = 'fill') +
  facet_grid(~fix_cond, switch = "x", scales = "free_x", space = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside") +
  labs(x='', y='', title='Rule type x categorization') +
  scale_fill_brewer(palette="Paired")

glm(mix_type=='complex'~fix+rule_change, 
    data=mix, family='binomial') %>% 
  summary()

glm(mix_type=='general'~fix+rule_change, 
    data=mix, family='binomial') %>% 
  summary()

glm(mix_type=='exact'~fix+rule_change, 
    data=mix, family='binomial') %>% 
  summary()

glm(mix_type=='redundant'~fix+rule_change, 
    data=mix, family='binomial') %>% 
  summary()

# Features mentioned
vfeats<-bind_rows(
  labels %>% filter(rule_type=='vague') %>% 
    select(ix, condition, source=shade, fix_cond, rule_change_cond) %>%
    mutate(feature='edge'),
  labels %>% filter(rule_type=='vague') %>% 
    select(ix, condition, source=edge, fix_cond, rule_change_cond) %>%
    mutate(feature='shade')
) %>%
  mutate(source=factor(source, levels = c(
    'A', 'R', 'constant', 'multiple', 'general', 'unstated'
  )))

vfeats %>%
  ggplot(aes(x=condition, fill=source)) +
  geom_bar(stat='count', position='fill') +
  facet_wrap(~feature) +
  labs(x='', y='', title='Effect sources') +
  scale_fill_brewer(palette='Paired') +
  theme(text = element_text(size=20))



ggplot(vfeats, aes(x=rule_change_cond, fill=source)) + 
  geom_bar(stat="count", position = 'fill') +
  facet_grid(fix_cond~feature, switch = "x", scales = "free_x", space = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside") +
  labs(x='', y='', title='M feature reference') +
  scale_fill_brewer(palette="Paired")


# Review request - accuracy
labels %>% 
  group_by(condition) %>%
  summarise(n=n(), 
    groundtruth=sum(rule_type=='true_relative'), 
    altertruth=sum(rule_type=='true_constant')) %>%
  ungroup() %>%
  mutate(
    condition=paste0('B', substr(condition, 2, 2)),
    alltrue=groundtruth+altertruth) %>%
  mutate(
    ground_tr_rate=round(groundtruth/n, 2),
    alter_tr_rate=round(altertruth/n, 2),
    all_tr_rate=round(alltrue/n, 2)) %>%
  select(condition, ground_tr_rate, alter_tr_rate, all_tr_rate)











