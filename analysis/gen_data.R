
library(dplyr)
library(ggplot2)


load('../data/mturk/mturk_main.Rdata')
load('../data/gen_labeled.Rdata')


# Get aggregated data
all_objects<-c()
for (s in 3:7) {
  for (c in 1:4) {
    all_objects<-c(all_objects, s*10+c)
  }
}
default<-expand.grid(
  group=paste0('A',1:4), trial=1:16, object=as.character(all_objects),
  stringsAsFactors =F)

# Sneaky: see compatible ones
# good_ixes<-labels %>% filter(rule_type!='incompatible') %>% pull(ix)
# Nope

counts<-df.tw %>%
  filter(phase=='gen'&grepl('gen', sid)) %>%
  mutate(
    group=condition,
    trial=as.numeric(substr(sid,8,9)), 
    object=as.character(result)) %>%
  group_by(group, trial, object) %>%
  summarise(count=n()) %>%
  mutate(freq=count/sum(count)) %>%
  ungroup() %>%
  full_join(default, by=c('group', 'trial', 'object')) %>%
  mutate(
    count=ifelse(is.na(count), 0, count), 
    freq=ifelse(is.na(freq), 0, freq),
    
  ) %>%
  arrange(group, trial, object)

# Plot
ggplot(counts, aes(x=object, y=trial, fill=freq)) + geom_tile() + 
  labs(x='object', y='task') +
  scale_y_continuous(trans="reverse", breaks=1:16) + 
  scale_fill_gradient(low='white', high='#293352') +
  facet_grid(~group)

# Analyze consistency with cronbach-alpha
# https://en.wikipedia.org/wiki/Cronbach%27s_alpha
get_cronbach_alpha<-function(vec) {
  sx<-var(c(1, rep(0,19)))
  k=sum(vec)
  sy<-var(vec)
  return(k/(k-1)*(1-(k*sx)/sy))
} 

get_cronbach_alpha(filter(counts, group=='A1', trial==1) %>% pull(count))

consistency<-expand.grid(
  condition=paste0('A',1:4), trial=1:16, cronbach_alpha=NA, 
  stringsAsFactors = F
) %>% 
  arrange(condition, trial)

for (i in 1:nrow(consistency)) {
  cond=consistency[i,'condition']
  tid=consistency[i, 'trial']
  count_vec<-filter(counts, group==cond, trial==tid) %>% pull(count)
  consistency[i, 'cronbach_alpha'] = get_cronbach_alpha(count_vec)
}

cond_info<-labels %>% 
  select(condition, fix, fix_cond, rule_change, rule_change_cond) %>%
  distinct()

consistency<-consistency %>%
  left_join(cond_info, by='condition')

ggplot(consistency, aes(x=condition, y=cronbach_alpha, fill=condition)) +
  geom_violin() 

ggplot(consistency, aes(x=rule_change_cond, y=cronbach_alpha, fill=condition)) +
  geom_violin() +
  facet_grid(~fix_cond, switch = "x", scales = "free_x", space = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "none") +
  labs(x='', y='', title='Cronbach alpha per trial') +
  scale_fill_brewer(palette="Paired")

lm(cronbach_alpha~fix+rule_change, data=consistency) %>% summary()
#lm(cronbach_alpha~fix+rule_change+fix*rule_change, data=consistency) %>% summary()


# Label gen task types
gen_labeled<-read.csv('../data/setup/main.csv', stringsAsFactors=F) %>%
  select(condition, phase, trial=task, agent, recipient)

get_stone_overlaps<-function(vec_a, vec_b) {
  appeared_shades<-unique(vec_a%%10)
  appeared_edges<-unique(floor(vec_a/10))
  tasked_shades<-unique(vec_b%%10)
  tasked_edges<-unique(floor(vec_b/10))
  return(2-max(tasked_edges %in% appeared_edges)-max(tasked_shades %in% appeared_shades))
}


for (i in 1:nrow(gen_labeled)) {
  cond=gen_labeled[i, 'condition']
  tid=gen_labeled[i, 'trial']
  agent=gen_labeled[i, 'agent']
  recipient=gen_labeled[i, 'recipient']
  all_tasked<-c(agent, recipient) %>% unique()
  # Get learning data
  learn_agents<-gen_labeled %>% filter(phase=='learn', condition==cond) %>% 
    pull(agent) %>% unique()
  learn_recipient<-gen_labeled %>% filter(phase=='learn', condition==cond) %>% 
    pull(recipient) %>% unique()
  all_appeared<-c(learn_agents, learn_recipient) %>% unique()
  # calculate different measurements
  gen_labeled[i, 'agent_diff']<-get_stone_overlaps(learn_agents, agent)
  gen_labeled[i, 'recipient_diff']<-get_stone_overlaps(learn_recipient, recipient)
  gen_labeled[i, 'fixed_diff']<-ifelse(cond %in% c('A1', 'A3'), gen_labeled[i, 'agent_diff'], gen_labeled[i, 'recipient_diff'])
  gen_labeled[i, 'varied_diff']<-ifelse(cond %in% c('A1', 'A3'), gen_labeled[i, 'recipient_diff'], gen_labeled[i, 'agent_diff'])
}



gen_labeled<-gen_labeled %>%
  filter(phase=='gen') %>% 
  select(condition, trial, ends_with('diff'))

gen_labeled<-gen_labeled %>% 
  left_join(consistency, by=c('condition', 'trial'))

gen_labeled$total_diff<-gen_labeled$fixed_diff+gen_labeled$varied_diff

ggplot(gen_labeled, aes(x=rule_change_cond, y=cronbach_alpha, fill=condition)) +
  geom_violin() +
  facet_grid(~fix_cond, switch = "x", scales = "free_x", space = "free_x") +
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "none") +
  labs(x='', y='', title='Cronbach alpha per trial') +
  scale_fill_brewer(palette="Paired")

gen_labeled %>% 
  filter(fix=='R') %>%
  summarise(mu=mean(cronbach_alpha), sd=sd(cronbach_alpha))

# Random baseline (simulated)
sim.random<-function(n) {
  a<-data.frame(object=all_objects, id=seq(20))
  x<-replicate(24, sample(seq(20), 1)) %>% table() %>% data.frame()
  colnames(x)<-c('id', 'n')
  x<- x %>% mutate(id=as.numeric(as.character(id)))
  a<-a %>%
    left_join(x, by='id') %>%
    mutate(n=ifelse(is.na(n), 0, n))
  return(a$n)
}
mean(replicate(100,get_cronbach_alpha(sim.random(25)))) # ~0


ggplot(gen_labeled, aes(x=total_diff, y=cronbach_alpha)) +
  geom_point() +
  facet_wrap(~condition)

gen_labeled %>%
  ggplot(aes(x=total_diff, y=cronbach_alpha, shape=condition, color=condition)) +
  geom_point(size=2) +
  geom_smooth(method = "lm", fill=NA) +
  scale_color_brewer(palette="Paired")

lm(cronbach_alpha~total_diff, data=gen_labeled) %>% summary()

ggplot(gen_labeled, aes(x=varied_diff, y=cronbach_alpha)) +
  geom_point() +
  facet_wrap(~condition)

lm(cronbach_alpha~fixed_diff+varied_diff+fixed_diff*varied_diff, 
   data=gen_labeled) %>% summary()
lm(cronbach_alpha~agent_diff+recipient_diff+agent_diff*recipient_diff, 
   data=gen_labeled) %>% summary()


save(gen_labeled, file='../data/gen_labeled.Rdata')

library(tidyr)
x<-gen_labeled %>%
  select(condition, trial, ends_with('diff'), cronbach_alpha, fix_cond, rule_change_cond) %>%
  pivot_longer(cols=ends_with('diff'), names_to='diff', values_to='value')

a<-x %>%
  filter(diff %in% c('agent_diff', 'recipient_diff')) %>%
  ggplot(aes(x=value, y=cronbach_alpha, color=diff, shape=diff)) +
  geom_point() +
  geom_smooth(method = "lm", fill=NA)

x %>%
  filter(diff %in% c('agent_diff', 'recipient_diff')) %>%
  ggplot(aes(x=value, y=cronbach_alpha, color=diff, shape=diff)) +
  geom_point() + labs(x='') +
  scale_x_continuous(breaks = c(0,1,2)) +
  geom_smooth(method = "lm", fill=NA) 


x %>%
  filter(diff %in% c('agent_diff', 'recipient_diff')) %>%
  ggplot(aes(x=value, y=cronbach_alpha, color=diff, shape=diff)) +
  geom_point() +
  geom_smooth(method = "lm", fill=NA) +
  facet_grid(fix_cond~rule_change_cond)

b<-x %>%
  filter(diff %in% c('fixed_diff', 'varied_diff')) %>%
  ggplot(aes(x=value, y=cronbach_alpha, color=diff, shape=diff)) +
  geom_point() + labs(x='') +
  scale_x_continuous(breaks = c(0,1,2)) +
  geom_smooth(method = "lm", fill=NA)


x %>%
  filter(diff %in% c('fixed_diff', 'varied_diff')) %>%
  ggplot(aes(x=value, y=cronbach_alpha, color=diff, shape=diff)) +
  geom_point() +
  geom_smooth(method = "lm", fill=NA) +
  facet_grid(fix_cond~rule_change_cond)
  


ggarrange(a, b, labels = c('A', 'B'), ncol=2, legend='bottom')



lm(cronbach_alpha~agent_diff, gen_labeled) %>% summary()
lm(cronbach_alpha~recipient_diff, gen_labeled) %>% summary()
lm(cronbach_alpha~fixed_diff, gen_labeled) %>% summary()
lm(cronbach_alpha~varied_diff, gen_labeled) %>% summary()



x %>%
  filter(diff %in% c('fixed_diff', 'varied_diff')) %>%
  ggplot(aes(x=value, y=cronbach_alpha, color=diff, shape=diff)) +
  geom_point() +
  geom_smooth(method = "lm", fill=NA)



t<-lm(cronbach_alpha~agent_diff, gen_labeled) %>% summary()
t$coefficients['agent_diff',4]











