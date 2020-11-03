
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

df.sw<-df.sw%>%
  select(names(df.sw)[seq(18)])

# Pass both check trials
strict_pass<-df.checks %>%
  group_by(ix) %>%
  summarise(correct=sum(correct)) %>%
  filter(correct==2) %>%
  pull(ix)
df.sw<-df.sw%>%mutate(strict_pass=if_else(ix %in% strict_pass, 1, 0))

# Pass at least one check trials
pass<-df.checks %>%
  group_by(ix) %>%
  summarise(correct=sum(correct)) %>%
  filter(correct>0) %>%
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









