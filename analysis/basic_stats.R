options("scipen" = 10)
options()$scipen

library(dplyr)
rm(list=ls())

# Print mean ± sd
ms<-function(vec, time=F) print(paste0(round(mean(vec), 2), ' ± ', round(sd(vec),2)))

# Get filtered data
df.fs<-df.sw%>%filter(rule_ok>0)
df.bs<-df.sw%>%filter(rule_ok==0)

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

# Correction rates
sum(df.sw$correct)/(nrow(df.sw)*18) # 29%
sum(df.fs$correct)/(nrow(df.fs)*18) # 42%
sum(df.bs$correct)/(nrow(df.bs)*18) # 7% ~ 5%

df.fs %>%
  group_by(condition) %>%
  summarise(c=sum(correct), n=n()) %>%
  mutate(c_rate = c/(n*18))

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










