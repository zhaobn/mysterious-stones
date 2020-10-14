options("scipen" = 10)
options()$scipen

library(dplyr)
rm(list=ls())


# Print mean ± sd
ms<-function(vec, time=F) print(paste0(round(mean(vec), 2), ' ± ', round(sd(vec),2)))

# Get filtered data
df.fs<-df.sw%>%filter(rule_ok>0)

# Condition sizes
df.sw%>%count(condition)
df.fs%>%count(condition)

# Demographics
df.sw%>%filter(sex=='female')%>%nrow() # 30/112 = 27%
df.fs%>%filter(sex=='female')%>%nrow() # 23/72 = 32%

ms(df.sw$age) # 36.48 ± 10.44
ms(df.fs$age) # 35.08 ± 9.61

# Task duration
ms(df.sw$task_duration/60000) # 9.96 ± 5.14
ms(df.fs$task_duration/60000) # 9.64 ± 4.74

# Mind-change
sum(df.sw$final_changed)/nrow(df.sw) # 57%
sum(df.fs$final_changed)/nrow(df.fs) # 42%

# Correction rates
sum(df.sw$correct)/(nrow(df.sw)*18) # 30%
sum(df.fs$correct)/(nrow(df.fs)*18) # 43%
df.fs %>%
  group_by(condition) %>%
  summarise(c=sum(correct), n=n()) %>%
  mutate(c_rate = c/(n*18))













