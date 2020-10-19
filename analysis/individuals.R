
library(tidyverse)
df.sw %>% 
  filter(token=='LDW57CTI') %>% 
  select(ix) %>%
  left_join(df.tw, by='ix') %>%
  select(condition, ix, phase, tid, sid, agent, recipient, result)

df.sw %>% 
  filter(token=='W8K7NJY3') %>% 
  select(feedback)
  

