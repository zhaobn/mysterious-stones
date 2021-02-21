
library(tidyverse)

# Sample 15% responses
df<-read_sheet("1mmLKveAu3GGDAfQDxg87LrzH2kRwjCuyfKQr83mGwHw")

n<-round(nrow(df) * 0.15)
ixes<-sample(df$ix, n)

sampled<-df %>% filter(ix %in% ixes)
sampled<-sampled %>% 
  select(ix, condition, input=initial_input, bot_like=bot...6, 
         compatible, rule_type, categorization) %>%
  mutate(rule_type_a=ifelse(grepl('true', rule_type), 'specific', rule_type)) %>%
  mutate(rule_type_a=ifelse(grepl('tacit', rule_type), 'tacit', rule_type_a)) %>%
  mutate(input=as.character(input)) %>%
  select(ix, condition, input, bot_like, compatible, rule_type=rule_type_a, categorization)

# Santity checks
sampled %>% count(condition)

# Export
write_csv(sampled, 'coder.csv')
