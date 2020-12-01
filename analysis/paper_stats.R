
library(tidyverse)

load('../data/mturk/mturk_main.Rdata')
load('../data/gen_labeled.Rdata')
load('../data/labels.Rdata')

##### myst: gen data ####
lm(cronbach_alpha~fix, data=gen_labeled) %>% summary()
lm(cronbach_alpha~rule_change, data=gen_labeled) %>% summary()
lm(cronbach_alpha~fix+rule_change+fix:rule_change, data=gen_labeled) %>% summary()

# myst: gen~diff
lm(cronbach_alpha~total_diff, data=gen_labeled) %>% summary()
lm(cronbach_alpha~total_diff + fix, data=gen_labeled) %>% summary()
lm(cronbach_alpha~total_diff + fix + total_diff * fix, data=gen_labeled) %>% summary()
lm(cronbach_alpha~total_diff + rule_change, data=gen_labeled) %>% summary()

# myst: causal role diff
lm(cronbach_alpha~agent_diff, gen_labeled) %>% summary()
lm(cronbach_alpha~recipient_diff, gen_labeled) %>% summary()

# myst: variation diff
lm(cronbach_alpha~varied_diff, gen_labeled) %>% summary()
lm(cronbach_alpha~fixed_diff, gen_labeled) %>% summary()


#### Free responses ####
labels %>% count(compatible)
comp_data %>% filter(rule_type=='fuzzy_tacit') %>% count(condition)

# Rule type: effect
glm(rule_type_2=='specific' ~ fix + rule_change, 
    data=comp_data, family='binomial') %>% summary()
glm(rule_type_2=='specific' ~ fix + rule_change + fix:rule_change, 
    data=comp_data, family='binomial') %>% summary()

glm(rule_type_2=='fuzzy' ~ fix + rule_change, 
    data=comp_data, family='binomial') %>% summary()
glm(rule_type_2=='fuzzy' ~ fix + rule_change + fix:rule_change, 
    data=comp_data, family='binomial') %>% summary()


glm(rule_type_2=='tacit' ~ fix + rule_change, 
    data=comp_data, family='binomial') %>% summary()
glm(rule_type_2=='tacit' ~ fix + rule_change + fix:rule_change, 
    data=comp_data, family='binomial') %>% summary()

# Rule type: causes
glm(categorization=='universal' ~ fix + rule_change, 
    data=comp_data, family='binomial') %>% summary() 
glm(categorization=='universal' ~ fix + rule_change + fix:rule_change, 
    data=comp_data, family='binomial') %>% summary() 


# Rule type: combined
comp_data$cause_effect %>% unique()
glm(cause_effect=='categorized, fuzzy' ~ fix + rule_change, 
    data=comp_data, family='binomial') %>% summary() 
glm(cause_effect=='categorized, fuzzy' ~ fix + rule_change +fix:rule_change, 
    data=comp_data, family='binomial') %>% summary() 

glm(cause_effect=='universal, tacit' ~ fix + rule_change, 
    data=comp_data, family='binomial') %>% summary() 
glm(cause_effect=='universal, tacit' ~ fix + rule_change +fix:rule_change, 
    data=comp_data, family='binomial') %>% summary() 

glm(cause_effect=='universal, specific' ~ fix + rule_change, 
    data=comp_data, family='binomial') %>% summary() 
glm(cause_effect=='universal, specific' ~ fix + rule_change +fix:rule_change, 
    data=comp_data, family='binomial') %>% summary() 


glm(cause_effect=='categorized, specific' ~ fix + rule_change, 
    data=comp_data, family='binomial') %>% summary() 
glm(cause_effect=='categorized, specific' ~ fix + rule_change +fix:rule_change, 
    data=comp_data, family='binomial') %>% summary() 











