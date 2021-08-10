
library(tidyverse)
library(nnet)


load('../data/mturk/mturk_main.Rdata')
load('../data/gen_labeled.Rdata')
load('../data/labels.Rdata')

##### myst: gen data vs random ####
t.test(gen_labeled$cronbach_alpha, rep(0, nrow(gen_labeled)), alternative = 'greater', paired = T )


##### myst: gen data ####
m1<-lm(cronbach_alpha~fix+rule_change, data=gen_labeled)
m2<-lm(cronbach_alpha~fix+rule_change+fix:rule_change, data=gen_labeled)

summary(m1)
summary(m2)

lm(cronbach_alpha~total_diff+fix+rule_change, data=gen_labeled) %>% summary

t.test((filter(gen_labeled, fix=='A'))$cronbach_alpha, (filter(gen_labeled, fix=='R'))$cronbach_alpha, paired = T)
t.test((filter(gen_labeled,  rule_change=='edge'))$cronbach_alpha, (filter(gen_labeled,  rule_change=='shade'))$cronbach_alpha, paired = T)


# myst: gen~diff
lm(cronbach_alpha~total_diff, data=gen_labeled) %>% summary()
lm(cronbach_alpha~total_diff+fix+rule_change, gen_labeled) %>% summary()
lm(cronbach_alpha~total_diff+fix+rule_change+total_diff*fix+total_diff*rule_change+fix*rule_change+total_diff*fix*rule_change, gen_labeled) %>% summary()
lm(cronbach_alpha~total_diff+fix+rule_change+total_diff*fix*rule_change, gen_labeled) %>% summary()


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


glm(rule_type ~ fix + rule_change, 
    data=comp_data, family='binomial') %>% summary()
glm(rule_type ~ fix + rule_change + fix:rule_change, 
    data=comp_data, family='binomial') %>% summary()

# Rule type: causes
glm(categorization=='universal' ~ fix + rule_change, 
    data=comp_data, family='binomial') %>% summary() 
glm(categorization=='universal' ~ fix + rule_change + fix:rule_change, 
    data=comp_data, family='binomial') %>% summary() 

# Multinom
comp_data$rule_type_2 <- relevel(comp_data$rule_type_2, ref = "specific")
test <- multinom(rule_type_2 ~ fix + rule_change, data = comp_data)
z <- summary(test)$coefficients/summary(test)$standard.errors
(1 - pnorm(abs(z), 0, 1)) * 2

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


# Individual fit clusters
library(mclust)

load('../data/results/individual_best_fits.Rdata')
load('../data/labels.Rdata')
load('ind_ndp_fits.Rdata')

ind<-do.call(rbind.data.frame, individual_best_fits)
ind$ix<-rownames(ind)
rownames(ind)<-NULL
ind<-ind %>%
    mutate(ix=as.numeric(as.character(substr(ix, 2, nchar(ix))))) %>%
    select(ix, alpha, beta, gamma, base, raw_ll, fitted_ll, id) %>%
    left_join(labels, by='ix') %>%
    select(ix, condition, alpha, beta, gamma, base, raw_ll, fitted_ll, id, categorization) %>%
    mutate(
        fix=if_else(condition %in% c('A1', 'A3'), 'A', 'R'),
        fix_cond=if_else(condition %in% c('A1', 'A3'), 'Fix A', 'Fix R'),
        rule_change=if_else(condition %in% c('A1','A2'), 'edge', 'shade'),
        rule_change_cond=if_else(condition %in% c('A1','A2'), 'Rule edge(A)', 'Rule shade(A)'))

rand_bic_val=-2*(log(1/20)*16)
ind$dp_bic=4*log(16)-2*ind$fitted_ll
ind$rand_bic=rand_bic_val

dp_best<-ind %>% filter(ix %in% dp_fitted)

Y<-dp_best %>% select(alpha, beta, gamma, base)
Z<-dp_best %>% mutate(l_alpha=log(alpha),l_beta=log(beta)) %>% 
    mutate(l_beta=ifelse(l_beta==-Inf, -5, l_beta)) %>%
    select(l_alpha, l_beta, gamma)
clPairs(Y)
clPairs(Z)

BIC<-mclustBIC(Y)
plot(BIC)
summary(BIC)

mod1 <- Mclust(Y, x = BIC)
summary(mod1)

# Cluster scatter plot
dp_best %>%
    ggplot(aes(x=log(alpha), y=log(beta), 
               color=factor(gamma,levels=c(0, .25, .5, .75, 1)))) +
    geom_point() +
    scale_color_brewer(type='div', palette=6) +
    facet_wrap(~mod1) +
    labs(color='Gamma') +
    theme_bw() +
    theme(axis.text=element_text(size=18),
          axis.title = element_text(size=18),
          strip.text = element_text(size=18),
          legend.title = element_text(size=18),
          legend.text = element_text(size=18))


# Plot clusters against conditions
dp_best %>% mutate(rule_change_cond = ifelse(rule_change_cond=='Rule edge(A)', 'Rule 1', 'Rule 2')) %>%
ggplot(aes(x=rule_change_cond, fill=factor(mod1))) +
    geom_bar(stat='count', position='fill') +
    facet_grid(~fix_cond, switch = "x", scales = "free_x", space = "free_x") +
    labs(x='', y='', fill='Cluster') +
    scale_fill_brewer(palette='Set1') +
    theme(panel.spacing = unit(0, "lines"), 
          strip.background = element_blank(),
          strip.placement = "outside" ,
          legend.title = element_blank(), 
          panel.background = element_rect(fill = "transparent"),
          axis.line = element_line(colour = "black"),
          axis.text=element_text(size=18),
          strip.text.x = element_text(size=18),
          legend.text = element_text(size=18))
 

# Chi-square for randomness check
data = df.tw %>%
    filter(grepl('gen',sid)) %>%
    mutate(trial=as.numeric(substr(sid,8,9))) %>%
    mutate(result=as.character(result)) %>%
    count(condition, trial, result) %>%
    ungroup()

# from shared.R
full = expand.grid(condition=paste0('A',seq(4)), trial=seq(16), result=all_objects) %>%
    mutate(condition=as.character(condition), result=as.character(result)) %>%
    arrange(condition, trial, result)
data<-full %>% left_join(data, by=c('condition','trial','result'))

test_A1 = data %>% filter(condition=='A1') %>%
    select(-condition) %>%
    pivot_wider(names_from = result, values_from=n) %>%
    replace(is.na(.), 0)
chisq.test(test_A1)

tests = df.tw %>%
    filter(grepl('gen',sid)) %>%
    mutate(trial=as.numeric(substr(sid,8,9))) %>%
    mutate(result=as.character(result)) %>%
    select(ix, condition, trial, result) %>%
    mutate(condition=as.factor(condition), trial=as.factor(trial), result=as.factor(result))

fisher_test<-function(cond, data=tests){
    x = data%>%filter(condition==cond)
    return(fisher.test(table(x$trial, x$result), simulate.p.value=TRUE, B=10000))
}

fisher_test('A1')
fisher_test('A2')
fisher_test('A3')
fisher_test('A4')

# Multinom
comp_data$fix=factor(comp_data$fix)
comp_data$rule_change=factor(comp_data$rule_change)
comp_data$rule_type_2=relevel(comp_data$rule_type_2, ref='fuzzy')

model <- multinom(rule_type_2 ~ fix + rule_change, data=comp_data)
summary(model)
coef(model)
zvalues <- summary(model)$coefficients / summary(model)$standard.errors
pnorm(abs(zvalues), lower.tail=FALSE)*2



