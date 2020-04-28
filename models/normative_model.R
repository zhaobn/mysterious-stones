options("scipen" = 10)
options()$scipen

library(dplyr)
library(ggplot2)
rm(list=ls())

# Global settings
features<-c('lightness', 'sidedness')
feature_values<-list()
feature_values[[features[1]]]<-paste0('l', c(1:4))
feature_values[[features[2]]]<-paste0('p', c(3:7))

operators<-c('=', '~', '>', '<')
extra_operators<-c('++', '--')

# Helper settings
abbs<-list()
abbs[['L']]<-'lightness'
abbs[['S']]<-'sidedness'
abbs[['A']]<-'agent'
abbs[['R']]<-'recipient'
abbs[['T']]<-'result'
abbs[['=']]<-'0'
abbs[['>']]<-'1'
abbs[['<']]<-'2'

# Helper functions
# Check if a feature value is legitimate
is_var<-function(var) {
  if ((typeof(var) != 'character') || nchar(var) != 2) {
    print('Invalid feature value input')
    return(FALSE)
  } else {
    f<-strsplit(var, '')[[1]][1]
    v<-as.numeric(strsplit(var, '')[[1]][2])
    if (f == 'l') {
      if (v > 0 && v < 5) return(TRUE) else {
        print('Lightness index exceeded')
        return(FALSE)
      }
    } else if (f == 'p') {
      if (v > 2 && v < 8) return(TRUE) else {
        print('Sidedness index exceeded')
        return(FALSE)
      }
    } else {
      print('Invalid feature type')
      return(FALSE)
    }
  }
}
# Eg. get('l1-p3', 'lightness') => 'l1'
get<-function(object, feature) {
  vars<-list()
  vars[['lightness']]<-strsplit(object, '-')[[1]][1]
  vars[['sidedness']]<-strsplit(object, '-')[[1]][2]
  if (is_var(vars[[feature]])) return(vars[[feature]])
}
# '0': var_1 == var_2; '1': var_1 > var_2; '2': var_1 < var_2
# Eg. match('l1', 'l2') => '2'
match<-function(var_1, var_2) {
  if (strsplit(var_1, '')[[1]][1] != strsplit(var_2, '')[[1]][1]) {
    print('Features do not match')
    return(FALSE)
  } else {
    if (var_1 == var_2) {
      return('0')
    } else {
      subs_1<-as.numeric(strsplit(var_1, '')[[1]][2])
      subs_2<-as.numeric(strsplit(var_2, '')[[1]][2])
      if (subs_1 > subs_2) return('1') else return('2')
    }
  }
}
# Eg. arith('l1', 'add', 2) => 'l3
arith<-function(var, type, quant) {
  if (typeof(quant) != 'double') {
    stop('Invalid quantity type')
  } else {
    feature<-strsplit(var, '')[[1]][1]
    index<-as.numeric(strsplit(var, '')[[1]][2])
    if (type == 'add') {
      index<-index + quant
    } else if (type == 'sub') {
      index<-index - quant
      if (index < 1) index <- 1
    } else {
      stop('Unknown operation type')
    }
    #if (is_var(paste0(feature, index))) return(paste0(feature, index)) else return(var)
    return(paste0(feature, index))
  }
}
# Eg. shift('l1', '++') => 'l2'
shift<-function(var, opr) {
  if (opr == '++') return(arith(var, 'add', 1))
  else if (opr == '--') return(arith(var, 'sub', 1))
  else return('Operation not found')
}

# Generate all hypothesis
all_hypotheses<-c()
# Generate hypos with trivial cause
generate_basic_hypos<-function(feature, operator, subject, feature_vars=feature_values) {
  f<-toupper(substring(feature, 1, 1))
  hypos<-c()
  
  vars<-feature_vars[[feature]]
  objs<-vars
  if (subject == 'T') objs<-c(objs, 'A', 'R')
    else if (subject == 'A') objs<-c(objs, 'R')
  
  for (obj in objs) {
    hypos<-c(hypos, paste0(f, '(', subject, operator, obj, ')'))
  }
  
  if (operator == '=') {
    extra_obj<-if (subject == 'T') c('A', 'R') else
      if (subject == 'A') c('R') else c('A') 
    for (extra in extra_operators) {
      for (ob in extra_obj) {
        hypos<-c(hypos, paste0(f, '(', subject, operator, ob, extra, ')'))
      }
    }
  }
  return(hypos)
}
# generate_basic_hypos('sidedness', '=', 'A')
get_hypo<-function(feature, subject, src, ops=operators) {
  for (o in ops) {
    src<-c(src, generate_basic_hypos(feature, o, subject))
  }
  return(src)
}
lightness_effects<-c()
lightness_effects<-get_hypo('lightness', 'T', lightness_effects)

sidedness_effects<-c()
sidedness_effects<-get_hypo('sidedness', 'T', sidedness_effects)

effects<-c()
for (l in lightness_effects) {
  for (s in sidedness_effects) {
    hypo<-paste0(l, ',', s)
    effects<-c(effects, hypo)
  }
}

df.effects<-data.frame(effects)
df.effects$effects<-as.character(df.effects$effects)

causes<-c()
as<-c(); as<-get_hypo('sidedness', 'A', as)
al<-c(); al<-get_hypo('lightness', 'A', al)
rs<-c(); rs<-get_hypo('sidedness', 'R', rs)
rl<-c(); rl<-get_hypo('lightness', 'R', rl)

causes<-c(as, al, rs, rl)
df.causes<-data.frame(causes)
df.causes$causes<-as.character(df.causes$causes)

# Checking functions
flatten<-function(list, sep=',') {
  str=c()
  for (i in 1:length(list)) str<-c(str, list[[i]])
  return(paste(str, collapse=sep))
}
to_list<-function(str, sep=',') {
  vecs<-strsplit(str, sep)[[1]]
  data<-list()
  data[['agent']]<-vecs[1]
  data[['recipient']]<-vecs[2]
  if (length(vecs) > 2) {
    data[['result']]<-vecs[3]
  }
  return(data)
}

pass<-function(sentence, obs, dict=abbs) {
  f<-substr(sentence, 1, 1)
  relation<-substr(sentence, 4, 4)
  desc<-substr(sentence, 3, nchar(sentence)-1)
  
  obj_1<-strsplit(desc, relation)[[1]][1]
  obj_2<-strsplit(desc, relation)[[1]][2]
  
  var_1<-get(obs[[dict[[obj_1]]]], dict[[f]])
  var_2<-obj_2
  if (nchar(var_2) == 1) {
    var_2<-get(obs[[dict[[obj_2]]]], dict[[f]])
  } else if (nchar(var_2) > 2) {
    obj<-substr(var_2, 1, 1)
    opr<-substr(var_2, 2, 3)
    var_2<-shift(get(obs[[dict[[obj]]]], dict[[f]]), opr)
  }
  if (relation != '~') {
    return(dict[[relation]] == match(var_1, var_2))
  } else {
    return(match(var_1, var_2) != '0')
  }
}
check_hypo<-function(hypo, data) {
  is_true<-TRUE
  hypos<-strsplit(hypo, ',')[[1]]
  for (h in hypos) {
    is_true<-is_true&pass(h, to_list(data))
  }
  return(is_true)
}
softmax<-function(vec, base=1) {
  exps<-c()
  for (i in 1:length(vec)) exps<-c(exps, exp(base*vec[[i]]))
  total<-sum(exps)
  for (i in 1:length(vec)) vec[[i]]<-exps[i]/total
  return(vec)
}
normalize<-function(vec) {
  total<-sum(vec)
  for (i in 1:length(vec)) vec[i]<-vec[i]/total
  return(vec)
}

# Learning
#### The rigid way ####
df.effects$prior<-1/nrow(df.tr_hypos)
ld_A1<-as.list(df.learn%>%filter(cond=='A1'&trial==1)%>%select(agent, recipient, result))

df.tr_hypos.a1<-df.tr_hypos
df.tr_hypos.a1['ld_1']<-flatten(ld_A1)
df.tr_hypos.a1['li_1']<-as.numeric(mapply(check_hypo, df.tr_hypos.a1$effects, df.tr_hypos.a1['ld_1']))
df.tr_hypos.a1['pr_1']<-df.tr_hypos.a1['li_1']*df.tr_hypos.a1['prior']
df.tr_hypos.a1['post_1']<-normalize(df.tr_hypos.a1['pr_1'])

for (i in 2:6) {
  ld<-as.list(df.learn%>%filter(cond=='A1'&trial==i)%>%select(agent, recipient, result))
  df.tr_hypos.a1[paste0('ld_', i)]<-flatten(ld)
  df.tr_hypos.a1[paste0('li_', i)]<-as.numeric(mapply(check_hypo, df.tr_hypos.a1$effects, df.tr_hypos.a1[paste0('ld_', i)]))
  df.tr_hypos.a1[paste0('pr_', i)]<-df.tr_hypos.a1[paste0('li_', i)]*df.tr_hypos.a1[paste0('post_', i-1)]
  df.tr_hypos.a1[paste0('post_', i)]<-normalize(df.tr_hypos.a1[paste0('pr_', i)])
}
# df.tr_hypos.a1$post_1<-softmax(df.tr_hypos.a1$li_1, 20)

#### Shortcuts ####
df.effects.a1<-df.effects%>%select(effects)

update<-function(df, group, n_trials=6, src=df.learn) {
  for (i in 1:n_trials) {
    ld<-as.list(src%>%filter(cond==group&trial==i)%>%select(agent, recipient, result))
    df[paste0('ld_', i)]<-flatten(ld)
    df[paste0('li_', i)]<-as.numeric(mapply(check_hypo, df$effects, df[paste0('ld_', i)]))
  }
  cols<-seq(3, 3+2*(n_trials-1), 2)
  df<-df%>%
    mutate(sum=rowSums(.[c(cols)]))%>%
    mutate(final=if_else(sum==n_trials, 1, 0))
  df$post<-normalize(df$final)
  return(df)
}
df.effects.a1<-update(df.effects.a1, 'A1', 6)

df.effects.a2<-df.effects%>%select(effects)
df.effects.a2<-update(df.effects.a2, 'A2', 6)

df.effects.a3<-df.effects%>%select(effects)
df.effects.a3<-update(df.effects.a3, 'A3', 6)

df.effects.a4<-df.effects%>%select(effects)
df.effects.a4<-update(df.effects.a4, 'A4', 6)

p<-df.tr_hypos.a4%>%select(effects, post)
df.effects<-df.effects%>%left_join(p, by='effects')%>%
  select(effects, post_a1, post_a2, post_a3, post_a4=post)

# Checks for causes
df.causes<-data.frame(causes)
df.causes$causes<-as.character(df.causes$causes)

update_causes<-function(cid, n_trials=6, src=df.learn, tar=df.causes) {
  df<-tar%>%select(causes)
  for (i in 1:n_trials) {
    ld<-as.list(src%>%filter(cond==cid&trial==i)%>%select(agent, recipient, result))
    df[paste0('ld_', i)]<-flatten(ld)
    df[paste0('li_', i)]<-as.numeric(mapply(check_hypo, df$causes, df[paste0('ld_', i)]))
  }
  cols<-seq(3, 3+2*(n_trials-1), 2)
  df<-df%>%
    mutate(sum=rowSums(.[c(cols)]))%>%
    mutate(final=if_else(sum==n_trials, 1, 0))%>%
    rename(!!cid:=final)
  tar<-cbind(tar, df[cid])
  return(tar)
}

for (i in 1:4) {
  #df.causes<-update_causes(paste0('A', i))
  print(sum(df.causes[paste0('A', i)]))
}
# save(df.causes, df.effects, df.effects_gen, file='normative_model.Rdata')

# Generalization
all_stones<-c();
for (l in feature_values[['lightness']]) {
  for (s in feature_values[['sidedness']]) {
    all_stones<-c(all_stones, paste0(l, '-', s))
  }
}
df.posteriors<-data.frame(all_stones)
colnames(df.posteriors)<-c('stone')
df.posteriors$stone<-as.character(df.posteriors$stone)

check_trial<-function(cid, tid, 
                      results=df.posteriors, 
                      gen_src=df.gen,
                      causes_src=df.causes,
                      effects_src=df.effects) {
  causes<-c('', causes_src[causes_src[cid]>0, 'causes'])
  ecol<-paste0('post_', tolower(cid))
  effects<-effects_src[effects_src[ecol]>0, 'effects']
  
  dc<-data.frame(causes)
  de<-data.frame(effects)
  da<-merge(dc, de)
  da$causes<-as.character(da$causes)
  da$effects<-as.character(da$effects)
  
  gen_trial<-gen_src%>%filter(cond==cid&trial==tid)%>%select(agent, recipient)
  for (i in 1:length(all_stones)) {
    da<-check_stone(all_stones[i], flatten(gen_trial), da)
  }
  
  gen_results<-c()
  for (i in 1:length(all_stones)) {
    stone<-all_stones[i]
    checks<-da%>%select(causes, effects, !!stone)
    gen_results<-c(gen_results, sum(checks[stone]))
  }
  
  prob<-normalize(gen_results)
  results<-cbind(results, data.frame(prob))
  fcn<-paste0(tolower(cid), '_0', tid)
  results<-results%>%rename(!!fcn:=prob)
  
  return(results)
}

check_stone<-function(stone, gen, src) {
  df<-src%>%select(causes, effects)
  r_stone<-strsplit(gen, ',')[[1]][2]
  df$data<-paste0(gen, ',', stone)
  df$pass_c<-as.numeric(mapply(check_hypo, df$causes, df$data))
  df$pass_e<-as.numeric(mapply(check_hypo, df$effects, df$data))
  df<-df%>%mutate(pass=if_else(pass_c>0&pass_e>0, 1, 
                            if_else(pass_c<1&!!stone==!!r_stone, 1, 0)))
  
  df<-df%>%select(causes, effects, !!stone:=pass)
  src<-src%>%left_join(df, by=c('causes', 'effects'))
  return(src)
}


for (i in 1:5) {
  df.posteriors<-check_trial('A4', i, df.posteriors)
}

save(file="normative_model.Rdata", 
     df.causes, df.effects, df.posteriors)

save(file="intermediate.Rdata",
     df.effects.a1, df.effects.a2,
     df.effects.a3, df.effects.a4)

# Test new trials
df.test<-read.csv('learn_test.csv')
df.test$cond<-as.character(df.test$cond)
df.test$agent<-as.character(df.test$agent)
df.test$recipient<-as.character(df.test$recipient)
df.test$result<-as.character(df.test$result)

test.learn<-df.test%>%filter(cond=='A2')
test.gen<-df.gen%>%filter(cond=='A2')
test.causes<-df.causes%>%select(causes, orig_a2=A2)
test.effects<-df.effects%>%select(effects, orig_a2=post_a2)

x<-test.causes%>%select(causes)
x<-update_causes('A2', 6, test.learn, x)
test.causes<-test.causes%>%left_join(x, by='causes')%>%
  select(causes, orig_a2, A2)

test.effects.a2<-test.effects%>%select(effects)
test.effects.a2<-update(test.effects.a2, 'A2', 6, test.learn)
test.effects<-test.effects%>%left_join(test.effects.a2, by='effects')%>%
  select(effects, orig_a2, post_a2=post)

test.posteriors<-df.posteriors%>%select(stone, starts_with('a2'))
y<-test.posteriors%>%select(stone)
y<-check_trial('A2', 1, y, test.gen, test.causes, test.effects)

for (i in 2:5) {
  y<-check_trial('A2', i, y, test.gen, test.causes, test.effects)
}
cns<-paste0('test_a2_0', seq(1, 5))
cns<-c('stone', cns)
colnames(y)<-cns
test.posteriors<-test.posteriors%>%left_join(y, by='stone')

# Plot
tid<-'a2_01'
ttid<-paste0('test_', tid)

df$type<-factor(df$type, levels = c('used', 'test'))
df<-test.posteriors%>%select(stone, post=!!tid)%>%mutate(type='used')
df<-rbind(df, test.posteriors%>%select(stone, post=!!ttid)%>%mutate(type='test'))
ggplot(df, aes(fill=type, y=post, x=stone)) + 
  geom_bar(position="dodge", stat="identity")

save(test.causes, test.effects, test.posteriors, file='tests.Rdata')




