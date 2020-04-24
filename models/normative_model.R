options("scipen" = 10)
options()$scipen

library(dplyr)
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
effects<-c()

generate_basic_hypos<-function(feature, operator, subject, feature_vars=feature_values) {
  f<-toupper(substring(feature, 1, 1))
  hypos<-c()
  if (subject == 'T') {
    vars<-feature_vars[[feature]]
    objs<-c(vars, 'A', 'R')
    for (obj in objs) {
      hypos<-c(hypos, paste0(f, '(', subject, operator, obj, ')'))
    }
    if (operator == '=') {
      for (extra in extra_operators) {
        hypos<-c(hypos, paste0(f, '(', subject, operator, 'A', extra, ')'))
        hypos<-c(hypos, paste0(f, '(', subject, operator, 'R', extra, ')'))
      }
    }
  }
  return(hypos)
}
#ngenerate_basic_hypos('sidedness', '=', 'T')
lightness_effects<-c()
for (o in operators) {
  lightness_effects<-c(lightness_effects, generate_basic_hypos('lightness', o, 'T'))
}
sidedness_effects<-c()
for (o in operators) {
  sidedness_effects<-c(sidedness_effects, generate_basic_hypos('sidedness', o, 'T'))
}
for (l in lightness_effects) {
  for (s in sidedness_effects) {
    hypo<-paste0(l, ',', s)
    effects<-c(effects, hypo)
  }
}
causes<-rep('', length(effects))
hypos<-data.frame(causes)
hypos<-cbind(hypos, data.frame(effects))
df.tr_hypos<-hypos
df.tr_hypos$effects<-as.character(df.tr_hypos$effects)

save(df.tr_hypos, file="normative_model.Rdata")

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
  data[['result']]<-vecs[3]
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
df.tr_hypos$prior<-1/nrow(df.tr_hypos)
ld_A1<-as.list(df.learn%>%filter(cond=='A1'&trial==1)%>%select(agent, recipient, result))

#### The rigid way ####
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
save(df.tr_hypos, df.tr_hypos.a1, file='normative_model.Rdata')
# df.tr_hypos.a1$post_1<-softmax(df.tr_hypos.a1$li_1, 20)

#### Shortcuts ####
df.tr_hypos.strict.a1<-df.tr_hypos.a1
df.tr_hypos.a1<-df.tr_hypos.a1%>%select(causes, effects)

update<-function(df, group, n_trials=6, src=df.learn) {
  for (i in 1:n_trials) {
    ld<-as.list(src%>%filter(cond==group&trial==i)%>%select(agent, recipient, result))
    df[paste0('ld_', i)]<-flatten(ld)
    df[paste0('li_', i)]<-as.numeric(mapply(check_hypo, df$effects, df[paste0('ld_', i)]))
  }
  cols<-seq(4, 4+2*(n_trials-1), 2)
  df<-df%>%
    mutate(sum=rowSums(.[c(cols)]))%>%
    mutate(final=if_else(sum==n_trials, 1, 0))
  df$post<-normalize(df$final)
  return(df)
}

df.tr_hypos.a2<-df.tr_hypos%>%select(causes, effects)
df.tr_hypos.a2<-update(test, 'A2', 6)

df.tr_hypos.a3<-df.tr_hypos%>%select(causes, effects)
df.tr_hypos.a3<-update(test, 'A3', 6)

df.tr_hypos.a4<-df.tr_hypos%>%select(causes, effects)
df.tr_hypos.a4<-update(test, 'A4', 6)

p<-df.tr_hypos.a4%>%select(effects, post)
df.tr_hypos<-df.tr_hypos%>%left_join(p, by='effects')%>%
  select(causes, effects, post_a1, post_a2, post_a3, post_a4=post)
  
save(df.tr_hypos, 
     df.tr_hypos.a1, 
     df.tr_hypos.a2,
     df.tr_hypos.a3,
     df.tr_hypos.a4,
     df.tr_hypos.strict.a1, 
     file="normative_model.Rdata")

# Generalization


# Tests

