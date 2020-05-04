options("scipen" = 10)
options()$scipen

library(dplyr)

# Constants
relations<-c('=', '~', '>', '<')

features<-list()
features[['lightness']]<-paste0('l', c(1:4))
features[['sidedness']]<-paste0('p', c(3:7))

abbs<-list()
abbs[['L']]<-'lightness'
abbs[['S']]<-'sidedness'
abbs[['A']]<-'agent'
abbs[['R']]<-'recipient'
abbs[['T']]<-'result'

# Helper functions
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
read_val<-function(str, f, obs) {
  f_idx<-if (f=='L') 1 else 2
  if (nchar(str)==1) {
    return(strsplit(obs[[abbs[[str]]]], '-')[[1]][f_idx])
  } else if (nchar(str)==2) {
    return(str)
  } else {
    o<-substr(str, 1, 1)
    shift<-substr(str, 2, 3)
    o_val<-strsplit(obs[[abbs[[o]]]], '-')[[1]][f_idx]
    f_val<-as.numeric(substr(o_val, 2, 2))
    f_val<-if (shift=='++') f_val+1 else f_val-1
    # bound
    f_val<-if ((f=='L'&f_val<1)|(f=='S'&f_val<3)) f_val+1 else f_val
    return(paste0(substr(o_val, 1, 1), f_val))
  }
}
match<-function(val_1, val_2, relation) {
  v_1<-as.numeric(substr(val_1, 2, 2))
  v_2<-as.numeric(substr(val_2, 2, 2))
  if (relation=='=') {
    return((v_1-v_2)==0)
  } else if (relation=='~') {
    return((v_1-v_2)!=0)
  } else if (relation=='>') {
    return((v_1-v_2)>0)
  } else if (relation=='<') {
    return((v_1-v_2)<0)
  }
}
normalize<-function(vec) {
  total<-sum(vec)
  for (i in 1:length(vec)) vec[i]<-vec[i]/total
  return(vec)
}

# Update with learning data
check<-function(sentence, obs) {
  f<-substr(sentence, 1, 1)
  subj<-substr(sentence, 3, 3)
  rel<-substr(sentence, 4, 4)
  obj<-substr(sentence, 5, nchar(sentence)-1)
  return(match(read_val(subj, f, obs), read_val(obj, f, obs), rel))
}
pass<-function(hypo, obs) {
  causes<-strsplit((strsplit(hypo, "\\*")[[1]][1]), ',')[[1]]
  effects<-strsplit((strsplit(hypo, "\\*")[[1]][2]), ',')[[1]]
  
  is_consistent<-TRUE
  # Check whether cause conditions are met
  for (c in causes) is_consistent<-is_consistent&check(c, obs)
  if (!is_consistent) {
    return(TRUE)
  } else {
    # Check whether effect conditions are met
    if (length(effects)<2) {
      # Default to no changes
      spec_f<-substr(effects, 1, 1)
      other_f<-if (spec_f=='L') 'S' else 'L'
      effects<-c(effects, paste0(other_f, '(T=R)'))
    }
    for (e in effects) is_consistent<-is_consistent&check(e, obs)
    return(is_consistent)
  }
}
check_hypo<-function(hypo, data) {
  is_true<-FALSE
  hypos<-strsplit(hypo, '\\|')[[1]]
  for (h in hypos) is_true<-is_true|pass(h, to_list(data))
  return(is_true)
}
update<-function(df, cid, n_trials=6, src=df.learn) {
  for (i in 1:n_trials) {
    prior_col<-if (i==1) 'prior' else paste0('pp_', i-1)
    ld<-as.list(src%>%filter(cond==cid&trial==i)%>%select(agent, recipient, result))
    df[paste0('ld_', i)]<-flatten(ld)
    df[paste0('li_', i)]<-as.numeric(mapply(check_hypo, df$hypo, df[paste0('ld_', i)]))
    df[paste0('pp_', i)]<-normalize(df[prior_col]*df[paste0('li_', i)])
  }
  return(df)
}

df.test<-df.hypos%>%top_n(100)
for (i in 1:1) {
  cn<-paste0('A', i)
  df<-df.test%>%select(hypo, prior)
  df<-update(df, cn)
  df.test<-df.test%>%left_join(df%>%select(hypo, !!cn:=pp_6), by='hypo')
}

# Model predictions
all_stones<-c()
for (l in features[['lightness']]) {
  for (s in features[['sidedness']]) {
    all_stones<-c(all_stones, paste0(l, '-', s))
  }
}
df.posteriors<-data.frame(all_stones)
colnames(df.posteriors)<-c('stone')
df.posteriors$stone<-as.character(df.posteriors$stone)

check_trial<-function(cid, tid, src=df.test,
                      results=df.posteriors, 
                      gen_src=df.gen) {
  gd<-gen_src%>%filter(cond==cid&trial==tid)
  gen_results<-c()
  for (i in 1:length(all_stones)) {
    stone<-all_stones[i]
    dp<-list()
    dp[['agent']]<-gd$agent
    dp[['recipient']]<-gd$recipient
    dp[['result']]<-stone
    da<-src%>%select(hypo, pp=!!cid)%>%mutate(data=flatten(dp))
    da$pass<-as.numeric(mapply(check_hypo, da$hypo, da$data))
    da$post<-da$pp * da$pass
    gen_results<-c(gen_results, sum(da$post))
  }
  
  prob<-normalize(gen_results)
  results<-cbind(results, data.frame(prob))
  fcn<-paste0(tolower(cid), '_0', tid)
  results<-results%>%rename(!!fcn:=prob)
  
  return(results)
}

for (i in 1:5) {
  df.posteriors<-check_trial('A4', i, df.posteriors)
}

  
  
  
  
  
  







