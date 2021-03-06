
source('shared.R')

################################################################
# Generate universal effects
generate_hypo<-function() {
  pcfg<-function(s, role) {
    s<-gsub('S', sample(c('', 'and(S, S)', 'F(X)LYT'), 1), s)
    
    x_vals<-if (role=='cause') c('A', 'R') else 'M'
    s<-sub('X', sample(x_vals,1), s)
    
    f_drawn<-sample(names(feature_setting), 1)
    s<-sub('F', f_drawn, s)
    
    s<-sub('L', sample(c('==', '!=', '>', '<'), 1, prob=c(1, .2, .2, .2)), s)
    
    s<-sub('Y', sample(c('V', 'O'), 1), s)
    
    s<-sub('V', sample(feature_setting[[f_drawn]], 1), s)
    
    o_drawn<-sample(c('A', 'R'), 1)
    o_drawn_formatted<-paste0(f_drawn, '(', o_drawn, ')')
    s<-sub('O', o_drawn_formatted, s)
    
    s<-sub('T', sample(c('+1', '-1', ''), 1), s)
    
    if (grepl('S|X|F|L|Y|V|O|T',s)) return(pcfg(s, role)) else return(s)
  }
  
  # cause<-pcfg('S', 'cause')
  # effect<-pcfg('S', 'effect')
  # 
  # return(paste0("list(cause='",cause,"',effect='",effect,"')"))
  return(paste0("list(cause='',effect='", pcfg('S', 'effect'), "')"))
}

# effects<-list()
# for (i in seq(100000)) effects[[i]]<-generate_hypo()
# effects<-unique(effects)
# save(effects, file='effects.Rdata')


################################################################
# Universal evaluation
universal_effects<-function(hypo, data) {
  # Format input data
  if (typeof(data)!='list') {
    obs<-strsplit(data, ',')[[1]]
    data<-list(agent=obs[1], recipient=obs[2], result=obs[3])
  }
  
  input<-eval(parse(text=hypo))
  effect<-input$effect
  
  na_effect<-F
  if (effect=='') na_effect<-T else {
    
    effect<-gsub('A', data$agent, effect)
    effect<-gsub('R', data$recipient, effect)
    
    dist<-init_dist()
    
    for (d in names(dist)) {
      d_effect<-gsub('M', d, effect)
      cond<-eval(parse(text=d_effect))
      if (is.na(cond)|is.null(cond)) {
        na_effect<-T; break;
      } else {
        dist[[d]]<-as.numeric(cond)
      }
    }
  }
  
  if (na_effect) dist[[as.character(data$recipient)]]<-1 else dist<-normalize(dist)
  return(dist)
}


################################################################
# Group by causal equivalence
#load('effects.Rdata')
some_hypos<-effects[seq(10)]
data_strs<-all_data_str

#list_info<-data.frame(current=0)
results<-list()
for (hi in 1:length(some_hypos)) {
  h<-some_hypos[[hi]]
  results[[h]]<-list()
  for (d in data_strs) {
    x<-universal_effects(h, d)
    names(x)<-paste0(d, ',', names(x))
    results[[h]]<-c(results[[h]], x)
  }
  # # logging  
  # list_info$current<-hi
  # write.csv(list_info, 'list_info.csv')
}
#save(results, file='results_raw.Rdata')

ut<-unique(results)
ut_info<-data.frame(total=length(ut))

group_hypos<-function(i, source) {
  f<-Filter(function(x){sum(unlist(x)==unlist(ut[[i]]))==length(all_objects)*length(data_strs)}, source)
  fn<-names(f)
  return(data.frame(shortest=(fn[which(nchar(fn)==min(nchar(fn)))])[1], 
                    n=length(f), 
                    hypos=I(list(names(f)))))
}

df<-group_hypos(1, results)
for (i in 2:length(ut)) {
  # logging current line number
  # ut_info$current<-i
  # write.csv(ut_info, 'ut_info.csv')
  # save up
  df<-rbind(df, group_hypos(i, results))
  # save(df, file='effects_grouped.Rdata')
}


################################################################
# Prior
library(stringr)

pcfg_prior<-function(hypo) {
  get_prior<-function(x) {
    # count ands
    n_and<-str_count(x, 'and')
    # count ==s
    n_eq<-str_count(x, '==')
    # count sub sentence
    x<-gsub('and\\(', '', x)
    x<-gsub(' ', '', x)
    ss<-strsplit(x, ',')[[1]]
    n_drawn<-length(ss)
    n_neq<-n_drawn-n_eq
    # count relative picks
    rels<-0
    for (s in ss) {
      obj<-strsplit(s, '==|!=|>|<')[[1]][2]
      if (!is.na(obj)&nchar(obj)>5) rels<-rels+1
    }
    vals<-n_drawn-rels
    
    return((1/3)^n_and*0.625^n_eq*0.124^n_neq*0.5^rels*0.2^vals)
  }
  h<-eval(parse(text=hypo))
  return(get_prior(h$cause)*get_prior(h$effect))
}


effects_grouped<-effects_grouped%>%select(shortest, n, hypos)
effects_grouped$prior<-normalize(effects_grouped$n)
save(effects_grouped, file='../data/effects_grouped.Rdata')

# Add posterior dists to faster the sampler
tasks<-read.csv('../data/pilot_setup.csv')
hypos<-df.effects.grouped%>%select(hypo=shortest, prior)

get_hypo_posts<-function(cond, task_source=tasks, hypo_source=hypos) {
  task_obs<-tasks%>%filter(group==cond&phase=='learn')%>%select(agent, recipient, result)
  df<-hypos

  for (i in seq(nrow(task_obs))) {
    d<-paste(task_obs[i,], collapse=',')
    post_col<-paste0('post_',i)
    df[,post_col]<-mapply(get_likeli, df$hypo, rep(d, nrow(df))) # likelihoods
    df[,post_col]<-df[,post_col]*df$prior
    df[,post_col]<-normalize(df[,post_col])
  }
  
  df$group=cond
  return(df)
}

x<-get_hypo_posts('A1')
for (c in paste0('A', seq(2,4))) x<-rbind(x, get_hypo_posts(c))

df.effects.grouped<-effects_grouped
df.effects.posts<-x
save(df.effects.grouped, df.effects.posts, file='../data/effects_grouped.Rdata')


# systematically generate basic ones
g_edges<-c()
for (r in c('==', '!=', '>', '<')) {
  for (m in c('3', '4', '5', '6', '7', 
              'edges(A)', 'edges(R)', 
              'edges(A)+1', 'edges(R)+1', 'edges(A)-1', 'edges(R)-1')) {
    if (!((r=='>' & m=='7') | (r=='<' & m=='3')))
      g_edges<-c(g_edges, paste0('edges(M)', r, m))
  }
}
g_shades<-c()
for (r in c('==', '!=', '>', '<')) {
  for (m in c('1', '2', '3', '4', 
              'shades(A)', 'shades(R)', 
              'shades(A)+1', 'shades(R)+1', 'shades(A)-1', 'shades(R)-1')) {
    if (!((r=='>' & m=='4') | (r=='<' & m=='1')))
      g_shades<-c(g_shades, paste0('shades(M)', r, m))
  }
}
combos<-c()
for (e in g_edges) {
  for (s in g_shades) {
    combos<-c(combos, (paste0('and(', e, ', ', s, ')')))
  }
}


# unify them, get prior, clean up
hypos<-c(g_edges, g_shades, combos)
df.hypos<-data.frame(hypo=hypos) %>%
  mutate(hypo=paste0("list(cause='',effect='", hypo, "')"))
df.hypos$prior<-mapply(pcfg_prior, df.hypos$hypo)
df.hypos$prior<-normalize(df.hypos$prior)
save(df.hypos, file='hypos.Rdata')



