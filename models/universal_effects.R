
source('shared.R')
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
# for (i in seq(20000)) effects[[i]]<-generate_hypo()
# effects<-unique(effects)
# save(effects, file='effects.Rdata')

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



load('effects.Rdata')
some_hypos<-effects
data_strs<-all_data_str[seq(3)]

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

save(results, file='results_raw.Rdata')

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
  ut_info$current<-i
  write.csv(ut_info, 'ut_info.csv')
  # save up
  df<-rbind(df, group_hypos(i, results))
  save(df, file='hypos_grouped.Rdata')
}

################################################################
# checks ####
df$shortest<-as.character(df$shortest)
h1<-df$shortest[57]
h2<-df$hypos[57][[1]][5]

d1<-all_data_str[1]
c1<-causal_mechanism(h1, d1)
c2<-causal_mechanism(h2, d1)
unlist(c1)==unlist(c2)



