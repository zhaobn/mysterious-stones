
#### Glabal variables ####
# configs
feature_setting<-list()
feature_setting[['edges']]<-seq(3,8)
feature_setting[['shades']]<-seq(4)

# defined according to configs
all_objects<-vector()
for (e in feature_setting$edges) {
  for (s in feature_setting$shades) {
    all_objects<-c(all_objects, paste(c(e,s), collapse=''))
  }
}

dist<-list()
for (o in all_objects) dist[[o]]<-0


all_data_str<-vector()
for (a in all_objects) {
  for (r in all_objects) {
    all_data_str<-c(all_data_str, paste(c(a,r),collapse=',')) 
  }
}

################################################################
##### PCFG ####
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
  
  cause<-pcfg('S', 'cause')
  effect<-pcfg('S', 'effect')
  
  return(paste0("list(cause='",cause,"',effect='",effect,"')"))
}

#### Evaluation ####
edges<-function(x) floor(x/10)
shades<-function(x) x%%10
and<-function(x, y) {
  if (missing(x)) x<-NA
  if (missing(y)) y<-NA
  return(x&y)
}

normalize<-function(raw) {
  if (typeof(raw)=='list') {
    sum<-Reduce('+', raw)
    if (sum==0) return(raw) else {
      for (r in names(raw)) raw[[r]]<-raw[[r]]/sum; return(raw)
    }
  } else return(raw/sum(raw))
}

causal_mechanism<-function(hypo, data) {
  # Format input data
  if (typeof(data)!='list') {
    obs<-strsplit(data, ',')[[1]]
    data<-list(agent=obs[1], recipient=obs[2], result=obs[3])
  }
  
  hypo<-gsub('A', data$agent, hypo)
  hypo<-gsub('R', data$recipient, hypo)
  
  input<-eval(parse(text=hypo))
  # Take care of empty ones
  if (input$cause==''|input$effect=='') {
    dist[[as.character(data$recipient)]]<-1
  } else {
    # Check cause conditions
    cause_cond<-eval(parse(text=input$cause))
    # Default to no change
    if (is.na(cause_cond)|!cause_cond) {
      dist[[as.character(data$recipient)]]<-1
    } else {
      na_effect<-F
      # Check for all possible result object
      for (d in names(dist)) {
        effect_text<-input$effect
        effect_text<-gsub('M', d, effect_text)
        if (is.na(eval(parse(text=effect_text)))) {
          na_effect<-T; break;
        } else {
          dist[[d]]<-as.numeric(eval(parse(text=effect_text)))
        }
      }
      if (na_effect) dist[[as.character(data$recipient)]]<-1 else dist<-normalize(dist)
    }
  }
  return(dist)
}

################################################################
# Group causally-equivalent hypothesis ####
load('hypos.Rdata')
some_hypos<-hypos[seq(10)]
data_strs<-all_data_str[seq(3)]

list_info<-data.frame(current=0)

results<-list()
for (hi in 1:length(some_hypos)) {
  h<-some_hypos[[hi]]
  results[[h]]<-list()
  for (d in data_strs) {
    x<-causal_mechanism(h, d)
    names(x)<-paste0(d, ',', names(x))
    results[[h]]<-c(results[[h]], x)
  }
  list_info$current<-hi
  write.csv(list_info, 'list_info.csv')
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









