
#### Glabal variables ####
# configs
feature_setting<-list()
feature_setting[['edges']]<-seq(3,8)
feature_setting[['shades']]<-seq(4)

# helpers, defined according to configs
all_objects<-vector()
for (e in feature_setting$edges) {
  for (s in feature_setting$shades) {
    all_objects<-c(all_objects, paste(c(e,s), collapse=''))
  }
}

dist<-list()
for (o in all_objects) dist[[o]]<-0

################################################################
##### PCFG ####
generate_hypo<-function() {
  pcfg<-function(s, role) {
    s<-gsub('S', sample(c('', 'and(S, S)', 'F(X)LYT'), 1, prob=c(.1, 1, 2)), s)
    
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
    
    s<-sub('T', sample(c('+1', '-1', ''), 1, prob=c(.2, .2, 1)), s)
    
    if (grepl('S|X|F|L|Y|V|O|T',s)) return(pcfg(s, role)) else return(s)
  }
  
  cause<-pcfg('S', 'cause')
  effect<-pcfg('S', 'effect')
  
  return(paste0("list(cause='",cause,"',effect='",effect,"')"))
}

hypos<-list()
for (i in seq(10000)) hypos[[i]]<-generate_hypo()

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

causal_prediction<-function(hypo, data, type='dist') {
  hypo<-gsub('A', data$agent, hypo)
  hypo<-gsub('R', data$recipient, hypo)
  
  input<-eval(parse(text=hypo))
  cause_cond<-eval(parse(text=input$cause))
  
  # Default to no change
  if (is.null(cause_cond)) {
    dist[[as.character(data$recipient)]]<-1
  } else if (is.na(cause_cond)|!cause_cond) {
    dist[[as.character(data$recipient)]]<-1
  } else {
    # Check for all possible result object
    for (d in names(dist)) {
      effect_text<-input$effect
      effect_text<-gsub('M', d, effect_text)
      dist[[d]]<-as.numeric(eval(parse(text=effect_text)))
    }
    dist<-normalize(dist, 'list')
  }
  
  # Return desired output: prob dist or simulated prediction
  if (type=='dist') return(dist) else if (type=='sim') {
    drawn<-sample(dist, 1, prob=unlist(dist))
    return(names(drawn))
  }
}

hypo=hypos[[6]]
data=list(agent=31, recipient=32, result=31)
causal_prediction(hypos[[14]], data, 'sim')












