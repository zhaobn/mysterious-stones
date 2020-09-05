
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

init_dist<-function() {
  dist<-list()
  for (o in all_objects) dist[[o]]<-0
  return(dist)
}
dist<-init_dist()


all_data_str<-vector()
for (a in all_objects) {
  for (r in all_objects) {
    all_data_str<-c(all_data_str, paste(c(a,r),collapse=',')) 
  }
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

listify_data<-function(data_str) {
  if (typeof(data_str)=='list') return(data_str) else {
    obs<-strsplit(data_str, ',')[[1]]
    return(list(agent=obs[1], recipient=obs[2], result=obs[3]))
  }
}

causal_mechanism<-function(hypo, data) {
  data<-listify_data(data)
  
  hypo<-gsub('A', data$agent, hypo)
  hypo<-gsub('R', data$recipient, hypo)
  
  input<-eval(parse(text=hypo))
  effect<-input$effect
  dist<-init_dist()
  
  if (input$cause=='') {
    set_default<-if (effect=='') T else F
  } else {
    cause_cond<-eval(parse(text=input$cause))
    set_default<-if (is.na(cause_cond)|!cause_cond) T else F
  }
  
  if (!set_default) {
    for (d in names(dist)) {
      effect_text<-gsub('M', d, effect)
      cond<-eval(parse(text=effect_text))
      if (is.na(cond)|is.null(cond)) {
        set_default<-T; break;
      } else {
        dist[[d]]<-as.numeric(cond)
      }
    }
  }
  
  if (set_default) {
    dist[[as.character(data$recipient)]]<-1
  } else {
    dist<-normalize(dist)
  }
  
  return(dist)
}

get_likeli<-function(hypo, data) {
  data<-listify_data(data)
  return(causal_mechanism(hypo, data)[[as.character(data$result)]])
}



