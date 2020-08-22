
#### Global settings ####
feature_values<-list()
feature_values[['edges']]<-paste(seq(3,8))
feature_values[['shades']]<-c('l', 'm', 'd', 'v')

features<-names(feature_values)

relations<-c('=', '~', '>', '<')
addon<-c('+1', '-1')

#### Grammar ####
H<-function(role) return(sample(c(empty(), AND(role), form(role)), 1))

empty<-function() return('')

AND<-function(role) {
  if (runif(1)>0.5) {
    funcs<-c(empty(), form(role))
    return(paste0("AND(",sample(funcs, 1), ",", sample(funcs, 1), ")"))
  } else {
    return(paste0("AND(",H(role), ",", H(role), ")"))
  }
}

form<-function(role) {
  obj<-if (role=='c') sample(c('A','R'), 1) else 'M'
  f<-draw_feature()
  r<-draw_relation()
  o<-draw_obj(f)
  t<-draw_term()
  return(paste0(c(f, '(', obj, ')', r, o, t), collapse=''))
}

draw_feature<-function() sample(features, 1)
draw_relation<-function() sample(relations, 1)
draw_obj<-function(feature) {
  if (runif(1) > 0.5) {
    return(sample(feature_values[[feature]], 1))
  } else {
    rel_o<-sample(c('A', 'R'), 1) 
    #if (role=='e') sample(c('A', 'R'), 1) else sample(setdiff(c('A', 'R'), obj), 1)
    return(paste0(c(feature, '(', rel_o, ')'), collapse=''))
  }
}
draw_term<-function() sample(c(addon, empty()), 1)

# Final comsumer
gen_hypo<-function() {
  hypo<-paste0(c(H('c'), '=>', H('e')), collapse='')
  return(hypo)
}

# tests
gen_hypo()








