options("scipen" = 10)
options()$scipen

library(dplyr)
rm(list=ls())

# Constants
relations<-c('=', '~', '>', '<')

features<-list()
features[['lightness']]<-paste0('l', c(1:4))
features[['sidedness']]<-paste0('p', c(3:7))

# Helper dict
prep_abbs<-function() {
  abbs<-list()
  abbs[['L']]<-'lightness'
  abbs[['S']]<-'sidedness'
  abbs[['A']]<-'agent'
  abbs[['R']]<-'recipient'
  abbs[['T']]<-'result'
  abbs[['=']]<-c(0)
  abbs[['>']]<-c(1)
  abbs[['<']]<-c(2)
  abbs[['~']]<-c(1, 2)
  return(abbs)
}
abbs<-prep_abbs()

# Base functions
# 1. Draw relations
draw_relation<-function(pr=0.5) {
  rel<-list()
  if (runif(1) <= pr) {
    r<-'='; p<-pr
  } else {
    r<-sample(relations[relations!='='], 1); p<-(1-pr)/3
  }
  rel[[r]]<-p
  return(rel)
}
# draw_relation()

# 2. Draw subject
draw_subject<-function(sig='') {
  subj<-list()
  if (sig=='e') {
    s<-'T'; p<-1
  } else if (sig=='c') {
    s<-if (runif(1) <= 0.5) "A" else "R"; p<-1/2
  } else {
    stop('Invalid subject signature')
  }
  subj[[s]]<-p
  return(subj)
}
# draw_subject('c')

# 3. Draw object
draw_object<-function(sig='', f='', rel='', sub='', pr=0.5){
  obj<-list()
  draw_relative<-function(sig, rel, sub) {
    ro<-list()
    if (sig=='e') {
      if (rel=='=') {
        o<-sample(c('A', 'R', 'A++', 'A--', 'R++', 'R--'), 1)
        p<-1/6
      } else {
        o<-sample(c('A', 'R'), 1)
        p<-1/2
      }
    } else if (sig=='c') {
      if (sub=='A') {
        o<-if (rel=='=') sample(c('R', 'R++', 'R--'), 1) else 'R'
        p<-if (rel=='=') 1/3 else 1
      } else if (sub=='R') {
        o<-if (rel=='=') sample(c('A', 'A++', 'A--'), 1) else 'A'
        p<-if (rel=='=') 1/3 else 1
      } else stop('Invalid subject')
    }
    ro[[o]]<-p; return(ro)
  }
  draw_absolute<-function(f) {
    ra<-list()
    o<-sample(features[[f]], 1)
    ra[[o]]<-1/length(features[[f]])
    return(ra)
  }
  if (runif(1) <= pr) {
    obj<-draw_relative(sig, rel, sub)
    obj[[1]]<-obj[[1]] * pr
  } else {
    obj<-draw_absolute(f)
    obj[[1]]<-obj[[1]] * (1 - pr)
  }
  return(obj)
}
# draw_object('e', 'lightness', '=')
# draw_object('c', 'lightness', '=', 'A')

# Resursive functions
# Compose atomic sentence(s)
compose_atomics<-function(sig, ter=0.5, feature_w=0.5, relation_w=0.5, relative_w=0.5) {
  at<-list()
  
  f_idx<-if (runif(1) <= 0.5) 1 else 2 
  feature<-names(features)[f_idx]
  f<-toupper(substr(feature, 1, 1))
  
  subj<-draw_subject(sig);
  reln<-draw_relation(relation_w);
  obj<-draw_object(sig, feature, names(reln), names(subj), relative_w)
  
  sentence<-paste(c(f, "(", names(subj), names(reln), names(obj), ")"), collapse='')
  at[[sentence]]<-feature_w*subj[[1]]*reln[[1]]*obj[[1]]
  return(at)
}
# compose_atomics('c')

# Compose entailment(s)

# Compose hypothesis/es

# Debug/tests

# Results


