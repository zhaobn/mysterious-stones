
hypo<-gen_hypo()

# Check for empty ones
is_empty<-function(clause) if (nchar(clause)<3|is.na(clause)) return(T) else return(F)

# Simplify
list_clauses<-function(clause) {
  merged<-gsub('AND', '', clause)
  merged<-gsub("\\[", "", merged)
  merged<-gsub("\\]", "", merged)
  # Remove empty clauses
  m<-strsplit(merged, ',')[[1]]
  return(m[m!=''])
}
#list_clauses("AND[AND[,edges(M)~edges(A)+1],AND[edges(M)=4+1,]]")

# If cannot resolve, return F
# If can, return the resolved atomic clause
resolve_term<-function(atom) {
  rl_regex<-paste(c('[', relations, ']'),collapse='')
  atomics<-strsplit(atom, rl_regex, fixed = F)[[1]]
  atom_head<-atomics[1]
  atom_tail<-atomics[2]
  if (nchar(atom_tail)>1 & nchar(atom_tail)<4) {
    num1<-as.numeric(substr(atom_tail,1,1))
    num2<-as.numeric(substr(atom_tail,3,3))
    operator<-substr(atom_tail,2,2)
    new_tail<-if (operator=='+') num1 + num2 else num1 - num2
    
    feature<-strsplit(atom_head, '\\(')[[1]][1]
    if (new_tail < min(feature_values[[feature]])) return(F) else
      chars<-strsplit(atom,'')[[1]]
      relation<-chars[grep(rl_regex, chars)]
      return(paste(c(atom_head, relation, new_tail), collapse=''))
  } else {
    return(atom)
  }
}
#resolve_term("shades(M)<shades(A)")

tidy_me<-function(clause) {
  tidied<-c()
  sub_clauses<-list_clauses(clause)
  if (length(sub_clauses)>0) {
    for (s in sub_clauses) if (typeof(resolve_term(s))=='character') tidied<-c(tidied, resolve_term(s))
  }
  return(paste(tidied, collapse=','))
}
#tidy_me("AND[AND[shades(A)>3-1,AND[AND[shades(R)<2-1,shades(R)<2-1],AND[,edges(R)>edges(A)+1]]],AND[AND[,edges(A)>8+1],]]")

# Keep only non-contradictive ones
strip_contradictive<-function(clause) {
  fts<-c() # mentioned features
  cls<-strsplit(clause, ',')[[1]]
  for (c in cls) fts<-c(fts, strsplit(c, '\\(')[[1]][1])
  if (length(fts)>length(unique(fts))) return('') else return(clause)
}

# tests
filter_me<-function(hypo) {
  cause_clause<-strsplit(hypo, '=>')[[1]][1]
  effect_clause<-strsplit(hypo, '=>')[[1]][2]
  if (is_empty(cause_clause) & is_empty(effect_clause)) return('') else {
    cleaned_cause<-if (is_empty(cause_clause)) '' else
      strip_contradictive(tidy_me(cause_clause))
    cleaned_effect<-if (is_empty(effect_clause)) '' else
      strip_contradictive(tidy_me(effect_clause))
    if (is_empty(cleaned_cause) & is_empty(cleaned_effect)) return('') else {
      return(paste0(cleaned_cause, '=>', cleaned_effect))
    }
  }
}

x<-gen_hypo(); x; filter_me(x)





