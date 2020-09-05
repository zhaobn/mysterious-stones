
options("scipen" = 10)
options()$scipen

source('./shared.R')

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

################################################################
# Group causally-equivalent hypothesis ####
# hypos<-list()
# for (i in seq(20000)) hypos[[i]]<-generate_hypo()
# hypos<-unique(hypos)

load('hypos.Rdata')
some_hypos<-hypos[seq(100)]
data_strs<-all_data_str[seq(3)]

#list_info<-data.frame(current=0)
results<-list()
for (hi in 1:length(some_hypos)) {
  h<-some_hypos[[hi]]
  results[[h]]<-list()
  for (d in data_strs) {
    x<-causal_mechanism(h, d)
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
# Generative prior
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
      if (nchar(obj)>5) rels<-rels+1
    }
    vals<-n_drawn-rels
    
    return((1/3)^n_and*0.625^n_eq*0.124^n_neq*0.5^rels*0.2^vals)
  }
  h<-eval(parse(text=hypo))
  return(get_prior(h$cause)*get_prior(h$effect))
}
hypos_grouped$prior_raw<-mapply(pcfg_prior, hypos_grouped$shortest)
hypos_grouped$prior<-normalize(hypos_grouped$prior_raw)
save(hypos_grouped, file='../data/hypos_grouped.Rdata')

df$shortest<-as.character(df$shortest)
effects_grouped<-df
effects_grouped$prior_raw<-mapply(pcfg_prior, effects_grouped$shortest)
effects_grouped$prior<-normalize(effects_grouped$prior_raw)
save(effects_grouped, file='../data/effects_grouped.Rdata')

################################################################
# Check whethter true rule is included
h=hypos_grouped$shortest
x=h[which(grepl('edges\\(M\\)==edges\\(A\\)\\+1', h))]
y=h[which(grepl('shades\\(M\\)==shades\\(R\\)\\+1', h))]
z=intersect(x, y) # problematic


h2=effects_grouped$shortest
x2=h2[which(grepl('edges\\(M\\)==edges\\(A\\)\\+1', h2))]
y2=h2[which(grepl('shades\\(M\\)==shades\\(R\\)\\+1', h2))]
z2=intersect(x2, y2)









