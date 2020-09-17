
library(dplyr)
library(ggplot2)

# Normative model vs. participant data ####
# simple pilot
ggplot(ce_preds, aes(x=object, y=trial, fill=pred)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=unique(ce_preds$trial)) + 
  scale_fill_gradient(low='white', high='#293352') +
  facet_grid(~group)


# multi plot
ppt<-df.ppt%>%mutate(source='pilot')%>%select(group, trial, object, prob=freq, source)
ue<-ueffect_preds%>%mutate(source='uni_effects')%>%select(group, trial, object, prob=pred, source)
cp<-comp_preds%>%mutate(source='cause_effects')%>%select(group, trial, object, prob=pred, source)

df<-rbind(ppt, ue, cp)
df$source<-factor(df$source, levels=c('pilot', 'cause_effects', 'uni_effects'))
ggplot(df, aes(x=object, y=trial, fill=prob)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=unique(df$trial)) + 
  scale_fill_gradient(low='white', high='#293352') +
  #scale_fill_viridis(option="E", direction=-1) + 
  theme_linedraw() +
  facet_grid(source~group)

ce_preds<-ce_preds%>%mutate(source='causal_grouped')
ug_preds<-ug_preds%>%mutate(source='un_grouped')
df<-rbind(ppt, ug_preds, ce_preds)
df$source<-factor(df$source, levels=c('pilot', 'un_grouped', 'causal_grouped'))
##################################################

# Have a look at Gibbs posterior distribution ####
states<-a1.ar.states

df<-data.frame(matrix(unlist(states), nrow=length(states), byrow=T))
df$i<-seq(nrow(df))
df<-df%>%filter(i>500)

a1.a.cats<-df%>%group_by(X1, X2, X3, X4, X5, X6)%>%summarise(n=n())
a2.a.cats<-df%>%group_by(X1, X2, X3, X4, X5, X6)%>%summarise(n=n())

a1.ar.cats<-df%>%group_by(X1, X2, X3, X4, X5, X6)%>%summarise(n=n())
a2.ar.cats<-df%>%group_by(X1, X2, X3, X4, X5, X6)%>%summarise(n=n())

# Plot sample dists
sum_cat<-function(df) {
  df<-df%>%ungroup()%>%arrange(desc(n))%>%mutate(cat=seq(nrow(df)))%>%
    # mutate(X1=as.character(X1),X2=as.character(X2), X3=as.character(X3),
    #        X4=as.character(X4),X5=as.character(X5), X6=as.character(X6))%>%
    # mutate(cat=paste0(X1,'-',X2,'-',X3,'-',X4,'-',X5,'-',X6))%>%
    select(cat, n)
  return(df)
}

x<-rbind(sum_cat(a1.a.cats)%>%mutate(group='A1_A'),
         sum_cat(a1.ar.cats)%>%mutate(group='A1_AR'),
         sum_cat(a2.a.cats)%>%mutate(group='A2_A'),
         sum_cat(a2.ar.cats)%>%mutate(group='A2_AR'))
xf<-x%>%filter(cat<11)
ggplot(xf, aes(x=cat, y=n, fill=group)) + geom_bar(position="dodge", stat="identity")

# # Try density plot
for_dense<-function(df, type) {
  dendf<-data.frame(cat=numeric(0))
  for (i in 1:nrow(df)) {
    d<-data.frame(rep(i, df[i,'n']))
    colnames(d)<-c('cat')
    dendf<-rbind(dendf, d)
  }
  dendf$group=type
  return(dendf)
}


xd<-rbind(for_dense(sum_cat(a1.a.cats), 'A1_A'),
          for_dense(sum_cat(a1.ar.cats), 'A1_AR'),
          for_dense(sum_cat(a2.a.cats), 'A2_A'),
          for_dense(sum_cat(a2.ar.cats), 'A2_AR'))
xdf<-xd%>%filter(cat<11)
ggplot(xdf, aes(x=cat, fill=group, color=group)) +
  geom_density(alpha=0.3) + 
  facet_wrap(~group)

ggplot(xd, aes(x=cat, fill=group, color=group)) +
  geom_density(alpha=0.3) + 
  facet_wrap(~group) +
  theme_minimal()

# Plot number of cats
xg<-data.frame(group=c('A1_A', 'A1_AR', 'A2_A', 'A2_AR'),
               n=c(nrow(a1.a.cats), nrow(a1.ar.cats), nrow(a2.a.cats), nrow(a2.ar.cats)))
ggplot(xg, aes(x=group, y=n, fill=group)) + geom_bar(stat='identity') +
  ylab('distinct samples')

##################################################
# Plot gibbs sampler predictions ####
x<-get_cond_preds('A1', a1.ar.states, a1.ar.funcs, alpha, beta, 'AR', 1)
ggplot(x, aes(x=object, y=trial, fill=prob)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=unique(df$trial)) + 
  scale_fill_gradient(low='white', high='#293352') +
  #scale_fill_viridis(option="E", direction=-1) + 
  theme_linedraw() 


df<-rbind(get_cond_preds('A1', a1.a.states, a1.a.funcs, alpha, beta, 'A'),
          get_cond_preds('A1', a1.ar.states, a1.ar.funcs, alpha, beta, 'AR'),
          get_cond_preds('A2', a2.a.states, a2.a.funcs, alpha, beta, 'A'),
          get_cond_preds('A2', a2.ar.states, a2.ar.funcs, alpha, beta, 'AR'))
ggplot(df, aes(x=object, y=trial, fill=prob)) + geom_tile() + 
  scale_y_continuous(trans="reverse", breaks=unique(df$trial)) + 
  scale_fill_gradient(low='white', high='#293352') +
  #scale_fill_viridis(option="E", direction=-1) + 
  theme_linedraw() +
  facet_grid(type~group)


##################################################


















