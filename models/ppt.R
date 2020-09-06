
library(dplyr)

ppt<-df.tw%>%
  mutate(edges=substr(shape, 3, 3), 
         shades=case_when(color=='light' ~ 1, color=='medium' ~ 2,
                          color=='dark' ~ 3, color=='very_dark' ~ 4))%>%
  select(ix, condition, trial, edges, shades, conf)%>%
  mutate(pred=paste0(edges, shades))%>%
  select(ix, condition, trial, object=pred, conf)
ppt$conf<-as.numeric(as.character(ppt$conf))

ppt_agg<-ppt%>%group_by(condition, trial, pred)%>%summarise(n=n(), avg_conf=round(mean(conf),1))

all<-expand.grid(condition=paste0('A', seq(4)), trial=seq(5), object=all_objects)%>%
  mutate(condition=as.character(condition), object=as.character(object))%>%
  arrange(condition, trial, object)
ppt_pred<-ppt%>%group_by(condition, trial, object)%>%summarise(n=n())%>%mutate(freq=round(n/sum(n), 2))
ppt_pred<-all%>%left_join(ppt_pred, by=c('condition', 'trial', 'object'))
ppt_pred[is.na(ppt_pred)]<-0

df.ppt<-ppt_pred%>%select(group=condition, trial, object, freq)
save(df.ppt, file='../data/ppt.Rdata')






