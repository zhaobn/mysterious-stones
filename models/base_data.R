options("scipen" = 10)
options()$scipen

library(dplyr)
rm(list=ls())

# Generate all possible data points
lightness<-paste0('l', c(1:4))
sidedness<-paste0('p', c(3:7))

all_stones<-vector()
for (l in lightness) {
  for (s in sidedness) {
    all_stones<-c(all_stones, paste(l, s, sep='-'))
  }
}
n_stones<-length(all_stones)

all_datapoints<-list()
for (a in 1:n_stones) {
  for (r in 1:n_stones) {
    for (e in 1:n_stones) {
      index<-(a-1)*(n_stones^2)+(r-1)*n_stones+e
      data_point<-list();
      data_point['agent']<-all_stones[a];
      data_point['recipient']<-all_stones[r];
      data_point['result']<-all_stones[e];
      all_datapoints[[index]]<-data_point
    }
  }
}
df.all<-data.frame(matrix(unlist(all_datapoints), nrow=length(all_datapoints), byrow=T))
colnames(df.all)<-c("agent", "recipient", "result")
df.all$agent<-as.character(df.all$agent)
df.all$recipient<-as.character(df.all$recipient)
df.all$result<-as.character(df.all$result)

save(df.all, file='base_data.Rdata')

# Generate task data
# https://docs.google.com/spreadsheets/d/1PVMWB7csSe05_mc6p7R1zvxiy7kfTyCWEZEPZF3uxJU/edit?usp=sharing
# sheets: learn_tasks, gen_tasks
df.learn<-read.csv('learn.csv')
save(df.all, df.learn, file='base_data.Rdata')

df.gen<-read.csv('gen.csv')
save(df.all, df.learn, df.gen, file='base_data.Rdata')

# write.csv(df.learn, file = 'learn.csv')
# write.csv(df.gen, file = 'gen.csv')


