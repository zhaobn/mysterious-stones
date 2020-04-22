options("scipen" = 10)
options()$scipen

library(dplyr)
rm(list=ls())

# Generate all possible data points
darkness<-paste0('d', c(1:4))
sideness<-paste0('p', c(3:7))

all_stones<-vector()
for (d in darkness) {
  for (s in sideness) {
    all_stones<-c(all_stones, paste(d, s, sep='-'))
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

save(df.all, file='base_data.Rdata')

# Generate task data
# https://docs.google.com/spreadsheets/d/1PVMWB7csSe05_mc6p7R1zvxiy7kfTyCWEZEPZF3uxJU/edit?usp=sharing
# sheets: learn_tasks, gen_tasks
df.learn<-read.csv('learn_tasks.csv')
save(df.all, df.learn, file='base_data.Rdata')

df.gen<-read.csv('gen_tasks.csv')
save(df.all, df.learn, df.gen, file='base_data.Rdata')

