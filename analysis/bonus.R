
library(dplyr)
workers<-read.csv('pilot_mturk.csv')

mInfo<-workers[,c(1,2,5)]
colnames(mInfo)<-c('worker_id', 'token', 'assignment_id')
taskInfo<-workers[,3:4]

info<-taskInfo%>%full_join(mInfo, by='token')
# found a mising worker token
info[7,'worker_id']<-info[10, 'worker_id']
info[7,'assignment_id']<-info[10, 'assignment_id']
info<-info[-10,]

# generate commands
for (i in 1:nrow(info)) {
  cmd<-paste0('aws mturk send-bonus ',
              '--worker-id ', info[i,'worker_id'], ' ',
              '--bonus-amount ', info[i, 'bonus'], ' ',
              '--assignment-id ', info[i, 'assignment_id'], ' ',
              '--reason "bonus for mysterious stone task"')
  print(cmd)
}









