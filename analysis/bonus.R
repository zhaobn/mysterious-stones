
library(dplyr)
workers<-read.csv('bonus_raw.csv')

mInfo<-workers[,c(1,2,3)]
colnames(mInfo)<-c('assignment_id', 'worker_id', 'token')
taskInfo<-workers[,4:5]

info<-taskInfo%>%full_join(mInfo, by='token')

# First check account balance: 
#     aws mturk get-account-balance

# Generate bonus commands
output = data.frame(aws=character(0))
for (i in 1:nrow(info)) {
  cmd<-paste0('aws mturk send-bonus ',
              '--worker-id ', info[i,'worker_id'], ' ',
              '--bonus-amount ', info[i, 'bonus'], ' ',
              '--assignment-id ', info[i, 'assignment_id'], ' ',
              '--reason "bonus for mysterious stone task"')
  output<-rbind(output, data.frame(aws=cmd))
}
write.csv(output, 'bonus')








