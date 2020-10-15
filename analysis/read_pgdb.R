options("scipen" = 10)
options()$scipen

library(RPostgreSQL)
library(rjson)
library(dplyr)
rm(list=ls())

## First run this in the terminal to connect to the database:

# ssh -L 1111:localhost:5432 wwwbramleylabppl@chost4.is.ed.ac.uk

## Then you should be able to connect via an ssh port
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = 'wwwbramleylabppl_flask',
                 host = 'localhost', port = 1111,
                 password = 'testpassword',
                 user = 'wwwbramleylabppl_flaskuser')

## If its worked you should be able to detect these databases
exp = 'bn_mysterious_stones'
taskTableName = paste0(exp, '_', "task")
participantTableName = paste0(exp, '_', "participant")

dbExistsTable(con, taskTableName)
dbExistsTable(con, participantTableName)

## Then you can pull the task data from postgreSQL 
td <- dbGetQuery(con, paste("SELECT * from ", taskTableName))

# Pilot 
pilot_start = 175
pilot_end = 183

subject <- td$subject[c(pilot_start:pilot_end)]
trials <- td$trials[c(pilot_start:pilot_end)]

## Treat quotation marks separately
prep_JSON <- function(str) {
  str = gsub("\\{'", '\\{"', str)
  str = gsub("':", '\":', str)
  str = gsub(": '", ': \"', str)
  str = gsub(", '", ', \"', str)
  str = gsub("',", '\",', str)
  str = gsub("'}", '\"},', str)
  str = gsub("'}", '\"},', str)
  str = gsub('\\n', ' ', str, fixed=T)
  str = gsub("\\'", "'", str, fixed=T)
  return(str)
}

## Un-jsonify data
inv_fromJSON <- function(js, opt) {
  if (opt == TRUE) {
    js <- prep_JSON(js)
  } else {
    js <- chartr("\'\"","\"\'",js)
  }
  return(fromJSON(js))
}
# Manual cleanups
subject[3]<-gsub("didn't", 'didnt', subject[3], fixed=T)
subject[3]<-gsub("'dark'", 'dark', subject[3], fixed=T)


## Create dataframes
nsubs = length(subject);

df.sw.aux = as.data.frame(inv_fromJSON(subject[1], TRUE))
for (i in 2:nsubs) {
  sj = as.data.frame(inv_fromJSON(subject[i], TRUE))
  df.sw.aux = rbind(df.sw.aux, sj)
}
write.csv(df.sw.aux, file='main_4.csv')


df.tw.aux = (as.data.frame(inv_fromJSON(trials[1], FALSE)))
for (i in 2:nsubs) {
  #tj<-as.data.frame(trim(trials, i))
  tj = as.data.frame(inv_fromJSON(trials[i], FALSE))
  df.tw.aux = rbind(df.tw.aux, tj)
}

## And append them to the id and upis
N=24
df.sw <- data.frame(ix=td$id,
                    id=td$participant)
df.sw <- df.sw[c(pilot_start:pilot_end),]
df.sw <- cbind(df.sw, df.sw.aux)
df.tw <- cbind(ix=rep(df.sw$ix, each=N), id=rep(df.sw$id, each=N), df.tw.aux)

## Append condtions to tw, get trial id
conditions <- df.sw %>% select(ix, condition)
df.tw <- df.tw %>% left_join(conditions, by='ix')

## Formats
rownames(df.sw)<-NULL
df.sw$age<-as.numeric(as.character(df.sw$age))
df.sw$instructions_duration<-as.numeric(as.character(df.sw$instructions_duration))
df.sw$task_duration<-as.numeric(as.character(df.sw$task_duration))
df.sw$initial_certainty<-as.numeric(as.character(df.sw$initial_certainty))
df.sw$final_changed<-as.numeric(as.character(df.sw$final_changed))
df.tw$condition<-as.character(df.tw$condition)
df.tw$sid<-as.character(df.tw$sid)
## Save data
save(df.sw, df.tw, file='../data/mturk/main_batches/mturk_20201014.Rdata') 

# Combine data
b.sw<-df.sw
b.tw<-df.tw
df.sw<-rbind(df.sw, b.sw)
#df.tw<-df.tw%>%select(colnames(b.tw))
df.tw<-rbind(df.tw, b.tw)
save(df.sw, df.tw, file='../data/mturk/mturk_main.Rdata')

# Clean up duplicated lines
df.sw <- df.sw %>% arrange(ix)
rownames(df.sw)<-NULL
df.sw<-df.sw[-c(112:113),]

rownames(df.tw)<-NULL
test<-df.tw[-c(2689:2736),]
df.tw<-test

# Add rule_ok labels
labels<-read.csv('labels.csv')
df.sw<-select(df.sw,-'rule_ok')
df.sw<-df.sw%>%left_join(labels, by='token')

# Read task setups
tasks <-
  df.tw %>%
  filter(phase=='learn' | grepl('gen', sid)) %>%
  select(phase, sid, agent, recipient, result, condition) %>%
  mutate(result=if_else(phase=='gen', 0, result), 
         task=as.numeric(substr(sid, nchar(sid)-1, nchar(sid)))) %>%
  group_by(condition, phase, agent, recipient, result, task, sid) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  select(condition, phase, task, agent, recipient, result, sid)
write_csv(tasks, '../data/setup/main.csv')

