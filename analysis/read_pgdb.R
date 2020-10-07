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

pilot_start = 4
pilot_end = 12

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
trim<-function(data, i) {
  js<-data[i]
  js <- chartr("\'\"","\"\'",js)
  js <- fromJSON(js)
  if (length(js[['result']])>24) {
    print(paste0(i, ' exceeding 24!'))
    js[['result']]<-js[['result']][seq(24)]
  }
  return(js)
}

## Create dataframes
df.sw.aux = as.data.frame(inv_fromJSON(subject[1], TRUE))
df.tw.aux = as.data.frame(inv_fromJSON(trials[1], FALSE))
nsubs = length(subject);
for (i in 2:nsubs) {
  sj = as.data.frame(inv_fromJSON(subject[i], TRUE))
  df.sw.aux = rbind(df.sw.aux, sj)
}
write.csv(df.sw.aux, file='pilot.csv')

for (i in 2:nsubs) {
  tj<-as.data.frame(trim(trials, i))
  # tj = as.data.frame(inv_fromJSON(trials[i], FALSE))
  df.tw.aux = rbind(df.tw.aux, tj)
}

## And append them to the id and upis
df.sw <- data.frame(ix=td$id,
                    id=td$participant)
df.sw <- df.sw[c(pilot_start:pilot_end),]
df.sw <- cbind(df.sw, df.sw.aux)
df.tw <- cbind(ix=rep(df.sw$ix, each=N), id=rep(df.sw$id, each=N), df.tw.aux)

## Append condtions to tw, get trial id
conditions <- df.sw %>% select(ix, condition)
df.tw <- df.tw %>% left_join(conditions, by='ix')
df.tw <- df.tw %>% mutate(trial=substr(taskId, 6, 6))
df.tw <- df.tw %>% select(ix, condition, trial, shape, color, conf,
                          agent_shape, agent_color, recipient_shape, recipient_color, id)

## Formats
df.sw$age<-as.numeric(as.character(df.sw$age))
df.sw$instructions_duration<-as.numeric(as.character(df.sw$instructions_duration))
df.sw$task_duration<-as.numeric(as.character(df.sw$task_duration))
df.sw$initial_certainty<-as.numeric(as.character(df.sw$initial_certainty))
df.sw$final_certainty<-as.numeric(as.character(df.sw$final_certainty))

df.tw$trial<-as.numeric(as.character(df.tw$trial))
df.tw$condition<-as.character(df.tw$condition)
## Save data
save(df.sw, df.tw, file='../data/mturk_20200418_A3A4.Rdata') # ../data/mturk_20200416_A1A2.Rdata
# load(file="../data/mturk_20200416_A1A2_18.Rdata")

# Combine data
b.sw<-df.sw
b.tw<-df.tw
df.sw<-rbind(df.sw, b.sw)
df.tw<-rbind(df.tw, b.tw)
save(df.sw, df.tw, file='../data/mturk_20200419_A.Rdata')

# Get free responses
save_free_resp <- function (cond) {
  free_resp<-df.sw%>%filter(condition==cond)%>%
    select(ix, condition, initial_input, initial_certainty, 
           final_input, final_certainty, id)
  write.csv(file=paste0("../data/free_resp_", cond, ".csv"), free_resp)
}
save_free_resp("A4")







