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

pilot_start = 1
pilot_end = length(td[[1]])

subject <- td$subject[c(pilot_start:pilot_end)]
trials <- td$trials[c(pilot_start:pilot_end)]

## Treat quotation marks separately
prep_JSON <- function(str) {
  str = gsub("\\{'", '\\{"', str)
  str = gsub("':", '\":', str)
  str = gsub(": '", ': \"', str)
  str = gsub(", '", ', \"', str)
  str = gsub("',", '\",', str)
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


## Create dataframes
df.sw.aux = as.data.frame(inv_fromJSON(subject[1], FALSE))
df.tw.aux = as.data.frame(inv_fromJSON(trials[1], FALSE))
for (i in 2:pilot_end) {
  sj = as.data.frame(inv_fromJSON(subject[i], FALSE))
  tj = as.data.frame(inv_fromJSON(drop_clicks(trials[i]), FALSE))
  df.sw.aux = rbind(df.sw.aux, sj)
  df.tw.aux = rbind(df.tw.aux, tj)
}

N = max(df.tw.aux[[1]]) # 15 trials

## And append them to the id and upis
df.sw <- data.frame(ix=td$id,
                    id=td$participant)
df.sw <- cbind(df.sw, df.sw.aux)
df.tw <- cbind(ix=rep(df.sw$ix, each=N), id=rep(df.sw$id, each=N), df.tw.aux)

## Save data
save(file='../data/arc/mturk_20200112_plain.Rdata', df.sw, df.tw)
