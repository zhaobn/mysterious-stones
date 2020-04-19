options("scipen" = 10)
options()$scipen

library(dplyr)
rm(list=ls())

## Results ####################
## https://docs.google.com/spreadsheets/d/1PVMWB7csSe05_mc6p7R1zvxiy7kfTyCWEZEPZF3uxJU/edit?usp=sharing
###############################

# Basic stats
df.sw%>%group_by(condition)%>%tally()
df.sw%>%filter(sex=='female')%>%group_by(condition)%>%tally()
df.sw%>%group_by(condition)%>%summarise(age_m=mean(age), age_sd=sd(age))
df.sw%>%group_by(condition)%>%summarise(it_m=mean(instructions_duration/60000), tt_m=mean(task_duration/60000))

df.sw%>%group_by(condition)%>%summarise(ic_m=mean(initial_certainty), ic_sd=sd(initial_certainty))
df.sw%>%group_by(condition)%>%summarise(fc_m=mean(final_certainty), fc_sd=sd(final_certainty))

df.sw%>%filter(final_certainty>initial_certainty)%>%group_by(condition)%>%tally()
df.sw%>%filter(final_certainty<initial_certainty)%>%group_by(condition)%>%tally()
df.sw%>%filter(final_certainty==initial_certainty)%>%group_by(condition)%>%tally()

# Trial
save_trial_combos<-function(cond) {
  combos<-df.tw%>%filter(condition=='A1')%>%count(trial, color, shape)
  write.csv(combos, file=paste0("../data/combos_", cond, ".csv"))
}
save_trial_combos("A4")



