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

# Translate stones
sels<-df.tw%>%select(ix, cond=condition, trial, shape, color, conf)
sels<-sels%>%mutate(s=paste0(substr(shape, 1, 1), substr(shape, 3, 3)))
sels<-sels%>%mutate(l=case_when(color=='light'~'l1',
                                color=='medium'~'l2',
                                color=='dark'~'l3',
                                color=='very_dark'~'l4'))
sels<-sels%>%mutate(stone=paste0(l, '-', s))
sels<-sels%>%select(ix, condition=cond, trial, l, s, stone)
df.tw<-df.tw%>%left_join(sels, by=c('ix','condition', 'trial'))
save(df.sw, df.tw, file = '../data/mturk_20200419_A.Rdata')


















