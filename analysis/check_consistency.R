
library(tidyverse)


#### Make a df with predictions and labels #### 
load('../data/mturk/mturk_main.Rdata')

cons_data = df.tw %>%
  filter(!grepl('learn', sid)) %>%
  mutate(task=as.numeric(substr(sid, nchar(sid)-1,nchar(sid)))) %>%
  select(condition, ix, task, agent, recipient, result) %>%
  arrange(condition, ix, task)
cons_data$is_consistent = NA


read_side = function(x) { floor(x/10) }
read_shade = function(x) { x%%10 }


#### Check consistency #### 

length(setdiff(unique(cons_data$ix), unique(check_result$ix)))
setdiff(unique(cons_data$ix), unique(check_result$ix))[1]

ppt_ix = 208
checker = function(obj_a, obj_r, obj_s) {
  # 177, 200, 144, 158
  # (read_side(obj_s) != read_side(obj_r)) & (read_shade(obj_s) != read_shade(obj_r))
  # 194, 208
  (read_side(obj_s) != read_side(obj_r)) | (read_shade(obj_s) != read_shade(obj_r))
  # 215
  # (read_side(obj_s)== 5) & (read_shade(obj_r) != read_shade(obj_s))
  # 216, 85
  # (read_side(obj_s) == 5) & (read_shade(obj_s) == (read_shade(obj_r)+1))
  # 135, 142, 207
  # (read_side(obj_s) == 5) & (read_shade(obj_s) == (read_shade(obj_a)+1))
  # 57, 95, 160, 166, 190
  # (read_side(obj_s)== 5) & (read_shade(obj_s) > read_shade(obj_r))
  # 81, 140, 143, 145, 183, 202
  # (read_side(obj_s)== 5)
  # 189
  # read_side(obj_s)== 4
  # 68
  # read_shade(obj_s) < read_shade(obj_r)
  # 107
  # read_shade(obj_s) > read_shade(obj_r)
  # 63
  # read_shade(obj_s) == read_shade(obj_a)
  # 105
  # (read_shade(obj_s) == 3) & (read_side(obj_s)==(read_side(obj_a)+1))
  # 120, 128, 131, 156
  # read_shade(obj_s) == 3) & (read_side(obj_s)==(read_side(obj_r)+1))
  # 146, 203, 219
  # read_side(obj_s) == (read_side(obj_a)+1)
  # 84, 86, 88, 118, 132, 172, 137, 185, 186, 206, 104, 139, 153, 182
  # read_side(obj_s) == (read_side(obj_r)+1)
  # 162, 56, 101
  # (read_side(obj_s) == (read_side(obj_a)+1)) & (read_shade(obj_s) == (read_shade(obj_r)+1))
  # 75, 83
  # (read_side(obj_s) == (read_side(obj_r)+1)) & (read_shade(obj_s) == (read_shade(obj_a)+1))
  # 137, 152
  # (read_side(obj_s) == (read_side(obj_r)+1)) & (read_shade(obj_s) > read_shade(obj_r))
  # 114
  # (read_side(obj_s) == (read_side(obj_a)+1)) & (read_shade(obj_s) > read_shade(obj_r))
  # 117
  # (read_side(obj_s) == (read_side(obj_a)+1)) & (read_shade(obj_s) == 3)
  # 165, 72, 188, 130
  # read_side(obj_s) != read_side(obj_r)
  # 213, 66
  # read_shade(obj_s) != read_shade(obj_r)
  # 75, 210
  # read_side(obj_s) > read_side(obj_r)
  # 214
  # (read_shade(obj_s) == (read_shade(obj_r)+1)) & (read_side(obj_s) == read_side(obj_a))
  # 217
  # (read_side(obj_s)== 5) & (read_shade(obj_s) > read_shade(obj_r))
  # 218
  # (read_side(obj_s) > read_side(obj_r)) & (read_shade(obj_s) > read_shade(obj_r))
  # 64
  # if (read_side(obj_r)==6) {
  #   read_side(obj_s)==8
  # } else {
  #   read_side(obj_s) == (read_side(obj_r)+1)
  # }
  # 103
  # if (read_side(obj_r)==3) {
  #   return ((read_side(obj_s)!=read_side(obj_r))&read_shade(obj_s)==3)
  # } else if (read_side(obj_r)==8) {
  #   return(read_shade(obj_s)==3)
  # } else if (read_side(obj_r)==5) {
  #   return ((read_side(obj_s)==8)&read_shade(obj_s)==3)
  # } else {
  #   return (obj_s==obj_r)
  # }
  # 87
  # if (read_side(obj_r) == 5) {
  #   read_shade(obj_s) > read_shade(obj_r)
  # } else {
  #   read_side(obj_s) == 5
  # }
  # 50
  # if (read_side(obj_a)== 5 | read_side(obj_a)==6) {
  #   read_side(obj_s) == (read_side(obj_a)+1)
  # } else if (read_side(obj_a) > read_side(obj_r)) {
  #   read_side(obj_s) > read_side(obj_r)
  # } else {
  #   FALSE
  # }
  # 53
  # if (read_side(obj_a)== 5 | read_side(obj_a)==6) {
  #   read_side(obj_s) == (read_side(obj_a)+1)
  # } else if (read_side(obj_a) == 3) {
  #   read_shade(obj_s) != read_shade(obj_r)
  # } else {
  #   FALSE
  # }
  # 62, 79
  # if (read_side(obj_a) > 3) {
  #   (read_side(obj_s) != read_side(obj_r)) & (read_shade(obj_s) != read_shade(obj_r))
  # } else {
  #   read_shade(obj_s) != read_shade(obj_r)
  # }
  # 111
  # if (read_side(obj_a) == 6) {
  #   return (read_side(obj_s) == read_side(obj_r) + 1/2*read_side(obj_a)) & (read_shade(obj_s) > read_shade(obj_r))
  # } else if (read_side(obj_a) == 3) {
  #   return (read_shade(obj_s) > read_shade(obj_r))
  # } else if (read_side(obj_a) == 5) {
  #   return (read_side(obj_s) == read_side(obj_r)+2) & (read_shade(obj_s) > read_shade(obj_r))
  # } else {
  #   return (FALSE)
  # }
  # 205
  # if (read_side(obj_a) == 3) {
  #   read_shade(obj_s) > read_shade(obj_r)
  # } else {
  #   (read_side(obj_s) != read_side(obj_r)) & read_shade(obj_s) != read_shade(obj_r)
  # }
  # 123 
  # if (read_side(obj_r)==3) {
  #   (read_side(obj_s) == 4) & (read_shade(obj_s) > read_shade(obj_r))
  # } else if (read_side(obj_r)==6) {
  #   (read_side(obj_s) == 8) & (read_shade(obj_s) > read_shade(obj_r))
  # } else if (read_side(obj_r)==5) {
  #   (read_side(obj_s) == 6) & (read_shade(obj_s) == 3)
  # } else {
  #   FALSE
  # }
  # 106
  # if (read_shade(obj_a)==1) {
  #   read_side(obj_s) == 5
  # } else {
  #   (read_side(obj_s) == 5) & (read_shade(obj_s) > read_shade(obj_r))
  # }
  # 133
  # if (read_shade(obj_a) > read_shade(obj_r)) {
  #   (read_side(obj_s) == 5) & (read_shade(obj_s) > read_shade(obj_a))
  # } else {
  #   read_side(obj_s) == 5
  # }
}




to_check = cons_data %>% filter(ix == ppt_ix)
to_check$is_consistent = mapply(checker, to_check$agent, to_check$recipient, to_check$result)
#to_check$is_consistent = TRUE # 181, 191, 76, 82, 116
#to_check$is_consistent = FALSE # 99, 155, 124

check_result = rbind(check_result, to_check) %>% arrange(condition, ix, task)
# check_result = to_check

save(check_result, file = '../data/consistency_check.Rdata')


to_fix = #c(64, 103, 87, 50, 53, 62, 79, 111)
fix_ix = 137
check_result = check_result %>% filter(ix != fix_ix)



# checker = function(obj_a, obj_r) {
#   # 51, 70
#   # 5*10 + read_shade(obj_r)
#   # 52, 54, 58, 73, 97, 159
#   # 5*10 + (read_shade(obj_r)+1) 
#   # 77
#   # if (read_side(obj_r)==3) {
#   #   40+read_shade(obj_r)
#   # } else{
#   #   obj_r
#   # }
#   # 78
#   #60+read_shade(obj_r)
# }








#### Analysis #### 

load('../data/consistency_check.Rdata')


check_result %>%
  group_by(ix) %>%
  summarise(consist = sum(is_consistent)/n()) %>%
  summarise(overall_c = sum(consist)/n(), sd = sd(consist))


check_result %>%
  group_by(ix, condition) %>%
  summarise(consist = sum(is_consistent)/n()) %>%
  filter(consist==1) %>%
  nrow()
# 27


check_result %>%
  group_by(ix, condition) %>%
  summarise(consist = sum(is_consistent)/n()) %>%
  filter(consist==0) %>%
  nrow()
# 9

check_result %>%
  group_by(ix, condition) %>%
  summarise(consist = sum(is_consistent)/n()) %>%
  ungroup() %>%
  mutate(all_consist = (consist==1)) %>%
  group_by(condition) %>%
  summarise(all_c = sum(all_consist), n=n(), perc=round(all_c/n*100, 2))

check_result %>%
  group_by(ix, condition) %>%
  summarise(consist = sum(is_consistent)/n()) %>%
  ungroup() %>%
  mutate(all_consist = (consist==0)) %>%
  group_by(condition) %>%
  summarise(all_c = sum(all_consist), n=n(), perc=round(all_c/n*100, 2))





