
tasks<-read.csv('../data/test_task.csv')
load('../data/effects_grouped.Rdata') # hypothesis

# Add posterior dists to faster the sampler
hypos<-df.effects.grouped%>%select(hypo=shortest, prior)
get_hypo_posts<-function(cond, task_source=tasks, hypo_source=hypos) {
  task_obs<-tasks%>%filter(group==cond&phase=='learn')%>%select(agent, recipient, result)
  df<-hypos
  
  for (i in seq(nrow(task_obs))) {
    d<-paste(task_obs[i,], collapse=',')
    post_col<-paste0('post_',i)
    df[,post_col]<-mapply(get_likeli, df$hypo, rep(d, nrow(df))) # likelihoods
    df[,post_col]<-df[,post_col]*df$prior
    df[,post_col]<-normalize(df[,post_col])
  }
  
  df$group=cond
  return(df)
}

effects.posts<-get_hypo_posts('AT')

x<-run_gibbs_sampler('AT', 'A', 1, .1, 10000, F, effects.posts)
cats<-read_cats(x[[1]], 1000, 5)

test.results<-x
save(test.results, file='../data/test.Rdata')








