options("scipen" = 10)
options()$scipen

library(dplyr)
library(ggplot2)
library(GA)

#### Fit softmax and report log likelihood #### 
data_likeli<-function(data, b) {
  ds<-filter(data, group=='A1', trial==1) %>% mutate(softmaxed=softmax(prob, b))
  for (c in 1:4) {
    for (i in 1:16) {
      if (!(c==1 & i==1)) {
        ds<-rbind(ds,
                  filter(data, group==paste0('A', c), trial==i) %>% 
                    mutate(softmaxed=softmax(prob, b)))
      }
    }
  }
  ds<-ds%>%filter(count>0)
  ll<-sum(ds$count*log(ds$softmaxed))
  return(-ll)
}

#### On main data #### 
counts<-df.tw %>%
  filter(phase=='gen'&grepl('gen', sid)) %>%
  mutate(
    group=condition,
    trial=as.numeric(substr(sid,8,9)), 
    object=as.character(result)) %>%
  group_by(group, trial, object) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  right_join(ce_preds, by=c('group', 'trial', 'object')) %>%
  mutate(count=ifelse(is.na(count), 0, count)) %>%
  select(group, trial, object, count, prob=pred)

## Random baseline
102*16*log(1/20)
-2*-4841.103

## Normative model
out<-optim(par=0, fn=data_likeli, data=counts, method='Brent', lower=0, upper=100)
out$par #3.19
out$value #3706.359
2*log(102)-2*-3706.359 # 7421.948

## Fitted results
dpa_grid_2 %>% arrange(fitted_ll) %>% head(1)
3*log(102)-2*-3515

dpr_grid_2 %>% arrange(fitted_ll) %>% head(1)
3*log(102)-2*-3460

dpar_grid_2 %>% arrange(fitted_ll) %>% head(1)
3*log(102)-2*-3453


#### On pass check trials data ####

## Random baseline
53*16*log(1/20)
-2*53*16*log(1/20)

## Normative
counts<-df.tw.pass %>%
  filter(phase=='gen'&grepl('gen', sid)) %>%
  mutate(
    group=condition,
    trial=as.numeric(substr(sid,8,9)), 
    object=as.character(result)) %>%
  group_by(group, trial, object) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  right_join(ce_preds, by=c('group', 'trial', 'object')) %>%
  mutate(count=ifelse(is.na(count), 0, count)) %>%
  select(group, trial, object, count, prob=pred)

out<-optim(par=0, fn=data_likeli, data=counts, method='Brent', lower=0, upper=100)
out$par
out$value
2*log(53)-2*-out$value

## Models
dpa_pass_grid_2 %>% arrange(fitted_ll) %>% head(1)
3*log(53)-2*-1365

dpr_pass_grid_2 %>% arrange(fitted_ll) %>% head(1)
3*log(53)-2*-1403

dpar_pass_grid_2 %>% arrange(fitted_ll) %>% head(1)
3*log(53)-2*-1405



## Plot
library(GA)

alphas<-c(1:10, 2^(4:10))
betas<-c(seq(0,1,.1), 2^(1:10))

plot_sensitivity<-function(data, base, col) {
  fits<-matrix(nrow=length(alphas), ncol=length(betas))
  for (i in 1:length(alphas)) {
    for (j in 1:length(betas)) {
      val<-filter(data, alpha==alphas[i], beta==betas[j])
      if (nrow(val)>0) {
        fits[i,j]<-filter(data, alpha==alphas[i], beta==betas[j])%>%pull(col)
        #if (fits[i,j]==-Inf) fits[i,j]=base
      } else {
        fits[i,j]=base 
      }
      if (fits[i,j]>0) fits[i,j]=-fits[i,j]
    }
  }
  
  return(persp3D(alphas, betas, fits, theta=50, phi=20, expand=1, col.palette=topo.colors))
}

plot_sensitivity(dp25_grid, -4841.103, 'raw_ll')
plot_sensitivity(dpar_grid, -4841.103, 'raw_ll')

plot_sensitivity(dpa_grid, -4841.103, 'fitted_ll')
plot_sensitivity(dpar_grid, -4841.103, 'fitted_ll')

# Debug fitts
x<-dpa_raw_preds[[174]]
x<-x%>%left_join(counts, by=c('group', 'trial', 'object'))
sum(log(x$prob)*x$count)


data_likeli<-function(data, b) {
  ds<-filter(data, group=='A1', trial==1) %>% mutate(softmaxed=softmax(prob, b))
  for (c in 1:4) {
    for (i in 1:16) {
      if (!(c==1 & i==1)) {
        ds<-rbind(ds,
                  filter(data, group==paste0('A', c), trial==i) %>% 
                    mutate(softmaxed=softmax(prob, b)))
      }
    }
  }
  ds<-ds%>%filter(count>0)
  ll<-sum(ds$count*log(ds$softmaxed))
  return(-ll)
}

out<-optim(par=0, fn=data_likeli, data=x, method='Brent', lower=0, upper=100)


# Try plotly
plot_interactive<-function(data, base, col) {
  fits<-matrix(nrow=length(alphas), ncol=length(betas))
  for (i in 1:length(alphas)) {
    for (j in 1:length(betas)) {
      fits[i,j]<-filter(data, alpha==alphas[i], beta==betas[j])%>%pull(col)
      if (fits[i,j]==-Inf) fits[i,j]=base
      if (fits[i,j]>0) fits[i,j]=-fits[i,j]
    }
  }
  
  return(plot_ly(x=alphas, y=betas, z=fits, type = "surface"))
}
plot_interactive(dpar_grid, -4841.103, 'fitted_ll')

# Plot heatmap
dpa_grid %>%
  mutate(alpha=as.character(alpha), beta=as.character(beta),
         raw_ll=if_else(raw_ll==-Inf, -4841.103, raw_ll)) %>%
  ggplot(aes(x=alpha, y=beta, fill=raw_ll)) +
  geom_tile() 

# Second fit
alphas<-c(1:10, 2^(4:10))
betas<-c(seq(0,1,.1), 2^(1:10))

read_ll<-function(df, colname, default=0) {
  fits<-matrix(nrow=length(alphas), ncol=length(betas))
  for (i in 1:length(alphas)) {
    for (j in 1:length(betas)) {
      ll_col<-filter(df, alpha==alphas[i], beta==betas[j])
      ll_val<-if (nrow(ll_col)>0) ll_col[, colname] else default
      fits[i,j]<-if (ll_val > 0) -ll_val else ll_val
    }
  }
  return(fits)
}


persp3D(log(alphas), log(betas), read_ll(dp50_grid, 'raw_ll', -4841.103), 
        theta=20, phi=20, expand=1, 
        xlab="log(alpha)", ylab="log(beta)", zlab="", )


par(mfrow=c(2, 3))

persp3D(log(alphas), log(betas), read_ll(dpa_grid_2, 'raw_ll', 0), 
        theta=20, phi=20, expand=1, 
        xlab="log(alpha)", ylab="log(beta)", zlab="", main='1')

persp3D(log(alphas), log(betas), read_ll(dp75_grid, 'raw_ll', 0), 
        theta=20, phi=20, expand=1, 
        xlab="log(alpha)", ylab="log(beta)", zlab="", main='.75')

persp3D(log(alphas), log(betas), read_ll(dp50_grid, 'raw_ll', 0), 
        theta=20, phi=20, expand=1, 
        xlab="log(alpha)", ylab="log(beta)", zlab="", main='.5')

persp3D(log(alphas), log(betas), read_ll(dp25_grid, 'raw_ll', 0), 
        theta=20, phi=20, expand=1, 
        xlab="log(alpha)", ylab="log(beta)", zlab="", main='.25')

persp3D(log(alphas), log(betas), read_ll(dpr_grid_2, 'raw_ll', 0), 
        theta=20, phi=20, expand=1, 
        xlab="log(alpha)", ylab="log(beta)", zlab="", main='0')








