################################################################################


library(ape)
library(rethinking)

setwd("")
data <- read.csv("monk_nun_final.csv")
tree <- read.tree("ASJP_eHRAF_tree.nex")


## re-code the input variables
data <- subset(data,celibacy_cat_rev!=0) # subset the data by the presence of celibacy (N=51)
monks <- ifelse(data$celibacy_cat_rev==1,1,ifelse(data$celibacy_cat_rev==2,1,0)) # binary indicator for monks & monks and nuns
nuns <- ifelse(data$celibacy_cat_rev==1,1,ifelse(data$celibacy_cat_rev==3,1,0)) # binary indicator for nuns & monks and nuns
trans_gr <- ifelse(data$marriage_trans_rev==2,1,ifelse(data$marriage_trans_rev==4,1,0)) # binary indicator for payments from the groom's side & both sides
trans_br <- ifelse(data$marriage_trans_rev==3,1,ifelse(data$marriage_trans_rev==4,1,0)) # binary indicator for payments from the bride's side & both sides
marriage_monk <- ifelse(data$marital_comp_rev==2,0,1) # binary indicator for polygyny and polyandry
marriage_nun <- ifelse(data$marital_comp_rev==1,0,1) # binary indicator for monogamy and polyandry
inher <- data$inheritance_dplace
inher_s <- ifelse(inher==7,1,ifelse(inher==4,1,ifelse(inher==5,1,0))) # binary indicator for inheritance by sons only and both sons and daughters
strat <- data$class
religion <- data$religion
language <- data$language
data <- data.frame(cbind(monks,nuns,trans_gr,trans_br,marriage_monk,marriage_nun,inher_s,strat,religion,language))
data <- data[complete.cases(data),] # N=44

## prune the tree and convert it to correlation matrix
tree <- keep.tip(tree,data$language)
R <- vcv(tree,corr=TRUE)
R <- R[data$language,data$language]
N <- length(data$language)
language <- 1:N


## put the variables in a list
d <- list(monks=as.integer(data$monks),
          nuns=as.integer(data$nuns),
          trans_gr=as.integer(data$trans_gr),
          trans_br=as.integer(data$trans_br),
          marriage_monk=as.integer(data$marriage_monk),
          marriage_nun=as.integer(data$marriage_nun),
          inher_s=as.integer(data$inher_s),
          strat=as.integer(data$strat),
          religion=as.integer(data$religion),
          language=language,
          R=R,
          N=N)


## marriage transactions
## monks
m_list <- alist(
  # monks model
  monks ~ dbinom(1,p),
  logit(p) <- a + bT*trans_gr + phy[language], 
  
  # phylogenetic adaptive prior
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # hyper-priors
  a ~ normal(0,1.5),
  bT ~ normal(0,0.5),
  sigma_phy ~ exponential(1)
)

## fit the model with ulam
monk_trans <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
monk_trans_prec <- precis(monk_trans,3,pars=c("a","bT","sigma_phy"),prob=0.9) 
monk_trans_prec <- data.frame(round(monk_trans_prec,2))
write.csv(data.frame(monk_trans_prec),file="monk_tr_prec.csv")
monk_trans_post <- extract.samples(monk_trans,pars=c("a","bT","sigma_phy")) 
write.csv(data.frame(monk_trans_post),file="monk_tr_post.csv")


## add religion
m_list <- alist(
  # monks model
  monks ~ dbinom(1,p),
  logit(p) <- z_a[religion]*sigma_a + (z_bT[religion]*sigma_bT)*trans_gr + phy[language], 
  
  # z-scores
  z_a[religion] ~ normal(0,1),
  z_bT[religion] ~ normal(0,1),
  
  # phylogenetic adaptive prior
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # hyper-priors
  sigma_a ~ exponential(1),
  sigma_bT ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct back our adaptive priors)
  gq> vector[religion]:a <<- z_a*sigma_a,
  gq> vector[religion]:bT <<- z_bT*sigma_bT
)

## fit the model with ulam
monk_trans <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
monk_trans_prec <- precis(monk_trans,3,pars=c("a","bT","sigma_a","sigma_bT","sigma_phy"),prob=0.9) 
monk_trans_prec <- data.frame(round(monk_trans_prec,2))
write.csv(data.frame(monk_trans_prec),file="monk_tr_rel_prec.csv")
monk_trans_post <- extract.samples(monk_trans,pars=c("a","bT","sigma_a","sigma_bT","sigma_phy")) 
write.csv(data.frame(monk_trans_post),file="monk_tr_rel_post.csv")


## nuns
m_list <- alist(
  # nuns model
  nuns ~ dbinom(1,p),
  logit(p) <- a + bT*trans_br + phy[language], 
  
  # phylogenetic adaptive prior
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # hyper-priors
  a ~ normal(0,1.5),
  bT ~ normal(0,0.5),
  sigma_phy ~ exponential(1)
)

## fit the model with ulam
nun_trans <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
nun_trans_prec <- precis(nun_trans,3,pars=c("a","bT","sigma_phy"),prob=0.9) 
nun_trans_prec <- data.frame(round(nun_trans_prec,2))
write.csv(data.frame(nun_trans_prec),file="nun_tr_prec.csv")
nun_trans_post <- extract.samples(nun_trans,pars=c("a","bT","sigma_phy")) 
write.csv(data.frame(nun_trans_post),file="nun_tr_post.csv")


## add religion
m_list <- alist(
  # nuns model
  nuns ~ dbinom(1,p),
  logit(p) <- z_a[religion]*sigma_a + (z_bT[religion]*sigma_bT)*trans_br + phy[language], 
  
  # z-scores
  z_a[religion] ~ normal(0,1),
  z_bT[religion] ~ normal(0,1),
  
  # phylogenetic adaptive prior
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # hyper-priors
  sigma_a ~ exponential(1),
  sigma_bT ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct back our adaptive priors)
  gq> vector[religion]:a <<- z_a*sigma_a,
  gq> vector[religion]:bT <<- z_bT*sigma_bT
)

## fit the model with ulam
nun_trans <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
nun_trans_prec <- precis(nun_trans,3,pars=c("a","bT","sigma_a","sigma_bT","sigma_phy"),prob=0.9) 
nun_trans_prec <- data.frame(round(nun_trans_prec,2))
write.csv(data.frame(nun_trans_prec),file="nun_tr_rel_prec.csv")
nun_trans_post <- extract.samples(nun_trans,pars=c("a","bT","sigma_a","sigma_bT","sigma_phy")) 
write.csv(data.frame(nun_trans_post),file="nun_tr_rel_post.csv")


## marital composition
## monks
m_list <- alist(
  # monks model
  monks ~ dbinom(1,p),
  logit(p) <- a + bS*strat + bM*marriage_monk + phy[language], 
  
  # phylogenetic adaptive prior
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # hyper-priors
  a ~ normal(0,1.5),
  bS ~ normal(0,0.5),
  bM ~ normal(0,0.5),
  sigma_phy ~ exponential(1)
)

## fit the model with ulam
monk_marit <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
monk_marit_prec <- precis(monk_marit,3,pars=c("a","bM","sigma_phy"),prob=0.9) 
monk_marit_prec <- data.frame(round(monk_marit_prec,2))
write.csv(data.frame(monk_marit_prec),file="monk_mar_prec.csv")
monk_marit_post <- extract.samples(monk_marit,pars=c("a","bM","sigma_phy")) 
write.csv(data.frame(monk_marit_post),file="monk_mar_post.csv")


## add religion
m_list <- alist(
  # monks model
  monks ~ dbinom(1,p),
  logit(p) <- z_a[religion]*sigma_a + (z_bS[religion]*sigma_bS)*strat + (z_bM[religion]*sigma_bM)*marriage_monk + phy[language], 
  
  # z-scores
  z_a[religion] ~ normal(0,1),
  z_bS[religion] ~ normal(0,1),
  z_bM[religion] ~ normal(0,1),
  
  # phylogenetic adaptive prior
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # hyper-priors
  sigma_a ~ exponential(1),
  sigma_bS ~ exponential(1),
  sigma_bM ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct back our adaptive priors)
  gq> vector[religion]:a <<- z_a*sigma_a,
  gq> vector[religion]:bS <<- z_bS*sigma_bS,
  gq> vector[religion]:bM <<- z_bM*sigma_bM
)

## fit the model with ulam
monk_marit <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
monk_marit_prec <- precis(monk_marit,3,pars=c("a","bM","sigma_a","sigma_bM","sigma_phy"),prob=0.9) 
monk_marit_prec <- data.frame(round(monk_marit_prec,2))
write.csv(data.frame(monk_marit_prec),file="monk_mar_rel_prec.csv")
monk_marit_post <- extract.samples(monk_marit,pars=c("a","bM","sigma_a","sigma_bM","sigma_phy")) 
write.csv(data.frame(monk_marit_post),file="monk_mar_rel_post.csv")


## nuns
m_list <- alist(
  # nuns model
  nuns ~ dbinom(1,p),
  logit(p) <- a + bS*strat + bM*marriage_nun + phy[language], 
  
  # phylogenetic adaptive prior
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # hyper-priors
  a ~ normal(0,1.5),
  bS ~ normal(0,0.5),
  bM ~ normal(0,0.5),
  sigma_phy ~ exponential(1)
)

## fit the model with ulam
nun_marit <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
nun_marit_prec <- precis(nun_marit,3,pars=c("a","bM","sigma_phy"),prob=0.9) 
nun_marit_prec <- data.frame(round(nun_marit_prec,2))
write.csv(data.frame(nun_marit_prec),file="nun_mar_prec.csv")
nun_marit_post <- extract.samples(nun_marit,pars=c("a","bM","sigma_phy")) 
write.csv(data.frame(nun_marit_post),file="nun_mar_post.csv")


## add religion
m_list <- alist(
  # nuns model
  nuns ~ dbinom(1,p),
  logit(p) <- z_a[religion]*sigma_a + (z_bS[religion]*sigma_bS)*strat + (z_bM[religion]*sigma_bM)*marriage_nun + phy[language], 
  
  # z-scores
  z_a[religion] ~ normal(0,1),
  z_bS[religion] ~ normal(0,1), 
  z_bM[religion] ~ normal(0,1),
  
  # phylogenetic adaptive prior
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # hyper-priors
  sigma_a ~ exponential(1),
  sigma_bS ~ exponential(1),
  sigma_bM ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct back our adaptive priors)
  gq> vector[religion]:a <<- z_a*sigma_a,
  gq> vector[religion]:bS <<- z_bS*sigma_bS,
  gq> vector[religion]:bM <<- z_bM*sigma_bM
)

## fit the model with ulam
nun_marit <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
nun_marit_prec <- precis(nun_marit,3,pars=c("a","bM","sigma_a","sigma_bM","sigma_phy"),prob=0.9) 
nun_marit_prec <- data.frame(round(nun_marit_prec,2))
write.csv(data.frame(nun_marit_prec),file="nun_mar_rel_prec.csv")
nun_marit_post <- extract.samples(nun_marit,pars=c("a","bM","sigma_a","sigma_bM","sigma_phy")) 
write.csv(data.frame(nun_marit_post),file="nun_mar_rel_post.csv")


## wealth inheritance
## monks
m_list <- alist(
  # monks model
  monks ~ dbinom(1,p),
  logit(p) <- a + bS*strat + bI*inher_s + phy[language], 
  
  # phylogenetic adaptive prior
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # hyper-priors
  a ~ normal(0,1.5),
  bS ~ normal(0,0.5),
  bI ~ normal(0,0.5),
  sigma_phy ~ exponential(1)
)

## fit the model with ulam
monk_inher <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
monk_inher_prec <- precis(monk_inher,3,pars=c("a","bI","sigma_phy"),prob=0.9) 
monk_inher_prec <- data.frame(round(monk_inher_prec,2))
write.csv(data.frame(monk_inher_prec),file="monk_in_prec.csv")
monk_inher_post <- extract.samples(monk_inher,pars=c("a","bI","sigma_phy")) 
write.csv(data.frame(monk_inher_post),file="monk_in_post.csv")


## add religion
m_list <- alist(
  # monks model
  monks ~ dbinom(1,p),
  logit(p) <- z_a[religion]*sigma_a + (z_bS[religion]*sigma_bS)*strat + (z_bI[religion]*sigma_bI)*inher_s + phy[language], 
  
  # z-scores
  z_a[religion] ~ normal(0,1),
  z_bS[religion] ~ normal(0,1),
  z_bI[religion] ~ normal(0,1),
  
  # phylogenetic adaptive prior
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # hyper-priors
  sigma_a ~ exponential(1),
  sigma_bS ~ exponential(1),
  sigma_bI ~ exponential(1),
  sigma_phy ~ exponential(1),
  
  ## generated quantities (reconstruct back our adaptive priors)
  gq> vector[religion]:a <<- z_a*sigma_a,
  gq> vector[religion]:bS <<- z_bS*sigma_bS,
  gq> vector[religion]:bI <<- z_bI*sigma_bI
)

## fit the model with ulam
monk_inher <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
monk_inher_prec <- precis(monk_inher,3,pars=c("a","bI","sigma_a","sigma_bI","sigma_phy"),prob=0.9) 
monk_inher_prec <- data.frame(round(monk_inher_prec,2))
write.csv(data.frame(monk_inher_prec),file="monk_in_rel_prec.csv")
monk_inher_post <- extract.samples(monk_inher,pars=c("a","bI","sigma_a","sigma_bI","sigma_phy")) 
write.csv(data.frame(monk_inher_post),file="monk_in_rel_post.csv")


################################################################################