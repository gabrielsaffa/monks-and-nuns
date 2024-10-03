################################################################################


library(ape)
library(rethinking)

setwd("")
data <- read.csv("monk_nun_final.csv")
tree <- read.tree("ASJP_eHRAF_tree.nex")


## re-code the input variables
data <- subset(data,region=="Europe" | region=="Asia" | region=="Middle East") # keep only Eurasian societies
celibacy <- ifelse(data$celibacy_cat_rev==0,0,1) # binary indicator for either monks, nuns or both
trans <- data$marriage_trans_dplace
trans <- ifelse(trans==1,1,ifelse(trans==7,1,ifelse(trans==4,1,0))) # binary indicator for the presence of bride-price or dowry or gift exchange
marriage <- data$marital_comp_dplace  
marriage <- ifelse(marriage==2,1,ifelse(marriage==3,1,ifelse(marriage==4,1,ifelse(marriage==5,1,ifelse(marriage==6,1,ifelse(marriage==1,2,3)))))) # 3-level ordinal scale from polygyny to polyandry
inher <- data$inheritance_dplace
inher <- ifelse(inher==1,0,1)
strat <- data$class
religion <- data$religion
language <- data$language
data <- data.frame(cbind(celibacy,trans,marriage,inher,strat,religion,language))
data <- data[complete.cases(data),] # N=91

## prune the tree and convert it to correlation matrix
tree <- keep.tip(tree,data$language)
R <- vcv(tree,corr=TRUE)
R <- R[data$language,data$language]
N <- length(data$language)
language <- 1:N


## put the variables in a list
d <- list(celibacy=as.integer(data$celibacy),
          trans=as.integer(data$trans),
          marriage=as.integer(data$marriage),
          inher=as.integer(data$inher),
          strat=as.integer(data$strat),
          religion=as.integer(data$religion),
          language=language,
          R=R,
          N=N)


## marriage transactions
## fit a model adjusted for the effect of phylogeny, as implied by our DAG
m_list <- alist(
  # celibacy model
  celibacy ~ dbinom(1,p),
  logit(p) <- a + bT*trans + phy[language], 
  
  # phylogenetic adaptive prior
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  a ~ normal(0,1.5),
  bT ~ normal(0,0.5),
  sigma_phy ~ exponential(1)
)

## fit the model with ulam
trans <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
trans_prec <- precis(trans,3,pars=c("a","bT","sigma_phy"),prob=0.9) 
trans_prec <- data.frame(round(trans_prec,2))
write.csv(data.frame(trans_prec),file="cel_tr_eur_prec.csv")
trans_post <- extract.samples(trans,pars=c("a","bT","sigma_phy")) 
write.csv(data.frame(trans_post),file="cel_tr_eur_post.csv")


## add religion as an effect modifier and estimate the effect of marriage transactions on celibacy in each religion
m_list <- alist(
  # celibacy model
  celibacy ~ dbinom(1,p),
  logit(p) <- z_a[religion]*sigma_a + (z_bT[religion]*sigma_bT)*trans + phy[language], 
  
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
trans <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
trans_prec <- precis(trans,3,pars=c("a","bT","sigma_a","sigma_bT","sigma_phy"),prob=0.9) 
trans_prec <- data.frame(round(trans_prec,2))
write.csv(data.frame(trans_prec),file="cel_tr_rel_eur_prec.csv")
trans_post <- extract.samples(trans,pars=c("a","bT","sigma_a","sigma_bT","sigma_phy")) 
write.csv(data.frame(trans_post),file="cel_tr_rel_eur_post.csv")


## marital composition
## fit a model adjusted for the effect of stratification and phylogeny, as implied by our DAG
m_list <- alist(
  # celibacy model
  celibacy ~ dbinom(1,p),
  logit(p) <- a + bS*strat + bM*marriage + phy[language], 
  
  # phylogenetic adaptive prior
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  a ~ normal(0,1.5),
  bS ~ normal(0,0.5),
  bM ~ normal(0,0.5),
  sigma_phy ~ exponential(1)
)

## fit the model with ulam
marit <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
marit_prec <- precis(marit,3,pars=c("a","bM","sigma_phy"),prob=0.9) 
marit_prec <- data.frame(round(marit_prec,2))
write.csv(data.frame(marit_prec),file="cel_mar_eur_prec.csv")
marit_post <- extract.samples(marit,pars=c("a","bM","sigma_phy")) 
write.csv(data.frame(marit_post),file="cel_mar_eur_post.csv")


## add religion as an effect modifier and estimate the effect of marriage type on celibacy in each religion
m_list <- alist(
  # celibacy model
  celibacy ~ dbinom(1,p),
  logit(p) <- z_a[religion]*sigma_a + (z_bS[religion]*sigma_bS)*strat + (z_bM[religion]*sigma_bM)*marriage + phy[language], 
  
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
marit <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
marit_prec <- precis(marit,3,pars=c("a","bM","sigma_a","sigma_bM","sigma_phy"),prob=0.9) 
marit_prec <- data.frame(round(marit_prec,2))
write.csv(data.frame(marit_prec),file="cel_mar_rel_eur_prec.csv")
marit_post <- extract.samples(marit,pars=c("a","bM","sigma_a","sigma_bM","sigma_phy")) 
write.csv(data.frame(marit_post),file="cel_mar_rel_eur_post.csv")


## wealth inheritance
## fit a model adjusted for the effect of stratification
m_list <- alist(
  # celibacy model
  celibacy ~ dbinom(1,p),
  logit(p) <- a + bS*strat + bI*inher + phy[language], 
  
  # phylogenetic adaptive prior
  transpars> vector[N]:phy <<- L_SIGMA*z,
  vector[N]:z ~ normal(0,1),
  transpars> matrix[N,N]:L_SIGMA <<- cholesky_decompose(S),
  transpars> matrix[N,N]:S <- sigma_phy*R,
  
  # priors
  a ~ normal(0,1.5),
  bS ~ normal(0,0.5),
  bI ~ normal(0,0.5),
  sigma_phy ~ exponential(1)
)

## fit the model with ulam
inher <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
inher_prec <- precis(inher,3,pars=c("a","bI","sigma_phy"),prob=0.9) 
inher_prec <- data.frame(round(inher_prec,2))
write.csv(data.frame(inher_prec),file="cel_in_eur_prec.csv")
inher_post <- extract.samples(inher,pars=c("a","bI","sigma_phy")) 
write.csv(data.frame(inher_post),file="cel_in_eur_post.csv")


## add religion as an effect modifier and estimate the effect of inheritance type on celibacy in each religion
m_list <- alist(
  # celibacy model
  celibacy ~ dbinom(1,p),
  logit(p) <- z_a[religion]*sigma_a + (z_bS[religion]*sigma_bS)*strat + (z_bI[religion]*sigma_bI)*inher + phy[language], 
  
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
inher <- ulam(m_list,data=d,chains=4,cores=4,iter=1000)


## get model summary and posterior samples
inher_prec <- precis(inher,3,pars=c("a","bI","sigma_a","sigma_bI","sigma_phy"),prob=0.9) 
inher_prec <- data.frame(round(inher_prec,2))
write.csv(data.frame(inher_prec),file="cel_in_rel_eur_prec.csv")
inher_post <- extract.samples(inher,pars=c("a","bI","sigma_a","sigma_bI","sigma_phy")) 
write.csv(data.frame(inher_post),file="cel_in_rel_eur_post.csv")


################################################################################