################################################################################


library(rethinking)
library(maps)

setwd("")
cel_tr <- read.csv("cel_tr_post.csv")
cel_tr_rel <- read.csv("cel_tr_rel_post.csv")
cel_tr_eur <- read.csv("cel_tr_eur_post.csv")
cel_tr_rel_eur <- read.csv("cel_tr_rel_eur_post.csv")
cel_mar <- read.csv("cel_mar_post.csv")
cel_mar_rel <- read.csv("cel_mar_rel_post.csv")
cel_mar_eur <- read.csv("cel_mar_eur_post.csv")
cel_mar_rel_eur <- read.csv("cel_mar_rel_eur_post.csv")
cel_in <- read.csv("cel_in_post.csv")
cel_in_rel <- read.csv("cel_in_rel_post.csv")
cel_in_eur <- read.csv("cel_in_eur_post.csv")
cel_in_rel_eur <- read.csv("cel_in_rel_eur_post.csv")
monk_tr <- read.csv("monk_tr_post.csv")
monk_tr_rel <- read.csv("monk_tr_rel_post.csv")
monk_tr_eur <- read.csv("monk_tr_eur_post.csv")
monk_tr_rel_eur <- read.csv("monk_tr_rel_eur_post.csv")
monk_mar <- read.csv("monk_mar_post.csv")
monk_mar_rel <- read.csv("monk_mar_rel_post.csv")
monk_mar_eur <- read.csv("monk_mar_eur_post.csv")
monk_mar_rel_eur <- read.csv("monk_mar_rel_eur_post.csv")
monk_in <- read.csv("monk_in_post.csv")
monk_in_rel <- read.csv("monk_in_rel_post.csv")
monk_in_eur <- read.csv("monk_in_eur_post.csv")
monk_in_rel_eur <- read.csv("monk_in_rel_eur_post.csv")
nun_tr <- read.csv("nun_tr_post.csv")
nun_tr_rel <- read.csv("nun_tr_rel_post.csv")
nun_tr_eur <- read.csv("nun_tr_eur_post.csv")
nun_tr_rel_eur <- read.csv("nun_tr_rel_eur_post.csv")
nun_mar <- read.csv("nun_mar_post.csv")
nun_mar_rel <- read.csv("nun_mar_rel_post.csv")
nun_mar_eur <- read.csv("nun_mar_eur_post.csv")
nun_mar_rel_eur <- read.csv("nun_mar_rel_eur_post.csv")


## Figure 1 (map)
monk_nun_final <- read.csv("monk_nun_final.csv",header=TRUE)
map("world",fill=TRUE,border=NA,col="gray75",bg="lightblue",ylim=c(-60,90),mar=c(0,0,0,0))
lat_jitt <- monk_nun_final$latitude + runif(nrow(monk_nun_final),-0.3,0.3) # add some random noise to latitude
long_jitt <- monk_nun_final$longitude + runif(nrow(monk_nun_final),-0.3,0.3) # add some random noise to longitude
bg_cols <- c("white","#547e93","goldenrod1","orangered2")
pch <- c(21:25)
points(long_jitt,lat_jitt,col="black",pch=pch[as.factor(monk_nun_final$religion)],bg=bg_cols[as.factor(monk_nun_final$celibacy_cat_rev)])
legend(-90,-80,legend=c("Absent","Monks and nuns","Monks","Nuns"),col=bg_cols,pch=16,bty="n",box.col=NA,cex=1.5,xpd=NA)
legend(30,-80,legend=c("Indigenous","Islam","Christianity","Buddhism","Hinduism"),col="black",pch=pch,bty="n",box.col=NA,cex=1.5,xpd=NA)
dev.off()


{
  ## Figure 3
  n <- 2000
  seq_f <- c(0,1)
  seq_o <- 1:3
  par(mfrow=c(3,3),mar=c(5.1,5.1,3.1,5.1))
  
  ## celibacy ~ marriage transactions
  plot(NULL,xlab="marriage transactions",ylab="p(celibacy)",xlim=c(0,1),ylim=c(0,0.2),yaxt="n",xaxt="n",cex.lab=2,axes=FALSE)
  axis(1,at=seq(0.1,0.9,length.out=2),labels=c("absent","present"),cex.axis=2)
  axis(2,at=seq(0,0.2,length.out=3),labels=c("0","0.1","0.2"),cex.axis=2)
  mtext("A",3,line=0.25,adj=0,cex=2)
  text(0.6,0.175,"PP 82.65 %",cex=2.5,col="#547e93")
  
  cel_tr_pred <- sapply(seq_f,function(x) inv_logit(cel_tr$a + cel_tr$bT*x))
  points(c(0.1,0.9),c(mean(cel_tr_pred[,1]),mean(cel_tr_pred[,2])),col="#547e93",pch=1,cex=7,lwd=7,lty=1,type="b")
  for (i in 1:2) {
    lines(rep(c(0.1,0.9)[i],2),c(PI(cel_tr_pred[,i],0.9)[1],PI(cel_tr_pred[,i],0.9)[2]),lwd=7,col="#547e93")
  }
  
  ## celibacy ~ marital composition
  plot(NULL,xlab="marital composition",ylab="p(celibacy)",xlim=c(1,3),ylim=c(0,0.2),yaxt="n",xaxt="n",cex.lab=2,axes=FALSE)
  axis(1,at=seq(1.1,2.9,length.out=3),labels=c("polyg.","monog.","polya."),cex.axis=2)
  axis(2,at=seq(0,0.2,length.out=3),labels=c("0","0.1","0.2"),cex.axis=2)
  mtext("B",3,line=0.25,adj=0,cex=2)
  text(2.25,0.175,"PP >99 %",cex=2.5,col="#547e93")
  
  cel_mar_pred <- sapply(seq_o,function(x) inv_logit(cel_mar$a + cel_mar$bM*x))
  points(c(1.1,2,2.9),c(mean(cel_mar_pred[,1]),mean(cel_mar_pred[,2]),mean(cel_mar_pred[,3])),col="#547e93",pch=1,cex=7,lwd=7,lty=1,type="b")
  for (i in 1:3) {
    lines(rep(c(1.1,2,2.9)[i],3),c(PI(cel_mar_pred[,i],0.9)[1],PI(cel_mar_pred[,i],0.9)[2],PI(cel_mar_pred[,i],0.9)[3]),lwd=7,col="#547e93")
  }
  
  ## celibacy ~ wealth inheritance
  plot(NULL,xlab="wealth inheritance",ylab="p(celibacy)",xlim=c(0,1),ylim=c(0,0.2),yaxt="n",xaxt="n",cex.lab=2,axes=FALSE)
  axis(1,at=seq(0.1,0.9,length.out=2),labels=c("absent","present"),cex.axis=2)
  axis(2,at=seq(0,0.2,length.out=3),labels=c("0","0.1","0.2"),cex.axis=2)
  mtext("C",3,line=0.25,adj=0,cex=2)
  text(0.6,0.175,"PP 89.15 %",cex=2.5,col="#547e93")
  
  cel_in_pred <- sapply(seq_f,function(x) inv_logit(cel_in$a + cel_in$bI*x))
  points(c(0.1,0.9),c(mean(cel_in_pred[,1]),mean(cel_in_pred[,2])),col="#547e93",pch=1,cex=7,lwd=7,lty=1,type="b")
  for (i in 1:2) {
    lines(rep(c(0.1,0.9)[i],2),c(PI(cel_in_pred[,i],0.9)[1],PI(cel_in_pred[,i],0.9)[2]),lwd=7,col="#547e93")
  }
  
  ## monks ~ marriage transactions
  plot(NULL,xlab="marriage transactions",ylab="p(monks)",xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",cex.lab=2,axes=FALSE)
  axis(1,at=seq(0.1,0.9,length.out=2),labels=c("bride's side","groom's side"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("D",3,line=0.25,adj=0,cex=2)
  text(0.6,0.15,"PP 95.25 %",cex=2.5,col="goldenrod1")
  
  monk_tr_pred <- sapply(seq_f,function(x) inv_logit(monk_tr$a + monk_tr$bT*x))
  points(c(0.1,0.9),c(mean(monk_tr_pred[,1]),mean(monk_tr_pred[,2])),col="goldenrod1",pch=1,cex=7,lwd=7,lty=1,type="b")
  for (i in 1:2) {
    lines(rep(c(0.1,0.9)[i],2),c(PI(monk_tr_pred[,i],0.9)[1],PI(monk_tr_pred[,i],0.9)[2]),lwd=7,col="goldenrod1")
  }
  
  ## monks ~ marital composition
  plot(NULL,xlab="marital composition",ylab="p(monks)",xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",cex.lab=2,axes=FALSE)
  axis(1,at=seq(0.1,0.9,length.out=2),labels=c("monogamy","polyg./polyandry"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("E",3,line=0.25,adj=0,cex=2)
  text(0.6,0.15,"PP 83.90 %",cex=2.5,col="goldenrod1")
  
  monk_mar_pred <- sapply(seq_f,function(x) inv_logit(monk_mar$a + monk_mar$bM*x))
  points(c(0.1,0.9),c(mean(monk_mar_pred[,1]),mean(monk_mar_pred[,2])),col="goldenrod1",pch=1,cex=7,lwd=7,lty=1,type="b")
  for (i in 1:2) {
    lines(rep(c(0.1,0.9)[i],2),c(PI(monk_mar_pred[,i],0.9)[1],PI(monk_mar_pred[,i],0.9)[2]),lwd=7,col="goldenrod1")
  }
  
  ## monks ~ wealth inheritance
  plot(NULL,xlab="wealth inheritance",ylab="p(monks)",xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",cex.lab=2,axes=FALSE)
  axis(1,at=seq(0.1,0.9,length.out=2),labels=c("non-patrilineal","patrilineal"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("F",3,line=0.25,adj=0,cex=2)
  text(0.6,0.15,"PP 90.35 %",cex=2.5,col="goldenrod1")
  
  monk_in_pred <- sapply(seq_f,function(x) inv_logit(monk_in$a + monk_in$bI*x))
  points(c(0.1,0.9),c(mean(monk_in_pred[,1]),mean(monk_in_pred[,2])),col="goldenrod1",pch=1,cex=7,lwd=7,lty=1,type="b")
  for (i in 1:2) {
    lines(rep(c(0.1,0.9)[i],2),c(PI(monk_in_pred[,i],0.9)[1],PI(monk_in_pred[,i],0.9)[2]),lwd=7,col="goldenrod1")
  }
  
  ## nuns ~ marriage transactions
  plot(NULL,xlab="marriage transactions",ylab="p(nuns)",xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",cex.lab=2,axes=FALSE)
  axis(1,at=seq(0.1,0.9,length.out=2),labels=c("groom's side","bride's side"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("G",3,line=0.25,adj=0,cex=2)
  text(0.6,0.15,"PP 85.95 %",cex=2.5,col="orangered2")
  
  nun_tr_pred <- sapply(seq_f,function(x) inv_logit(nun_tr$a + nun_tr$bT*x))
  points(c(0.1,0.9),c(mean(nun_tr_pred[,1]),mean(nun_tr_pred[,2])),col="orangered2",pch=1,cex=7,lwd=7,lty=1,type="b")
  for (i in 1:2) {
    lines(rep(c(0.1,0.9)[i],2),c(PI(nun_tr_pred[,i],0.9)[1],PI(nun_tr_pred[,i],0.9)[2]),lwd=7,col="orangered2")
  }
  
  ## nuns ~ marital composition
  plot(NULL,xlab="marital composition",ylab="p(nuns)",xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",cex.lab=2,axes=FALSE)
  axis(1,at=seq(0.1,0.9,length.out=2),labels=c("polygyny","monog./polyandry"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("H",3,line=0.25,adj=0,cex=2)
  text(0.6,0.15,"PP 93.65 %",cex=2.5,col="orangered2")
  
  nun_mar_pred <- sapply(seq_f,function(x) inv_logit(nun_mar$a + nun_mar$bM*x))
  points(c(0.1,0.9),c(mean(nun_mar_pred[,1]),mean(nun_mar_pred[,2])),col="orangered2",pch=1,cex=7,lwd=7,lty=1,type="b")
  for (i in 1:2) {
    lines(rep(c(0.1,0.9)[i],2),c(PI(nun_mar_pred[,i],0.9)[1],PI(nun_mar_pred[,i],0.9)[2]),lwd=7,col="orangered2")
  }
}


{
  par(mfrow=c(3,3),mar=c(5.1,5.1,3.1,5.1))
  ## Figure 4 (Eurasia)
  ## celibacy ~ marriage transactions
  plot(NULL,xlab="marriage transactions",ylab="p(celibacy)",xlim=c(0,1),ylim=c(0,0.5),yaxt="n",xaxt="n",cex.lab=2,axes=FALSE)
  axis(1,at=seq(0.1,0.9,length.out=2),labels=c("absent","present"),cex.axis=2)
  axis(2,at=seq(0,0.5,length.out=3),labels=c("0","0.25","0.5"),cex.axis=2)
  mtext("A",3,line=0.25,adj=0,cex=2)
  text(0.6,0.45,"PP 61.20 %",cex=2.5,col="#547e93")
  
  cel_tr_pred <- sapply(seq_f,function(x) inv_logit(cel_tr_eur$a + cel_tr_eur$bT*x))
  points(c(0.1,0.9),c(mean(cel_tr_pred[,1]),mean(cel_tr_pred[,2])),col="#547e93",pch=1,cex=7,lwd=7,lty=1,type="b")
  for (i in 1:2) {
    lines(rep(c(0.1,0.9)[i],2),c(PI(cel_tr_pred[,i],0.9)[1],PI(cel_tr_pred[,i],0.9)[2]),lwd=7,col="#547e93")
  }
  
  ## celibacy ~ marital composition
  plot(NULL,xlab="marital composition",ylab="p(celibacy)",xlim=c(1,3),ylim=c(0,0.5),yaxt="n",xaxt="n",cex.lab=2,axes=FALSE)
  axis(1,at=seq(1.1,2.9,length.out=3),labels=c("polyg.","monog.","polya."),cex.axis=2)
  axis(2,at=seq(0,0.5,length.out=3),labels=c("0","0.25","0.5"),cex.axis=2)
  mtext("B",3,line=0.25,adj=0,cex=2)
  text(2.25,0.45,"PP 94.75 %",cex=2.5,col="#547e93")
  
  cel_mar_pred <- sapply(seq_o,function(x) inv_logit(cel_mar_eur$a + cel_mar_eur$bM*x))
  points(c(1.1,2,2.9),c(mean(cel_mar_pred[,1]),mean(cel_mar_pred[,2]),mean(cel_mar_pred[,3])),col="#547e93",pch=1,cex=7,lwd=7,lty=1,type="b")
  for (i in 1:3) {
    lines(rep(c(1.1,2,2.9)[i],3),c(PI(cel_mar_pred[,i],0.9)[1],PI(cel_mar_pred[,i],0.9)[2],PI(cel_mar_pred[,i],0.9)[3]),lwd=7,col="#547e93")
  }
  
  ## celibacy ~ wealth inheritance
  plot(NULL,xlab="wealth inheritance",ylab="p(celibacy)",xlim=c(0,1),ylim=c(0,0.5),yaxt="n",xaxt="n",cex.lab=2,axes=FALSE)
  axis(1,at=seq(0.1,0.9,length.out=2),labels=c("absent","present"),cex.axis=2)
  axis(2,at=seq(0,0.5,length.out=3),labels=c("0","0.25","0.5"),cex.axis=2)
  mtext("C",3,line=0.25,adj=0,cex=2)
  text(0.6,0.45,"PP 84.55 %",cex=2.5,col="#547e93")
  
  cel_in_pred <- sapply(seq_f,function(x) inv_logit(cel_in_eur$a + cel_in_eur$bI*x))
  points(c(0.1,0.9),c(mean(cel_in_pred[,1]),mean(cel_in_pred[,2])),col="#547e93",pch=1,cex=7,lwd=7,lty=1,type="b")
  for (i in 1:2) {
    lines(rep(c(0.1,0.9)[i],2),c(PI(cel_in_pred[,i],0.9)[1],PI(cel_in_pred[,i],0.9)[2]),lwd=7,col="#547e93")
  }
  
  ## monks ~ marriage transactions
  plot(NULL,xlab="marriage transactions",ylab="p(monks)",xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",cex.lab=2,axes=FALSE)
  axis(1,at=seq(0.1,0.9,length.out=2),labels=c("bride's side","groom's side"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("D",3,line=0.25,adj=0,cex=2)
  text(0.6,0.15,"PP 86.95 %",cex=2.5,col="goldenrod1")
  
  monk_tr_pred <- sapply(seq_f,function(x) inv_logit(monk_tr_eur$a + monk_tr_eur$bT*x))
  points(c(0.1,0.9),c(mean(monk_tr_pred[,1]),mean(monk_tr_pred[,2])),col="goldenrod1",pch=1,cex=7,lwd=7,lty=1,type="b")
  for (i in 1:2) {
    lines(rep(c(0.1,0.9)[i],2),c(PI(monk_tr_pred[,i],0.9)[1],PI(monk_tr_pred[,i],0.9)[2]),lwd=7,col="goldenrod1")
  }
  
  ## monks ~ marital composition
  plot(NULL,xlab="marital composition",ylab="p(monks)",xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",cex.lab=2,axes=FALSE)
  axis(1,at=seq(0.1,0.9,length.out=2),labels=c("monogamy","polyg./polyandry"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("E",3,line=0.25,adj=0,cex=2)
  text(0.6,0.15,"PP 85.90 %",cex=2.5,col="goldenrod1")
  
  monk_mar_pred <- sapply(seq_f,function(x) inv_logit(monk_mar_eur$a + monk_mar_eur$bM*x))
  points(c(0.1,0.9),c(mean(monk_mar_pred[,1]),mean(monk_mar_pred[,2])),col="goldenrod1",pch=1,cex=7,lwd=7,lty=1,type="b")
  for (i in 1:2) {
    lines(rep(c(0.1,0.9)[i],2),c(PI(monk_mar_pred[,i],0.9)[1],PI(monk_mar_pred[,i],0.9)[2]),lwd=7,col="goldenrod1")
  }
  
  ## monks ~ wealth inheritance
  plot(NULL,xlab="wealth inheritance",ylab="p(monks)",xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",cex.lab=2,axes=FALSE)
  axis(1,at=seq(0.1,0.9,length.out=2),labels=c("non-patrilineal","patrilineal"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("F",3,line=0.25,adj=0,cex=2)
  text(0.6,0.15,"PP 66.80 %",cex=2.5,col="goldenrod1")
  
  monk_in_pred <- sapply(seq_f,function(x) inv_logit(monk_in_eur$a + monk_in_eur$bI*x))
  points(c(0.1,0.9),c(mean(monk_in_pred[,1]),mean(monk_in_pred[,2])),col="goldenrod1",pch=1,cex=7,lwd=7,lty=1,type="b")
  for (i in 1:2) {
    lines(rep(c(0.1,0.9)[i],2),c(PI(monk_in_pred[,i],0.9)[1],PI(monk_in_pred[,i],0.9)[2]),lwd=7,col="goldenrod1")
  }
  
  ## nuns ~ marriage transactions
  plot(NULL,xlab="marriage transactions",ylab="p(nuns)",xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",cex.lab=2,axes=FALSE)
  axis(1,at=seq(0.1,0.9,length.out=2),labels=c("groom's side","bride's side"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("G",3,line=0.25,adj=0,cex=2)
  text(0.6,0.15,"PP 90.60 %",cex=2.5,col="orangered2")
  
  nun_tr_pred <- sapply(seq_f,function(x) inv_logit(nun_tr_eur$a + nun_tr_eur$bT*x))
  points(c(0.1,0.9),c(mean(nun_tr_pred[,1]),mean(nun_tr_pred[,2])),col="orangered2",pch=1,cex=7,lwd=7,lty=1,type="b")
  for (i in 1:2) {
    lines(rep(c(0.1,0.9)[i],2),c(PI(nun_tr_pred[,i],0.9)[1],PI(nun_tr_pred[,i],0.9)[2]),lwd=7,col="orangered2")
  }
  
  ## nuns ~ marital composition
  plot(NULL,xlab="marital composition",ylab="p(nuns)",xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n",cex.lab=2,axes=FALSE)
  axis(1,at=seq(0.1,0.9,length.out=2),labels=c("polygyny","monog./polyandry"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("H",3,line=0.25,adj=0,cex=2)
  text(0.6,0.15,"PP 89.30 %",cex=2.5,col="orangered2")
  
  nun_mar_pred <- sapply(seq_f,function(x) inv_logit(nun_mar_eur$a + nun_mar_eur$bM*x))
  points(c(0.1,0.9),c(mean(nun_mar_pred[,1]),mean(nun_mar_pred[,2])),col="orangered2",pch=1,cex=7,lwd=7,lty=1,type="b")
  for (i in 1:2) {
    lines(rep(c(0.1,0.9)[i],2),c(PI(nun_mar_pred[,i],0.9)[1],PI(nun_mar_pred[,i],0.9)[2]),lwd=7,col="orangered2")
  }
}


{
  ## Figure 5 (religion)
  ## celibacy ~ marriage transactions
  par(mfrow=c(3,1),mar=c(5.1,5.1,3.1,5.1))
  cols <- c("#784617","#D4622E","#E5A827","#5E8C7B","#21443E")
  cel_tr_1 <- cel_tr_rel$bT.1
  cel_tr_2 <- cel_tr_rel$bT.2
  cel_tr_3 <- cel_tr_rel$bT.3
  cel_tr_4 <- cel_tr_rel$bT.4
  cel_tr_5 <- cel_tr_rel$bT.5
  cel_tr_mu <- c(mean(cel_tr_1),mean(cel_tr_2),mean(cel_tr_3),mean(cel_tr_4),mean(cel_tr_5))
  cel_tr_low <- as.numeric(c(PI(cel_tr_1,0.9)[1],PI(cel_tr_2,0.9)[1],PI(cel_tr_3,0.9)[1],PI(cel_tr_4,0.9)[1],PI(cel_tr_5,0.9)[1]))
  cel_tr_upp <- as.numeric(c(PI(cel_tr_1,0.9)[2],PI(cel_tr_2,0.9)[2],PI(cel_tr_3,0.9)[2],PI(cel_tr_4,0.9)[2],PI(cel_tr_5,0.9)[2]))
  plot(NA,xlim=c(-6,6),ylim=c(0,7),xlab="log odds(celibacy)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-6,6,length.out=5),labels=c("-6","-3","0","3","6"),cex.axis=2)
  mtext("A",3,line=0.25,adj=0,cex=2)
  points(cel_tr_mu,5:1,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:5) {
    lines(c(cel_tr_low[i],cel_tr_upp[i]),rep(c(5:1)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=3,col="gray75")
  legend(4,7,legend=c("Indigenous","Islam","Christianity","Buddhism","Hinduism"),col=cols,pch=21,lwd=5,bty="n",box.col=NA,cex=2,xpd=NA)
  
  ## monks ~ marriage transactions
  monk_tr_1 <- monk_tr_rel$bT.1
  monk_tr_2 <- monk_tr_rel$bT.2
  monk_tr_3 <- monk_tr_rel$bT.3
  monk_tr_4 <- monk_tr_rel$bT.4
  monk_tr_5 <- monk_tr_rel$bT.5
  monk_tr_mu <- c(mean(monk_tr_1),mean(monk_tr_2),mean(monk_tr_3),mean(monk_tr_4),mean(monk_tr_5))
  monk_tr_low <- as.numeric(c(PI(monk_tr_1,0.9)[1],PI(monk_tr_2,0.9)[1],PI(monk_tr_3,0.9)[1],PI(monk_tr_4,0.9)[1],PI(monk_tr_5,0.9)[1]))
  monk_tr_upp <- as.numeric(c(PI(monk_tr_1,0.9)[2],PI(monk_tr_2,0.9)[2],PI(monk_tr_3,0.9)[2],PI(monk_tr_4,0.9)[2],PI(monk_tr_5,0.9)[2]))
  plot(NA,xlim=c(-6,6),ylim=c(0,7),xlab="log odds(monks)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-6,6,length.out=5),labels=c("-6","-3","0","3","6"),cex.axis=2)
  mtext("B",3,line=0.25,adj=0,cex=2)
  points(monk_tr_mu,5:1,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:5) {
    lines(c(monk_tr_low[i],monk_tr_upp[i]),rep(c(5:1)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=3,col="gray75")
  
  ## nuns ~ marriage transactions
  nun_tr_1 <- nun_tr_rel$bT.1
  nun_tr_2 <- nun_tr_rel$bT.2
  nun_tr_3 <- nun_tr_rel$bT.3
  nun_tr_4 <- nun_tr_rel$bT.4
  nun_tr_5 <- nun_tr_rel$bT.5
  nun_tr_mu <- c(mean(nun_tr_1),mean(nun_tr_2),mean(nun_tr_3),mean(nun_tr_4),mean(nun_tr_5))
  nun_tr_low <- as.numeric(c(PI(nun_tr_1,0.9)[1],PI(nun_tr_2,0.9)[1],PI(nun_tr_3,0.9)[1],PI(nun_tr_4,0.9)[1],PI(nun_tr_5,0.9)[1]))
  nun_tr_upp <- as.numeric(c(PI(nun_tr_1,0.9)[2],PI(nun_tr_2,0.9)[2],PI(nun_tr_3,0.9)[2],PI(nun_tr_4,0.9)[2],PI(nun_tr_5,0.9)[2]))
  plot(NA,xlim=c(-6,6),ylim=c(0,7),xlab="log odds(nuns)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-6,6,length.out=5),labels=c("-6","-3","0","3","6"),cex.axis=2)
  mtext("C",3,line=0.25,adj=0,cex=2)
  points(nun_tr_mu,5:1,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:5) {
    lines(c(nun_tr_low[i],nun_tr_upp[i]),rep(c(5:1)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=3,col="gray75")
}


{
  ## Figure 6 (religion - Eurasia)
  ## celibacy ~ marriage transactions
  cel_tr_1 <- cel_tr_rel_eur$bT.1
  cel_tr_2 <- cel_tr_rel_eur$bT.2
  cel_tr_3 <- cel_tr_rel_eur$bT.3
  cel_tr_4 <- cel_tr_rel_eur$bT.4
  cel_tr_5 <- cel_tr_rel_eur$bT.5
  cel_tr_mu <- c(mean(cel_tr_1),mean(cel_tr_2),mean(cel_tr_3),mean(cel_tr_4),mean(cel_tr_5))
  cel_tr_low <- as.numeric(c(PI(cel_tr_1,0.9)[1],PI(cel_tr_2,0.9)[1],PI(cel_tr_3,0.9)[1],PI(cel_tr_4,0.9)[1],PI(cel_tr_5,0.9)[1]))
  cel_tr_upp <- as.numeric(c(PI(cel_tr_1,0.9)[2],PI(cel_tr_2,0.9)[2],PI(cel_tr_3,0.9)[2],PI(cel_tr_4,0.9)[2],PI(cel_tr_5,0.9)[2]))
  plot(NA,xlim=c(-6,6),ylim=c(0,7),xlab="log odds(celibacy)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-6,6,length.out=5),labels=c("-6","-3","0","3","6"),cex.axis=2)
  mtext("A",3,line=0.25,adj=0,cex=2)
  points(cel_tr_mu,5:1,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:5) {
    lines(c(cel_tr_low[i],cel_tr_upp[i]),rep(c(5:1)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=3,col="gray75")
  legend(4,7,legend=c("Indigenous","Islam","Christianity","Buddhism","Hinduism"),col=cols,pch=21,lwd=5,bty="n",box.col=NA,cex=2,xpd=NA)
  
  ## monks ~ marriage transactions
  monk_tr_1 <- monk_tr_rel_eur$bT.1
  monk_tr_2 <- monk_tr_rel_eur$bT.2
  monk_tr_3 <- monk_tr_rel_eur$bT.3
  monk_tr_4 <- monk_tr_rel_eur$bT.4
  monk_tr_5 <- monk_tr_rel_eur$bT.5
  monk_tr_mu <- c(mean(monk_tr_1),mean(monk_tr_2),mean(monk_tr_3),mean(monk_tr_4),mean(monk_tr_5))
  monk_tr_low <- as.numeric(c(PI(monk_tr_1,0.9)[1],PI(monk_tr_2,0.9)[1],PI(monk_tr_3,0.9)[1],PI(monk_tr_4,0.9)[1],PI(monk_tr_5,0.9)[1]))
  monk_tr_upp <- as.numeric(c(PI(monk_tr_1,0.9)[2],PI(monk_tr_2,0.9)[2],PI(monk_tr_3,0.9)[2],PI(monk_tr_4,0.9)[2],PI(monk_tr_5,0.9)[2]))
  plot(NA,xlim=c(-6,6),ylim=c(0,7),xlab="log odds(monks)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-6,6,length.out=5),labels=c("-6","-3","0","3","6"),cex.axis=2)
  mtext("B",3,line=0.25,adj=0,cex=2)
  points(monk_tr_mu,5:1,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:5) {
    lines(c(monk_tr_low[i],monk_tr_upp[i]),rep(c(5:1)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=3,col="gray75")
  
  ## nuns ~ marriage transactions
  nun_tr_1 <- nun_tr_rel_eur$bT.1
  nun_tr_2 <- nun_tr_rel_eur$bT.2
  nun_tr_3 <- nun_tr_rel_eur$bT.3
  nun_tr_4 <- nun_tr_rel_eur$bT.4
  nun_tr_5 <- nun_tr_rel_eur$bT.5
  nun_tr_mu <- c(mean(nun_tr_1),mean(nun_tr_2),mean(nun_tr_3),mean(nun_tr_4),mean(nun_tr_5))
  nun_tr_low <- as.numeric(c(PI(nun_tr_1,0.9)[1],PI(nun_tr_2,0.9)[1],PI(nun_tr_3,0.9)[1],PI(nun_tr_4,0.9)[1],PI(nun_tr_5,0.9)[1]))
  nun_tr_upp <- as.numeric(c(PI(nun_tr_1,0.9)[2],PI(nun_tr_2,0.9)[2],PI(nun_tr_3,0.9)[2],PI(nun_tr_4,0.9)[2],PI(nun_tr_5,0.9)[2]))
  plot(NA,xlim=c(-6,6),ylim=c(0,7),xlab="log odds(nuns)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-6,6,length.out=5),labels=c("-6","-3","0","3","6"),cex.axis=2)
  mtext("C",3,line=0.25,adj=0,cex=2)
  points(nun_tr_mu,5:1,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:5) {
    lines(c(nun_tr_low[i],nun_tr_upp[i]),rep(c(5:1)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=3,col="gray75")
}


{
  ## Figure 7 (religion)
  ## celibacy ~ marital composition
  cel_mar_1 <- cel_mar_rel$bM.1
  cel_mar_2 <- cel_mar_rel$bM.2
  cel_mar_3 <- cel_mar_rel$bM.3
  cel_mar_4 <- cel_mar_rel$bM.4
  cel_mar_5 <- cel_mar_rel$bM.5
  cel_mar_mu <- c(mean(cel_mar_1),mean(cel_mar_2),mean(cel_mar_3),mean(cel_mar_4),mean(cel_mar_5))
  cel_mar_low <- as.numeric(c(PI(cel_mar_1,0.9)[1],PI(cel_mar_2,0.9)[1],PI(cel_mar_3,0.9)[1],PI(cel_mar_4,0.9)[1],PI(cel_mar_5,0.9)[1]))
  cel_mar_upp <- as.numeric(c(PI(cel_mar_1,0.9)[2],PI(cel_mar_2,0.9)[2],PI(cel_mar_3,0.9)[2],PI(cel_mar_4,0.9)[2],PI(cel_mar_5,0.9)[2]))
  plot(NA,xlim=c(-6,6),ylim=c(0,7),xlab="log odds(celibacy)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-6,6,length.out=5),labels=c("-6","-3","0","3","6"),cex.axis=2)
  mtext("A",3,line=0.25,adj=0,cex=2)
  points(cel_mar_mu,5:1,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:5) {
    lines(c(cel_mar_low[i],cel_mar_upp[i]),rep(c(5:1)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=3,col="gray75")
  legend(4,7,legend=c("Indigenous","Islam","Christianity","Buddhism","Hinduism"),col=cols,pch=21,lwd=5,bty="n",box.col=NA,cex=2,xpd=NA)
  
  ## monks ~ marital composition
  monk_mar_1 <- monk_mar_rel$bM.1
  monk_mar_2 <- monk_mar_rel$bM.2
  monk_mar_3 <- monk_mar_rel$bM.3
  monk_mar_4 <- monk_mar_rel$bM.4
  monk_mar_5 <- monk_mar_rel$bM.5
  monk_mar_mu <- c(mean(monk_mar_1),mean(monk_mar_2),mean(monk_mar_3),mean(monk_mar_4),mean(monk_mar_5))
  monk_mar_low <- as.numeric(c(PI(monk_mar_1,0.9)[1],PI(monk_mar_2,0.9)[1],PI(monk_mar_3,0.9)[1],PI(monk_mar_4,0.9)[1],PI(monk_mar_5,0.9)[1]))
  monk_mar_upp <- as.numeric(c(PI(monk_mar_1,0.9)[2],PI(monk_mar_2,0.9)[2],PI(monk_mar_3,0.9)[2],PI(monk_mar_4,0.9)[2],PI(monk_mar_5,0.9)[2]))
  plot(NA,xlim=c(-6,6),ylim=c(0,7),xlab="log odds(monks)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-6,6,length.out=5),labels=c("-6","-3","0","3","6"),cex.axis=2)
  mtext("B",3,line=0.25,adj=0,cex=2)
  points(monk_mar_mu,5:1,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:5) {
    lines(c(monk_mar_low[i],monk_mar_upp[i]),rep(c(5:1)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=3,col="gray75")
  
  ## nuns ~ marital composition
  nun_mar_1 <- nun_mar_rel$bM.1
  nun_mar_2 <- nun_mar_rel$bM.2
  nun_mar_3 <- nun_mar_rel$bM.3
  nun_mar_4 <- nun_mar_rel$bM.4
  nun_mar_5 <- nun_mar_rel$bM.5
  nun_mar_mu <- c(mean(nun_mar_1),mean(nun_mar_2),mean(nun_mar_3),mean(nun_mar_4),mean(nun_mar_5))
  nun_mar_low <- as.numeric(c(PI(nun_mar_1,0.9)[1],PI(nun_mar_2,0.9)[1],PI(nun_mar_3,0.9)[1],PI(nun_mar_4,0.9)[1],PI(nun_mar_5,0.9)[1]))
  nun_mar_upp <- as.numeric(c(PI(nun_mar_1,0.9)[2],PI(nun_mar_2,0.9)[2],PI(nun_mar_3,0.9)[2],PI(nun_mar_4,0.9)[2],PI(nun_mar_5,0.9)[2]))
  plot(NA,xlim=c(-6,6),ylim=c(0,7),xlab="log odds(nuns)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-6,6,length.out=5),labels=c("-6","-3","0","3","6"),cex.axis=2)
  mtext("C",3,line=0.25,adj=0,cex=2)
  points(nun_mar_mu,5:1,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:5) {
    lines(c(nun_mar_low[i],nun_mar_upp[i]),rep(c(5:1)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=3,col="gray75")
}


{
  ## Figure 8 (religion - Eurasia)
  ## celibacy ~ marital composition
  cel_mar_1 <- cel_mar_rel_eur$bM.1
  cel_mar_2 <- cel_mar_rel_eur$bM.2
  cel_mar_3 <- cel_mar_rel_eur$bM.3
  cel_mar_4 <- cel_mar_rel_eur$bM.4
  cel_mar_5 <- cel_mar_rel_eur$bM.5
  cel_mar_mu <- c(mean(cel_mar_1),mean(cel_mar_2),mean(cel_mar_3),mean(cel_mar_4),mean(cel_mar_5))
  cel_mar_low <- as.numeric(c(PI(cel_mar_1,0.9)[1],PI(cel_mar_2,0.9)[1],PI(cel_mar_3,0.9)[1],PI(cel_mar_4,0.9)[1],PI(cel_mar_5,0.9)[1]))
  cel_mar_upp <- as.numeric(c(PI(cel_mar_1,0.9)[2],PI(cel_mar_2,0.9)[2],PI(cel_mar_3,0.9)[2],PI(cel_mar_4,0.9)[2],PI(cel_mar_5,0.9)[2]))
  plot(NA,xlim=c(-6,6),ylim=c(0,7),xlab="log odds(celibacy)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-6,6,length.out=5),labels=c("-6","-3","0","3","6"),cex.axis=2)
  mtext("A",3,line=0.25,adj=0,cex=2)
  points(cel_mar_mu,5:1,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:5) {
    lines(c(cel_mar_low[i],cel_mar_upp[i]),rep(c(5:1)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=3,col="gray75")
  legend(4,7,legend=c("Indigenous","Islam","Christianity","Buddhism","Hinduism"),col=cols,pch=21,lwd=5,bty="n",box.col=NA,cex=2,xpd=NA)
  
  ## monks ~ marital composition
  monk_mar_1 <- monk_mar_rel_eur$bM.1
  monk_mar_2 <- monk_mar_rel_eur$bM.2
  monk_mar_3 <- monk_mar_rel_eur$bM.3
  monk_mar_4 <- monk_mar_rel_eur$bM.4
  monk_mar_5 <- monk_mar_rel_eur$bM.5
  monk_mar_mu <- c(mean(monk_mar_1),mean(monk_mar_2),mean(monk_mar_3),mean(monk_mar_4),mean(monk_mar_5))
  monk_mar_low <- as.numeric(c(PI(monk_mar_1,0.9)[1],PI(monk_mar_2,0.9)[1],PI(monk_mar_3,0.9)[1],PI(monk_mar_4,0.9)[1],PI(monk_mar_5,0.9)[1]))
  monk_mar_upp <- as.numeric(c(PI(monk_mar_1,0.9)[2],PI(monk_mar_2,0.9)[2],PI(monk_mar_3,0.9)[2],PI(monk_mar_4,0.9)[2],PI(monk_mar_5,0.9)[2]))
  plot(NA,xlim=c(-6,6),ylim=c(0,7),xlab="log odds(monks)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-6,6,length.out=5),labels=c("-6","-3","0","3","6"),cex.axis=2)
  mtext("B",3,line=0.25,adj=0,cex=2)
  points(monk_mar_mu,5:1,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:5) {
    lines(c(monk_mar_low[i],monk_mar_upp[i]),rep(c(5:1)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=3,col="gray75")
  
  ## nuns ~ marital composition
  nun_mar_1 <- nun_mar_rel_eur$bM.1
  nun_mar_2 <- nun_mar_rel_eur$bM.2
  nun_mar_3 <- nun_mar_rel_eur$bM.3
  nun_mar_4 <- nun_mar_rel_eur$bM.4
  nun_mar_5 <- nun_mar_rel_eur$bM.5
  nun_mar_mu <- c(mean(nun_mar_1),mean(nun_mar_2),mean(nun_mar_3),mean(nun_mar_4),mean(nun_mar_5))
  nun_mar_low <- as.numeric(c(PI(nun_mar_1,0.9)[1],PI(nun_mar_2,0.9)[1],PI(nun_mar_3,0.9)[1],PI(nun_mar_4,0.9)[1],PI(nun_mar_5,0.9)[1]))
  nun_mar_upp <- as.numeric(c(PI(nun_mar_1,0.9)[2],PI(nun_mar_2,0.9)[2],PI(nun_mar_3,0.9)[2],PI(nun_mar_4,0.9)[2],PI(nun_mar_5,0.9)[2]))
  plot(NA,xlim=c(-6,6),ylim=c(0,7),xlab="log odds(nuns)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-6,6,length.out=5),labels=c("-6","-3","0","3","6"),cex.axis=2)
  mtext("C",3,line=0.25,adj=0,cex=2)
  points(nun_mar_mu,5:1,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:5) {
    lines(c(nun_mar_low[i],nun_mar_upp[i]),rep(c(5:1)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=3,col="gray75")
}


{
  ## Figure 9 (religion)
  ## celibacy ~ wealth inheritance
  cel_in_1 <- cel_in_rel$bI.1
  cel_in_2 <- cel_in_rel$bI.2
  cel_in_3 <- cel_in_rel$bI.3
  cel_in_4 <- cel_in_rel$bI.4
  cel_in_5 <- cel_in_rel$bI.5
  cel_in_mu <- c(mean(cel_in_1),mean(cel_in_2),mean(cel_in_3),mean(cel_in_4),mean(cel_in_5))
  cel_in_low <- as.numeric(c(PI(cel_in_1,0.9)[1],PI(cel_in_2,0.9)[1],PI(cel_in_3,0.9)[1],PI(cel_in_4,0.9)[1],PI(cel_in_5,0.9)[1]))
  cel_in_upp <- as.numeric(c(PI(cel_in_1,0.9)[2],PI(cel_in_2,0.9)[2],PI(cel_in_3,0.9)[2],PI(cel_in_4,0.9)[2],PI(cel_in_5,0.9)[2]))
  plot(NA,xlim=c(-6,6),ylim=c(0,7),xlab="log odds(celibacy)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-6,6,length.out=5),labels=c("-6","-3","0","3","6"),cex.axis=2)
  mtext("A",3,line=0.25,adj=0,cex=2)
  points(cel_in_mu,5:1,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:5) {
    lines(c(cel_in_low[i],cel_in_upp[i]),rep(c(5:1)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=3,col="gray75")
  legend(4,7,legend=c("Indigenous","Islam","Christianity","Buddhism","Hinduism"),col=cols,pch=21,lwd=5,bty="n",box.col=NA,cex=2,xpd=NA)
  
  ## monks ~ wealth inheritance
  monk_in_1 <- monk_in_rel$bI.1
  monk_in_2 <- monk_in_rel$bI.2
  monk_in_3 <- monk_in_rel$bI.3
  monk_in_4 <- monk_in_rel$bI.4
  monk_in_5 <- monk_in_rel$bI.5
  monk_in_mu <- c(mean(monk_in_1),mean(monk_in_2),mean(monk_in_3),mean(monk_in_4),mean(monk_in_5))
  monk_in_low <- as.numeric(c(PI(monk_in_1,0.9)[1],PI(monk_in_2,0.9)[1],PI(monk_in_3,0.9)[1],PI(monk_in_4,0.9)[1],PI(monk_in_5,0.9)[1]))
  monk_in_upp <- as.numeric(c(PI(monk_in_1,0.9)[2],PI(monk_in_2,0.9)[2],PI(monk_in_3,0.9)[2],PI(monk_in_4,0.9)[2],PI(monk_in_5,0.9)[2]))
  plot(NA,xlim=c(-6,6),ylim=c(0,7),xlab="log odds(monks)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-6,6,length.out=5),labels=c("-6","-3","0","3","6"),cex.axis=2)
  mtext("B",3,line=0.25,adj=0,cex=2)
  points(monk_in_mu,5:1,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:5) {
    lines(c(monk_in_low[i],monk_in_upp[i]),rep(c(5:1)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=3,col="gray75")
}


{
  par(mfrow=c(3,1),mar=c(5.1,5.1,3.1,5.1))
  ## Figure 10 (religion - Eurasia)
  ## celibacy ~ wealth inheritance
  cel_in_1 <- cel_in_rel_eur$bI.1
  cel_in_2 <- cel_in_rel_eur$bI.2
  cel_in_3 <- cel_in_rel_eur$bI.3
  cel_in_4 <- cel_in_rel_eur$bI.4
  cel_in_5 <- cel_in_rel_eur$bI.5
  cel_in_mu <- c(mean(cel_in_1),mean(cel_in_2),mean(cel_in_3),mean(cel_in_4),mean(cel_in_5))
  cel_in_low <- as.numeric(c(PI(cel_in_1,0.9)[1],PI(cel_in_2,0.9)[1],PI(cel_in_3,0.9)[1],PI(cel_in_4,0.9)[1],PI(cel_in_5,0.9)[1]))
  cel_in_upp <- as.numeric(c(PI(cel_in_1,0.9)[2],PI(cel_in_2,0.9)[2],PI(cel_in_3,0.9)[2],PI(cel_in_4,0.9)[2],PI(cel_in_5,0.9)[2]))
  plot(NA,xlim=c(-6,6),ylim=c(0,7),xlab="log odds(celibacy)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-6,6,length.out=5),labels=c("-6","-3","0","3","6"),cex.axis=2)
  mtext("A",3,line=0.25,adj=0,cex=2)
  points(cel_in_mu,5:1,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:5) {
    lines(c(cel_in_low[i],cel_in_upp[i]),rep(c(5:1)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=3,col="gray75")
  legend(4,7,legend=c("Indigenous","Islam","Christianity","Buddhism","Hinduism"),col=cols,pch=21,lwd=5,bty="n",box.col=NA,cex=2,xpd=NA)
  
  ## monks ~ wealth inheritance
  monk_in_1 <- monk_in_rel_eur$bI.1
  monk_in_2 <- monk_in_rel_eur$bI.2
  monk_in_3 <- monk_in_rel_eur$bI.3
  monk_in_4 <- monk_in_rel_eur$bI.4
  monk_in_5 <- monk_in_rel_eur$bI.5
  monk_in_mu <- c(mean(monk_in_1),mean(monk_in_2),mean(monk_in_3),mean(monk_in_4),mean(monk_in_5))
  monk_in_low <- as.numeric(c(PI(monk_in_1,0.9)[1],PI(monk_in_2,0.9)[1],PI(monk_in_3,0.9)[1],PI(monk_in_4,0.9)[1],PI(monk_in_5,0.9)[1]))
  monk_in_upp <- as.numeric(c(PI(monk_in_1,0.9)[2],PI(monk_in_2,0.9)[2],PI(monk_in_3,0.9)[2],PI(monk_in_4,0.9)[2],PI(monk_in_5,0.9)[2]))
  plot(NA,xlim=c(-6,6),ylim=c(0,7),xlab="log odds(monks)",ylab="",main="",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(-6,6,length.out=5),labels=c("-6","-3","0","3","6"),cex.axis=2)
  mtext("B",3,line=0.25,adj=0,cex=2)
  points(monk_in_mu,5:1,col=cols,pch=1,cex=5,lwd=5)
  for (i in 1:5) {
    lines(c(monk_in_low[i],monk_in_upp[i]),rep(c(5:1)[i],2),lwd=5,col=cols[i])
  }
  abline(v=0,lty=2,lwd=3,col="gray75")
}


{
  ## Figure 11
  names <- c("absent","present")
  par(mfrow=c(3,3),mar=c(5.1,5.1,3.1,5.1))
  
  ## celibacy ~ marriage transactions
  cel0 <- rbinom(n,size=1,prob=inv_logit(cel_tr$a + cel_tr$bT*0))
  cel1 <- rbinom(n,size=1,prob=inv_logit(cel_tr$a + cel_tr$bT*1))
  cel_sim <- gather(data.frame(cel0,cel1))
  barplot(table(cel_sim),xlab="celibacy",ylab="no. of societies",beside=TRUE,col=c("gray75","#547e93"),border=NA,names.arg=names,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("A",3,line=0.25,adj=0)
  legend(2.5,1500,legend=c("absent","present"),title="transactions",col=c("gray75","#547e93"),pch=15,box.col="black",cex=1.5)
  
  ## celibacy ~ marital composition
  cel1 <- rbinom(n,size=1,prob=inv_logit(cel_mar$a + cel_mar$bM*1))
  cel3 <- rbinom(n,size=1,prob=inv_logit(cel_mar$a + cel_mar$bM*3))
  cel_sim <- gather(data.frame(cel1,cel3))
  barplot(table(cel_sim),xlab="celibacy",ylab="no. of societies",beside=TRUE,col=c("gray75","#547e93"),border=NA,names.arg=names,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(2.5,1500,legend=c("polygyny","polyandry"),title="marriage",col=c("gray75","#547e93"),pch=15,box.col="black",cex=1.5)
  
  ## celibacy ~ wealth inheritance
  cel0 <- rbinom(n,size=1,prob=inv_logit(cel_in$a + cel_in$bI*0))
  cel1 <- rbinom(n,size=1,prob=inv_logit(cel_in$a + cel_in$bI*1))
  cel_sim <- gather(data.frame(cel0,cel1))
  barplot(table(cel_sim),xlab="celibacy",ylab="no. of societies",beside=TRUE,col=c("gray75","#547e93"),border=NA,names.arg=names,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("C",3,line=0.25,adj=0)
  legend(2.5,1500,legend=c("absent","present"),title="inheritance",col=c("gray75","#547e93"),pch=15,box.col="black",cex=1.5)
  
  ## monks ~ marriage transactions
  monk0 <- rbinom(n,size=1,prob=inv_logit(monk_tr$a + monk_tr$bT*0))
  monk1 <- rbinom(n,size=1,prob=inv_logit(monk_tr$a + monk_tr$bT*1))
  monk_sim <- gather(data.frame(monk0,monk1))
  barplot(table(monk_sim),xlab="male celibacy",ylab="no. of societies",beside=TRUE,col=c("gray75","goldenrod1"),border=NA,names.arg=names,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("D",3,line=0.25,adj=0)
  legend(1,1200,legend=c("bride's side","groom's side"),title="transactions",col=c("gray75","goldenrod1"),pch=15,box.col="black",cex=1.5)
  
  ## monks ~ marital composition
  monk0 <- rbinom(n,size=1,prob=inv_logit(monk_mar$a + monk_mar$bM*0))
  monk1 <- rbinom(n,size=1,prob=inv_logit(monk_mar$a + monk_mar$bM*1))
  monk_sim <- gather(data.frame(monk0,monk1))
  barplot(table(monk_sim),xlab="male celibacy",ylab="no. of societies",beside=TRUE,col=c("gray75","goldenrod1"),border=NA,names.arg=names,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("E",3,line=0.25,adj=0)
  legend(1,1200,legend=c("monogamy","polyg./polyandry"),title="marriage",col=c("gray75","goldenrod1"),pch=15,box.col="black",cex=1.5)
  
  ## monks ~ wealth inheritance
  monk0 <- rbinom(n,size=1,prob=inv_logit(monk_in$a + monk_in$bI*0))
  monk1 <- rbinom(n,size=1,prob=inv_logit(monk_in$a + monk_in$bI*1))
  monk_sim <- gather(data.frame(monk0,monk1))
  barplot(table(monk_sim),xlab="male celibacy",ylab="no. of societies",beside=TRUE,col=c("gray75","goldenrod1"),border=NA,names.arg=names,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("F",3,line=0.25,adj=0)
  legend(1,1200,legend=c("non-patrilineal","patrilineal"),title="inheritance",col=c("gray75","goldenrod1"),pch=15,box.col="black",cex=1.5)
  
  ## nuns ~ marriage transactions
  nun0 <- rbinom(n,size=1,prob=inv_logit(nun_tr$a + nun_tr$bT*0))
  nun1 <- rbinom(n,size=1,prob=inv_logit(nun_tr$a + nun_tr$bT*1))
  nun_sim <- gather(data.frame(nun0,nun1))
  barplot(table(nun_sim),xlab="female celibacy",ylab="no. of societies",beside=TRUE,col=c("gray75","orangered2"),border=NA,names.arg=names,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("G",3,line=0.25,adj=0)
  legend(1.5,600,legend=c("groom's side","bride's side"),title="transactions",col=c("gray75","orangered2"),pch=15,box.col="black",cex=1.5)
  
  ## nuns ~ marital composition
  nun0 <- rbinom(n,size=1,prob=inv_logit(nun_mar$a + nun_mar$bM*0))
  nun1 <- rbinom(n,size=1,prob=inv_logit(nun_mar$a + nun_mar$bM*1))
  nun_sim <- gather(data.frame(nun0,nun1))
  barplot(table(nun_sim),xlab="female celibacy",ylab="no. of societies",beside=TRUE,col=c("gray75","orangered2"),border=NA,names.arg=names,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("H",3,line=0.25,adj=0)
  legend(1.5,600,legend=c("polygyny","monog./polyandry"),title="marriage",col=c("gray75","orangered2"),pch=15,box.col="black",cex=1.5)
}


{
  ## Figure 12 (Eurasia)
  par(mfrow=c(3,3),mar=c(5.1,5.1,3.1,5.1))
  
  ## celibacy ~ marriage transactions
  cel0 <- rbinom(n,size=1,prob=inv_logit(cel_tr_eur$a + cel_tr_eur$bT*0))
  cel1 <- rbinom(n,size=1,prob=inv_logit(cel_tr_eur$a + cel_tr_eur$bT*1))
  cel_sim <- gather(data.frame(cel0,cel1))
  barplot(table(cel_sim),xlab="celibacy",ylab="no. of societies",beside=TRUE,col=c("gray75","#547e93"),border=NA,names.arg=names,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("A",3,line=0.25,adj=0)
  legend(2,1200,legend=c("absent","present"),title="transactions",col=c("gray75","#547e93"),pch=15,box.col="black",cex=1.5)
  
  ## celibacy ~ marital composition
  cel1 <- rbinom(n,size=1,prob=inv_logit(cel_mar_eur$a + cel_mar_eur$bM*1))
  cel3 <- rbinom(n,size=1,prob=inv_logit(cel_mar_eur$a + cel_mar_eur$bM*3))
  cel_sim <- gather(data.frame(cel1,cel3))
  barplot(table(cel_sim),xlab="celibacy",ylab="no. of societies",beside=TRUE,col=c("gray75","#547e93"),border=NA,names.arg=names,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("B",3,line=0.25,adj=0)
  legend(3,1200,legend=c("polygyny","polyandry"),title="marriage",col=c("gray75","#547e93"),pch=15,box.col="black",cex=1.5)
  
  ## celibacy ~ wealth inheritance
  cel0 <- rbinom(n,size=1,prob=inv_logit(cel_in_eur$a + cel_in_eur$bI*0))
  cel1 <- rbinom(n,size=1,prob=inv_logit(cel_in_eur$a + cel_in_eur$bI*1))
  cel_sim <- gather(data.frame(cel0,cel1))
  barplot(table(cel_sim),xlab="celibacy",ylab="no. of societies",beside=TRUE,col=c("gray75","#547e93"),border=NA,names.arg=names,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("C",3,line=0.25,adj=0)
  legend(3,1200,legend=c("absent","present"),title="inheritance",col=c("gray75","#547e93"),pch=15,box.col="black",cex=1.5)
  
  ## monks ~ marriage transactions
  monk0 <- rbinom(n,size=1,prob=inv_logit(monk_tr_eur$a + monk_tr_eur$bT*0))
  monk1 <- rbinom(n,size=1,prob=inv_logit(monk_tr_eur$a + monk_tr_eur$bT*1))
  monk_sim <- gather(data.frame(monk0,monk1))
  barplot(table(monk_sim),xlab="male celibacy",ylab="no. of societies",beside=TRUE,col=c("gray75","goldenrod1"),border=NA,names.arg=names,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("D",3,line=0.25,adj=0)
  legend(1,1200,legend=c("bride's side","groom's side"),title="transactions",col=c("gray75","goldenrod1"),pch=15,box.col="black",cex=1.5)
  
  ## monks ~ marital composition
  monk0 <- rbinom(n,size=1,prob=inv_logit(monk_mar_eur$a + monk_mar_eur$bM*0))
  monk1 <- rbinom(n,size=1,prob=inv_logit(monk_mar_eur$a + monk_mar_eur$bM*1))
  monk_sim <- gather(data.frame(monk0,monk1))
  barplot(table(monk_sim),xlab="male celibacy",ylab="no. of societies",beside=TRUE,col=c("gray75","goldenrod1"),border=NA,names.arg=names,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("E",3,line=0.25,adj=0)
  legend(1,1200,legend=c("monogamy","polyg./polyandry"),title="marriage",col=c("gray75","goldenrod1"),pch=15,box.col="black",cex=1.5)
  
  ## monks ~ wealth inheritance
  monk0 <- rbinom(n,size=1,prob=inv_logit(monk_in_eur$a + monk_in_eur$bI*0))
  monk1 <- rbinom(n,size=1,prob=inv_logit(monk_in_eur$a + monk_in_eur$bI*1))
  monk_sim <- gather(data.frame(monk0,monk1))
  barplot(table(monk_sim),xlab="male celibacy",ylab="no. of societies",beside=TRUE,col=c("gray75","goldenrod1"),border=NA,names.arg=names,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("F",3,line=0.25,adj=0)
  legend(1,1200,legend=c("non-patrilineal","patrilineal"),title="inheritance",col=c("gray75","goldenrod1"),pch=15,box.col="black",cex=1.5)
  
  ## nuns ~ marriage transactions
  nun0 <- rbinom(n,size=1,prob=inv_logit(nun_tr_eur$a + nun_tr_eur$bT*0))
  nun1 <- rbinom(n,size=1,prob=inv_logit(nun_tr_eur$a + nun_tr_eur$bT*1))
  nun_sim <- gather(data.frame(nun0,nun1))
  barplot(table(nun_sim),xlab="female celibacy",ylab="no. of societies",beside=TRUE,col=c("gray75","orangered2"),border=NA,names.arg=names,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("G",3,line=0.25,adj=0)
  legend(1.5,600,legend=c("groom's side","bride's side"),title="transactions",col=c("gray75","orangered2"),pch=15,box.col="black",cex=1.5)
  
  ## nuns ~ marital composition
  nun0 <- rbinom(n,size=1,prob=inv_logit(nun_mar_eur$a + nun_mar_eur$bM*0))
  nun1 <- rbinom(n,size=1,prob=inv_logit(nun_mar_eur$a + nun_mar_eur$bM*1))
  nun_sim <- gather(data.frame(nun0,nun1))
  barplot(table(nun_sim),xlab="female celibacy",ylab="no. of societies",beside=TRUE,col=c("gray75","orangered2"),border=NA,names.arg=names,cex=1.5,cex.lab=1.5,cex.axis=1.5)
  mtext("H",3,line=0.25,adj=0)
  legend(1.5,600,legend=c("polygyny","monog./polyandry"),title="marriage",col=c("gray75","orangered2"),pch=15,box.col="black",cex=1.5)
}


{
  ## Figure 
  par(mfrow=c(3,3),mar=c(5.1,5.1,3.1,5.1))
  legend <- c("religion - intercepts","religion - slopes","phylogeny")
  cols <- c("#66a182","#154360","#8d96a3")
  
  ## celibacy ~ marriage transactions
  cel_tr_a <- density(cel_tr_rel$sigma_a)
  cel_tr_b <- density(cel_tr_rel$sigma_bT)
  cel_tr_p <- density(cel_tr_rel$sigma_phy)
  plot(cel_tr_a,xlab="posterior",ylab="density",main="",xlim=c(0,5),ylim=c(0,1),col="#66a182",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(0,5,length.out=4),labels=c("0","2","4","6"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("A",3,line=0.25,adj=0,cex=2)
  text(3,0.9,expression(sigma[a]* ~ "= 3.03"),cex=2.5,col="#66a182")
  text(3,0.75,expression(sigma[bT]* ~ "= 1.15"),cex=2.5,col="#154360")
  text(3,0.6,expression(sigma[phy]* ~ "= 2.17"),cex=2.5,col="#8d96a3")
  lines(cel_tr_b,col="#154360",lwd=3)
  lines(cel_tr_p,col="#8d96a3",lwd=3)
  polygon(cel_tr_p,col=col.alpha("#8d96a3",alpha=0.7),border=NA)
  polygon(cel_tr_a,col=col.alpha("#66a182",alpha=0.7),border=NA)
  polygon(cel_tr_b,col=col.alpha("#154360",alpha=0.7),border=NA)
  
  ## celibacy ~ marital composition
  cel_mar_a <- density(cel_mar_rel$sigma_a)
  cel_mar_b <- density(cel_mar_rel$sigma_bM)
  cel_mar_p <- density(cel_mar_rel$sigma_phy)
  plot(cel_mar_a,xlab="posterior",ylab="density",main="",xlim=c(0,5),ylim=c(0,1),col="#66a182",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(0,5,length.out=4),labels=c("0","2","4","6"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("B",3,line=0.25,adj=0,cex=2)
  text(3,0.9,expression(sigma[a]* ~ "= 4.22"),cex=2.5,col="#66a182")
  text(3,0.75,expression(sigma[bM] ~ "= 0.67"),cex=2.5,col="#154360")
  text(3,0.6,expression(sigma[phy]* ~ "= 1.41"),cex=2.5,col="#8d96a3")
  lines(cel_mar_b,col="#154360",lwd=3)
  lines(cel_mar_p,col="#8d96a3",lwd=3)
  polygon(cel_mar_p,col=col.alpha("#8d96a3",alpha=0.7),border=NA)
  polygon(cel_mar_a,col=col.alpha("#66a182",alpha=0.7),border=NA)
  polygon(cel_mar_b,col=col.alpha("#154360",alpha=0.7),border=NA)
  
  ## celibacy ~ wealth inheritance
  cel_in_a <- density(cel_in_rel$sigma_a)
  cel_in_b <- density(cel_in_rel$sigma_bI)
  cel_in_p <- density(cel_in_rel$sigma_phy)
  plot(cel_in_a,xlab="posterior",ylab="density",main="",xlim=c(0,5),ylim=c(0,1),col="#66a182",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(0,5,length.out=4),labels=c("0","2","4","6"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("C",3,line=0.25,adj=0,cex=2)
  text(3,0.9,expression(sigma[a]* ~ "= 4.05"),cex=2.5,col="#66a182")
  text(3,0.75,expression(sigma[bI] ~ "= 0.57"),cex=2.5,col="#154360")
  text(3,0.6,expression(sigma[phy]* ~ "= 1.46"),cex=2.5,col="#8d96a3")
  lines(cel_in_b,col="#154360",lwd=3)
  lines(cel_in_p,col="#8d96a3",lwd=3)
  polygon(cel_in_p,col=col.alpha("#8d96a3",alpha=0.7),border=NA)
  polygon(cel_in_a,col=col.alpha("#66a182",alpha=0.7),border=NA)
  polygon(cel_in_b,col=col.alpha("#154360",alpha=0.7),border=NA)
  
  ## monks ~ marriage transactions
  monk_tr_a <- density(monk_tr_rel$sigma_a)
  monk_tr_b <- density(monk_tr_rel$sigma_bT)
  monk_tr_p <- density(monk_tr_rel$sigma_phy)
  plot(monk_tr_a,xlab="posterior",ylab="density",main="",xlim=c(0,5),ylim=c(0,1),col="#66a182",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(0,5,length.out=4),labels=c("0","2","4","6"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("D",3,line=0.25,adj=0,cex=2)
  text(3,0.9,expression(sigma[a]* ~ "= 1.26"),cex=2.5,col="#66a182")
  text(3,0.75,expression(sigma[bT] ~ "= 2.08"),cex=2.5,col="#154360")
  text(3,0.6,expression(sigma[phy]* ~ "= 0.88"),cex=2.5,col="#8d96a3")
  lines(monk_tr_b,col="#154360",lwd=3)
  lines(monk_tr_p,col="#8d96a3",lwd=3)
  polygon(monk_tr_p,col=col.alpha("#8d96a3",alpha=0.7),border=NA)
  polygon(monk_tr_a,col=col.alpha("#66a182",alpha=0.7),border=NA)
  polygon(monk_tr_b,col=col.alpha("#154360",alpha=0.7),border=NA)
  
  ## monks ~ marital composition
  monk_mar_a <- density(monk_mar_rel$sigma_a)
  monk_mar_b <- density(monk_mar_rel$sigma_bM)
  monk_mar_p <- density(monk_mar_rel$sigma_phy)
  plot(monk_mar_a,xlab="posterior",ylab="density",main="",xlim=c(0,5),ylim=c(0,1),col="#66a182",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(0,5,length.out=4),labels=c("0","2","4","6"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("E",3,line=0.25,adj=0,cex=2)
  text(3,0.9,expression(sigma[a]* ~ "= 0.95"),cex=2.5,col="#66a182")
  text(3,0.75,expression(sigma[bM] ~ "= 2.13"),cex=2.5,col="#154360")
  text(3,0.6,expression(sigma[phy]* ~ "= 0.80"),cex=2.5,col="#8d96a3")
  lines(monk_mar_b,col="#154360",lwd=3)
  lines(monk_mar_p,col="#8d96a3",lwd=3)
  polygon(monk_mar_p,col=col.alpha("#8d96a3",alpha=0.7),border=NA)
  polygon(monk_mar_a,col=col.alpha("#66a182",alpha=0.7),border=NA)
  polygon(monk_mar_b,col=col.alpha("#154360",alpha=0.7),border=NA)
  
  ## monks ~ wealth inheritance
  monk_in_a <- density(monk_in_rel$sigma_a)
  monk_in_b <- density(monk_in_rel$sigma_bI)
  monk_in_p <- density(monk_in_rel$sigma_phy)
  plot(monk_in_a,xlab="posterior",ylab="density",main="",xlim=c(0,5),ylim=c(0,1),col="#66a182",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(0,5,length.out=4),labels=c("0","2","4","6"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("F",3,line=0.25,adj=0,cex=2)
  text(3,0.9,expression(sigma[a]* ~ "= 1.14"),cex=2.5,col="#66a182")
  text(3,0.75,expression(sigma[bI] ~ "= 1.93"),cex=2.5,col="#154360")
  text(3,0.6,expression(sigma[phy]* ~ "= 0.79"),cex=2.5,col="#8d96a3")
  lines(monk_in_b,col="#154360",lwd=3)
  lines(monk_in_p,col="#8d96a3",lwd=3)
  polygon(monk_in_p,col=col.alpha("#8d96a3",alpha=0.7),border=NA)
  polygon(monk_in_a,col=col.alpha("#66a182",alpha=0.7),border=NA)
  polygon(monk_in_b,col=col.alpha("#154360",alpha=0.7),border=NA)
  
  ## nuns ~ marriage transactions
  nun_tr_a <- density(nun_tr_rel$sigma_a)
  nun_tr_b <- density(nun_tr_rel$sigma_bT)
  nun_tr_p <- density(nun_tr_rel$sigma_phy)
  plot(nun_tr_a,xlab="posterior",ylab="density",main="",xlim=c(0,5),ylim=c(0,1),col="#66a182",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(0,5,length.out=4),labels=c("0","2","4","6"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("G",3,line=0.25,adj=0,cex=2)
  text(3,0.9,expression(sigma[a]* ~ "= 1.07"),cex=2.5,col="#66a182")
  text(3,0.75,expression(sigma[bT] ~ "= 0.90"),cex=2.5,col="#154360")
  text(3,0.6,expression(sigma[phy]* ~ "= 0.98"),cex=2.5,col="#8d96a3")
  lines(nun_tr_b,col="#154360",lwd=3)
  lines(nun_tr_p,col="#8d96a3",lwd=3)
  polygon(nun_tr_p,col=col.alpha("#8d96a3",alpha=0.7),border=NA)
  polygon(nun_tr_a,col=col.alpha("#66a182",alpha=0.7),border=NA)
  polygon(nun_tr_b,col=col.alpha("#154360",alpha=0.7),border=NA)
  
  ## nuns ~ marital composition
  nun_mar_a <- density(nun_mar_rel$sigma_a)
  nun_mar_b <- density(nun_mar_rel$sigma_bM)
  nun_mar_p <- density(nun_mar_rel$sigma_phy)
  plot(nun_mar_a,xlab="posterior",ylab="density",main="",xlim=c(0,5),ylim=c(0,1),col="#66a182",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(0,5,length.out=4),labels=c("0","2","4","6"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("H",3,line=0.25,adj=0,cex=2)
  text(3,0.9,expression(sigma[a]* ~ "= 0.80"),cex=2.5,col="#66a182")
  text(3,0.75,expression(sigma[bM] ~ "= 0.79"),cex=2.5,col="#154360")
  text(3,0.6,expression(sigma[phy]* ~ "= 1.19"),cex=2.5,col="#8d96a3")
  lines(nun_mar_b,col="#154360",lwd=3)
  lines(nun_mar_p,col="#8d96a3",lwd=3)
  polygon(nun_mar_p,col=col.alpha("#8d96a3",alpha=0.7),border=NA)
  polygon(nun_mar_a,col=col.alpha("#66a182",alpha=0.7),border=NA)
  polygon(nun_mar_b,col=col.alpha("#154360",alpha=0.7),border=NA)
  
  ## add legend
  legend(7.5,0.75,col=cols,legend=legend,pch=19,cex=2,box.col=NA,xpd=NA)
}


{
  ## Figure (Eurasia)
  par(mfrow=c(3,3),mar=c(5.1,5.1,3.1,5.1))
  
  ## celibacy ~ marriage transactions
  cel_tr_a <- density(cel_tr_rel_eur$sigma_a)
  cel_tr_b <- density(cel_tr_rel_eur$sigma_bT)
  cel_tr_p <- density(cel_tr_rel_eur$sigma_phy)
  plot(cel_tr_a,xlab="posterior",ylab="density",main="",xlim=c(0,5),ylim=c(0,1),col="#66a182",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(0,5,length.out=4),labels=c("0","2","4","6"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("A",3,line=0.25,adj=0,cex=2)
  text(3,0.9,expression(sigma[a]* ~ "= 2.09"),cex=2.5,col="#66a182")
  text(3,0.75,expression(sigma[bT] ~ "= 0.98"),cex=2.5,col="#154360")
  text(3,0.6,expression(sigma[phy]* ~ "= 1.18"),cex=2.5,col="#8d96a3")
  lines(cel_tr_b,col="#154360",lwd=3)
  lines(cel_tr_p,col="#8d96a3",lwd=3)
  polygon(cel_tr_p,col=col.alpha("#8d96a3",alpha=0.7),border=NA)
  polygon(cel_tr_a,col=col.alpha("#66a182",alpha=0.7),border=NA)
  polygon(cel_tr_b,col=col.alpha("#154360",alpha=0.7),border=NA)
  
  ## celibacy ~ marital composition
  cel_mar_a <- density(cel_mar_rel_eur$sigma_a)
  cel_mar_b <- density(cel_mar_rel_eur$sigma_bM)
  cel_mar_p <- density(cel_mar_rel_eur$sigma_phy)
  plot(cel_mar_a,xlab="posterior",ylab="density",main="",xlim=c(0,5),ylim=c(0,1),col="#66a182",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(0,5,length.out=4),labels=c("0","2","4","6"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("B",3,line=0.25,adj=0,cex=2)
  text(3,0.9,expression(sigma[a]* ~ "= 1.96"),cex=2.5,col="#66a182")
  text(3,0.75,expression(sigma[bM] ~ "= 0.86"),cex=2.5,col="#154360")
  text(3,0.6,expression(sigma[phy]* ~ "= 1.18"),cex=2.5,col="#8d96a3")
  lines(cel_mar_b,col="#154360",lwd=3)
  lines(cel_mar_p,col="#8d96a3",lwd=3)
  polygon(cel_mar_p,col=col.alpha("#8d96a3",alpha=0.7),border=NA)
  polygon(cel_mar_a,col=col.alpha("#66a182",alpha=0.7),border=NA)
  polygon(cel_mar_b,col=col.alpha("#154360",alpha=0.7),border=NA)
  
  ## celibacy ~ wealth inheritance
  cel_in_a <- density(cel_in_rel_eur$sigma_a)
  cel_in_b <- density(cel_in_rel_eur$sigma_bI)
  cel_in_p <- density(cel_in_rel_eur$sigma_phy)
  plot(cel_in_a,xlab="posterior",ylab="density",main="",xlim=c(0,5),ylim=c(0,1),col="#66a182",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(0,5,length.out=4),labels=c("0","2","4","6"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("C",3,line=0.25,adj=0,cex=2)
  text(3,0.9,expression(sigma[a]* ~ "= 2.40"),cex=2.5,col="#66a182")
  text(3,0.75,expression(sigma[bI] ~ "= 0.79"),cex=2.5,col="#154360")
  text(3,0.6,expression(sigma[phy]* ~ "= 1.10"),cex=2.5,col="#8d96a3")
  lines(cel_in_b,col="#154360",lwd=3)
  lines(cel_in_p,col="#8d96a3",lwd=3)
  polygon(cel_in_p,col=col.alpha("#8d96a3",alpha=0.7),border=NA)
  polygon(cel_in_a,col=col.alpha("#66a182",alpha=0.7),border=NA)
  polygon(cel_in_b,col=col.alpha("#154360",alpha=0.7),border=NA)
  
  ## monks ~ marriage transactions
  monk_tr_a <- density(monk_tr_rel_eur$sigma_a)
  monk_tr_b <- density(monk_tr_rel_eur$sigma_bT)
  monk_tr_p <- density(monk_tr_rel_eur$sigma_phy)
  plot(monk_tr_a,xlab="posterior",ylab="density",main="",xlim=c(0,5),ylim=c(0,1),col="#66a182",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(0,5,length.out=4),labels=c("0","2","4","6"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("D",3,line=0.25,adj=0,cex=2)
  text(3,0.9,expression(sigma[a]* ~ "= 1.28"),cex=2.5,col="#66a182")
  text(3,0.75,expression(sigma[bT] ~ "= 2.45"),cex=2.5,col="#154360")
  text(3,0.6,expression(sigma[phy]* ~ "= 0.83"),cex=2.5,col="#8d96a3")
  lines(monk_tr_b,col="#154360",lwd=3)
  lines(monk_tr_p,col="#8d96a3",lwd=3)
  polygon(monk_tr_p,col=col.alpha("#8d96a3",alpha=0.7),border=NA)
  polygon(monk_tr_a,col=col.alpha("#66a182",alpha=0.7),border=NA)
  polygon(monk_tr_b,col=col.alpha("#154360",alpha=0.7),border=NA)
  
  ## monks ~ marital composition
  monk_mar_a <- density(monk_mar_rel_eur$sigma_a)
  monk_mar_b <- density(monk_mar_rel_eur$sigma_bM)
  monk_mar_p <- density(monk_mar_rel_eur$sigma_phy)
  plot(monk_mar_a,xlab="posterior",ylab="density",main="",xlim=c(0,5),ylim=c(0,1),col="#66a182",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(0,5,length.out=4),labels=c("0","2","4","6"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("E",3,line=0.25,adj=0,cex=2)
  text(3,0.9,expression(sigma[a]* ~ "= 1.10"),cex=2.5,col="#66a182")
  text(3,0.75,expression(sigma[bM] ~ "= 2.41"),cex=2.5,col="#154360")
  text(3,0.6,expression(sigma[phy]* ~ "= 0.85"),cex=2.5,col="#8d96a3")
  lines(monk_mar_b,col="#154360",lwd=3)
  lines(monk_mar_p,col="#8d96a3",lwd=3)
  polygon(monk_mar_p,col=col.alpha("#8d96a3",alpha=0.7),border=NA)
  polygon(monk_mar_a,col=col.alpha("#66a182",alpha=0.7),border=NA)
  polygon(monk_mar_b,col=col.alpha("#154360",alpha=0.7),border=NA)
  
  ## monks ~ wealth inheritance
  monk_in_a <- density(monk_in_rel_eur$sigma_a)
  monk_in_b <- density(monk_in_rel_eur$sigma_bI)
  monk_in_p <- density(monk_in_rel_eur$sigma_phy)
  plot(monk_in_a,xlab="posterior",ylab="density",main="",xlim=c(0,5),ylim=c(0,1),col="#66a182",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(0,5,length.out=4),labels=c("0","2","4","6"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("F",3,line=0.25,adj=0,cex=2)
  text(3,0.9,expression(sigma[a]* ~ "= 1.32"),cex=2.5,col="#66a182")
  text(3,0.75,expression(sigma[bI] ~ "= 1.35"),cex=2.5,col="#154360")
  text(3,0.6,expression(sigma[phy]* ~ "= 0.94"),cex=2.5,col="#8d96a3")
  lines(monk_in_b,col="#154360",lwd=3)
  lines(monk_in_p,col="#8d96a3",lwd=3)
  polygon(monk_in_p,col=col.alpha("#8d96a3",alpha=0.7),border=NA)
  polygon(monk_in_a,col=col.alpha("#66a182",alpha=0.7),border=NA)
  polygon(monk_in_b,col=col.alpha("#154360",alpha=0.7),border=NA)
  
  ## nuns ~ marriage transactions
  nun_tr_a <- density(nun_tr_rel_eur$sigma_a)
  nun_tr_b <- density(nun_tr_rel_eur$sigma_bT)
  nun_tr_p <- density(nun_tr_rel_eur$sigma_phy)
  plot(nun_tr_a,xlab="posterior",ylab="density",main="",xlim=c(0,5),ylim=c(0,1),col="#66a182",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(0,5,length.out=4),labels=c("0","2","4","6"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("G",3,line=0.25,adj=0,cex=2)
  text(3,0.9,expression(sigma[a]* ~ "= 0.65"),cex=2.5,col="#66a182")
  text(3,0.75,expression(sigma[bT] ~ "= 1.06"),cex=2.5,col="#154360")
  text(3,0.6,expression(sigma[phy]* ~ "= 1.09"),cex=2.5,col="#8d96a3")
  lines(nun_tr_b,col="#154360",lwd=3)
  lines(nun_tr_p,col="#8d96a3",lwd=3)
  polygon(nun_tr_p,col=col.alpha("#8d96a3",alpha=0.7),border=NA)
  polygon(nun_tr_a,col=col.alpha("#66a182",alpha=0.7),border=NA)
  polygon(nun_tr_b,col=col.alpha("#154360",alpha=0.7),border=NA)
  
  ## nuns ~ marital composition
  nun_mar_a <- density(nun_mar_rel_eur$sigma_a)
  nun_mar_b <- density(nun_mar_rel_eur$sigma_bM)
  nun_mar_p <- density(nun_mar_rel_eur$sigma_phy)
  plot(nun_mar_a,xlab="posterior",ylab="density",main="",xlim=c(0,5),ylim=c(0,1),col="#66a182",lwd=3,cex.lab=2,yaxt="n",xaxt="n",axes=FALSE)
  axis(1,at=seq(0,5,length.out=4),labels=c("0","2","4","6"),cex.axis=2)
  axis(2,at=seq(0,1,length.out=3),labels=c("0","0.5","1"),cex.axis=2)
  mtext("H",3,line=0.25,adj=0,cex=2)
  text(3,0.9,expression(sigma[a]* ~ "= 0.62"),cex=2.5,col="#66a182")
  text(3,0.75,expression(sigma[bM] ~ "= 0.77"),cex=2.5,col="#154360")
  text(3,0.6,expression(sigma[phy]* ~ "= 1.22"),cex=2.5,col="#8d96a3")
  lines(nun_mar_b,col="#154360",lwd=3)
  lines(nun_mar_p,col="#8d96a3",lwd=3)
  polygon(nun_mar_p,col=col.alpha("#8d96a3",alpha=0.7),border=NA)
  polygon(nun_mar_a,col=col.alpha("#66a182",alpha=0.7),border=NA)
  polygon(nun_mar_b,col=col.alpha("#154360",alpha=0.7),border=NA)
  
  ## add legend
  legend(7.5,0.75,col=cols,legend=legend,pch=19,cex=2,box.col=NA,xpd=NA)
}


################################################################################