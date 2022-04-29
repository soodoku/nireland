##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##   ##
##      Northern Ireland/Open/Final Tables
##Last Edited: 6.08.12   
##   Gaurav Sood##
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++

# Set Working dir.
setwd("C:/Users/gsood/Dropbox/")

# Sourcing Common Functions
source("func/func.R")

# Load data
load("Ireland/data/nireland.rdata")
open_ended <- read.csv("Ireland/open.ended/open_ended.csv")

# Some convenient subsets
# Restricted to Participants and CG
ireland <- subset(nireland, !is.na(nireland$cserial) & ((is.na(nireland$filter) | nireland$filter=='selected') | nireland$t3partgrp))

##
##  Table 1 Learning of Arguments   
##  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   ##  

#library(exactRankTests) 
#library(MASS)

openfunc <- function(x,y, subset="all") {
# tester: x <- nireland$t2tot; y <- nireland$t3tot
# tester2: x <- nireland$t2.q18a.sum; y <- nireland$t3.q18a.sum
resdf <- data.frame(sample=1:3, t2=NA, t3=NA, cg=NA, diff.t2.cg=NA, diff.t3.cg=NA, diff.t3.t2=NA, n.t2part=NA, n.t3part=NA, n.cg=NA)
# Not missing at t2 or t3 # nireland$filter=='selected' & nireland$t3partgrp==1 & 
t2nona <-  !is.na(x) & !is.na(y) & !is.na(nireland$filter) & nireland$filter=='selected' & nireland$t3partgrp==1
t3nona <-  !is.na(x) & !is.na(y) & nireland$t3partgrp==1
nonac  <-  !is.na(y) & nireland$control 

t2 <- x[t2nona]
t3 <- y[t3nona]
cg <- y[nonac]

dv1 <- c(cg, t2)
iv1 <- c(rep(1,length(cg)), rep(0, length(t2))) 
#p1  <- summary(glm(dv1 ~ iv1, family=poisson))$coef[2,4]
#p1  <- wilcox.exact(cg,t2, paired = FALSE, exact = FALSE, alternative = "two.sided")$p.value
#p1  <- summary(glm.nb(dv1 ~ iv1))$coef[2,4]
p1   <- summary(glm(dv1 ~ iv1))$coef[2,4]

dv2 <- c(cg, t3)
iv2 <- c(rep(1,length(cg)), rep(0, length(t3))) 
#p2  <- summary(glm(dv2 ~ iv2, family=poisson))$coef[2,4]
#p2  <- wilcox.exact(cg,t3, paired = FALSE, exact = FALSE, alternative = "two.sided")$p.value
#p2  <- summary(glm.nb(dv2 ~ iv2))$coef[2,4]
p2   <- summary(glm(dv2 ~ iv2))$coef[2,4]

t2t3  <- t.test(cg, t2) 
t3cg  <- t.test(cg, t3)

p3  <- summary(glm(I(t2 - t3) ~ 1))$coef[1,4]
#p3  <- wilcox.exact(t2,t3, paired = TRUE, exact = FALSE, alternative = "two.sided")$p.value

resdf[1,] <- c("Overall", kros(mean(t2)), kros(mean(t3)), kros(mean(cg)), 
paste(kros(mean(t2) - mean(cg)), stars(p1), sep=""), 
paste(kros(mean(t3) - mean(cg)), stars(p2), sep=""), 
paste(kros(mean(t3 - t2, na.rm=T)), stars(p2), sep=""), length(t2), length(t3), length(cg))

t2 <- x[t2nona  & !is.na(nireland$cathprot) & nireland$cathprot==1]
t3 <- y[t3nona  & !is.na(nireland$cathprot) & nireland$cathprot==1]
cg <- y[nonac   & !is.na(nireland$cathprot) & nireland$cathprot==1]

t2t3 <- t.test(cg, t2)
t3cg <- t.test(cg, t3)

dv1 <- c(cg, t2)
iv1 <- c(rep(1,length(cg)), rep(0, length(t2))) 
#p1  <- summary(glm(dv1 ~ iv1, family=poisson, na.action="na.omit"))$coef[2,4]
#p1  <- wilcox.exact(cg,t2, paired = FALSE, alternative = "two.sided")$p.value
#p1  <- summary(glm.nb(dv1 ~ iv1))$coef[2,4]
p1   <- summary(glm(dv1 ~ iv1))$coef[2,4]

dv2 <- c(cg, t3)
iv2 <- c(rep(1,length(cg)), rep(0, length(t3))) 
#p2  <- summary(glm(dv2 ~ iv2, family=poisson, na.action="na.omit"))$coef[2,4]
#p2  <- wilcox.exact(cg,t3, paired = FALSE, alternative = "two.sided")$p.value
#p2  <- summary(glm.nb(dv2 ~ iv2))$coef[2,4]
p2   <- summary(glm(dv2 ~ iv2))$coef[2,4]

p3  <- summary(glm(I(t2 - t3) ~ 1))$coef[1,4]
#p3  <- wilcox.exact(t2,t3, paired = TRUE, alternative = "two.sided")$p.value

resdf[2,] <- c("Catholic", kros(t2t3$est[2]) , kros(t3cg$est[2]), kros(t3cg$est[1]), 
paste(kros(t2t3$est[2] - t2t3$est[1]), stars(p1), sep=""), 
paste(kros(t3cg$est[2] - t3cg$est[1]), stars(p2), sep=""), 
paste(kros(mean(t3 - t2, na.rm=T)), stars(p2), sep=""), length(t2), length(t3), length(cg))
t2 <- x[t2nona   & !is.na(nireland$cathprot) & nireland$cathprot==0]
t3 <- y[t3nona   & !is.na(nireland$cathprot) & nireland$cathprot==0]
cg <- y[nonac    & !is.na(nireland$cathprot) & nireland$cathprot==0]
t2t3 <- t.test(cg, t2)
t3cg <- t.test(cg, t3)
dv1 <- c(cg, t2)
iv1 <- c(rep(1,length(cg)), rep(0, length(t2))) 
#p1  <- summary(glm(dv1 ~ iv1, family=poisson, na.action="na.omit"))$coef[2,4]
#p1  <- wilcox.exact(cg,t2, paired = FALSE, alternative = "two.sided")$p.value
#p1  <- summary(glm.nb(dv1 ~ iv1))$coef[2,4]
p1   <- summary(glm(dv1 ~ iv1))$coef[2,4]

dv2 <- c(cg, t3)
iv2 <- c(rep(1,length(cg)), rep(0, length(t3))) 
#p2  <- summary(glm(dv2 ~ iv2, family=poisson, na.action="na.omit"))$coef[2,4]
#p2  <- wilcox.exact(cg,t3, paired = FALSE, alternative = "two.sided")$p.value
#p2  <- summary(glm.nb(dv2 ~ iv2))$coef[2,4]
p2   <- summary(glm(dv2 ~ iv2))$coef[2,4]

p3  <- summary(glm(I(t2 - t3) ~ 1))$coef[1,4]
#p3  <- wilcox.exact(t2,t3, paired = TRUE, alternative = "two.sided")$p.value

resdf[3,] <- c("Protestant", kros(t2t3$est[2]), kros(t3cg$est[2]), kros(t3cg$est[1]), 
paste(kros(t2t3$est[2] - t2t3$est[1]), stars(p1), sep=""), 
paste(kros(t3cg$est[2] - t3cg$est[1]), stars(p2), sep=""), 
paste(kros(mean(t3 - t2, na.rm=T)), stars(p2), sep=""), length(t2), length(t3), length(cg))
resdf
}

openq <- with(nireland, data.frame(t2q18anu, t3q18anu,t2q18bnu, t3q18bnu, t2q19anu, t3q19anu, t2q19bnu, t3q19bnu, 
t2q20anu, t3q20anu, t2q20bnu, t3q20bnu, t2q21anu, t3q21anu, t2q21bnu, t3q21bnu, t2tot, t3tot))

tab1 <- data.frame(sample=1:27, t2=NA, t3=NA, cg=NA, diff.t2.cg=NA, diff.t3.cg=NA, diff.t3.t2=NA, n.t2part=NA, n.t3part=NA, n.cg=NA)
j=1
for(i in 1:length(openq)){
if(i%%2==0) {
a  <- as.character(open_ended[i/2,1])
tab1[j,] <- c(a,"","","","","","","","","")
tab1[(j+1):(j+3),] <- openfunc(openq[,(i-1)], openq[,i])
j= j+4
}
}

write.csv(tab1, file="Ireland/res.open/tab1.mean.reasons.csv", row.names = TRUE)

# Docking for reasons that weren't good enough
#*************************************************
openq <- with(nireland, data.frame(t2.q18a.sum, t3.q18a.sum, t2.q18b.sum, t3.q18b.sum, t2.q19a.sum, t3.q19a.sum, 
t2.q19b.sum, t3.q19b.sum, t2.q20a.sum, t3.q20a.sum, t2.q20b.sum, t3.q20b.sum,
t2.q21a.sum, t3.q21a.sum, t2.q21b.sum, t3.q21b.sum, t2totr, t3totr))

tab2 <- data.frame(sample=1:27, t2=NA, t3=NA, cg=NA, diff.t2.cg=NA, diff.t3.cg=NA, diff.t3.t2=NA, n.t2part=NA, n.t3part=NA, ncg=NA)
j=1
for(i in 1:length(openq)){
if(i%%2==0) {
a <- as.character(open_ended[i/2,1])
tab2[j,] <- c(a,"","","","","","","","","")
tab2[(j+1):(j+3),] <- openfunc(openq[,(i-1)], openq[,i])
j= j+4
}
}

write.csv(tab2, file="Ireland/res.open/append.docked.reasons.csv", row.names = TRUE)

# Unique Reasons + Good Enough
#*************************************************

openq <- with(nireland, data.frame(t2.q18a.uniq, t3.q18a.uniq, t2.q18b.uniq, t3.q18b.uniq, t2.q19a.uniq, t3.q19a.uniq, 
t2.q19b.uniq, t3.q19b.uniq, t2.q20a.uniq, t3.q20a.uniq, t2.q20b.uniq, t3.q20b.uniq,
t2.q21a.uniq, t3.q21a.uniq, t2.q21b.uniq, t3.q21b.uniq, t2totr, t3totr))

tab3 <- data.frame(sample=1:27, t2=NA, t3=NA, cg=NA, diff.t2.cg=NA, diff.t3.cg=NA, n.t2part=NA, n.t3part=NA, ncg=NA)
j=1
for(i in 1:length(openq)){
if(i%%2==0) {
a <- as.character(open_ended[i/2,1])
tab3[j,] <- c(a,"","","","","","","","")
tab3[(j+1):(j+3),] <- openfunc(openq[,(i-1)], openq[,i])
j= j+4
}
}

cbind(tab1[,1:6], tab2[,2:6])
write.csv(tab1, file="Ireland/res.open/tab1.csv", row.names = TRUE)

##  
##   Appendix D: Learning of Arguments, All T2 Participants    ##
##   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    ##

openfunc <- function(x,y, subset="all") {
# tester: x <- nireland$t2tot; y <- nireland$t3tot
# tester2: x <- nireland$t2.q18a.sum; y <- nireland$t3.q18a.sum
resdf <- data.frame(sample=1:3, t2=NA, cg=NA, diff.t2.cg=NA, n.t2part=NA, n.cg=NA)
# Not missing at t2 or t3 # nireland$filter=='selected' & nireland$t3partgrp==1 & 
t2nona <-  !is.na(x) & !is.na(nireland$filter) & nireland$filter=='selected'
nonac  <-  !is.na(y) & nireland$control 

t2 <- x[t2nona]
cg <- y[nonac]

dv1 <- c(cg, t2)
iv1 <- c(rep(1,length(cg)), rep(0, length(t2))) 
#p1  <- summary(glm(dv1 ~ iv1, family=poisson))$coef[2,4]
#p1  <- wilcox.exact(cg,t2, paired = FALSE, exact = FALSE, alternative = "two.sided")$p.value
#p1  <- summary(glm.nb(dv1 ~ iv1))$coef[2,4]
p1   <- summary(glm(dv1 ~ iv1))$coef[2,4]

resdf[1,] <- c("Overall", kros(mean(t2)), kros(mean(cg)), 
paste(kros(mean(t2) - mean(cg)), stars(p1), sep=""), 
length(t2), length(cg))

t2 <- x[t2nona  & !is.na(nireland$cathprot) & nireland$cathprot==1]
cg <- y[nonac   & !is.na(nireland$cathprot) & nireland$cathprot==1]

dv1 <- c(cg, t2)
iv1 <- c(rep(1,length(cg)), rep(0, length(t2))) 
#p1  <- summary(glm(dv1 ~ iv1, family=poisson, na.action="na.omit"))$coef[2,4]
#p1  <- wilcox.exact(cg,t2, paired = FALSE, alternative = "two.sided")$p.value
#p1  <- summary(glm.nb(dv1 ~ iv1))$coef[2,4]
p1   <- summary(glm(dv1 ~ iv1))$coef[2,4]

resdf[2,] <- c("Catholic", kros(mean(t2)), kros(mean(cg)), 
paste(kros(mean(t2) - mean(cg)), stars(p1), sep=""), 
length(t2), length(cg))

t2 <- x[t2nona   & !is.na(nireland$cathprot) & nireland$cathprot==0]
cg <- y[nonac    & !is.na(nireland$cathprot) & nireland$cathprot==0]

dv1 <- c(cg, t2)
iv1 <- c(rep(1,length(cg)), rep(0, length(t2))) 
#p1  <- summary(glm(dv1 ~ iv1, family=poisson, na.action="na.omit"))$coef[2,4]
#p1  <- wilcox.exact(cg,t2, paired = FALSE, alternative = "two.sided")$p.value
#p1  <- summary(glm.nb(dv1 ~ iv1))$coef[2,4]
p1   <- summary(glm(dv1 ~ iv1))$coef[2,4]

resdf[3,] <- c("Protestant", kros(mean(t2)), kros(mean(cg)), 
paste(kros(mean(t2) - mean(cg)), stars(p1), sep=""), 
length(t2), length(cg))
resdf
}

openq <- with(nireland, data.frame(t2q18anu, t3q18anu,t2q18bnu, t3q18bnu, t2q19anu, t3q19anu, t2q19bnu, t3q19bnu, 
t2q20anu, t3q20anu, t2q20bnu, t3q20bnu, t2q21anu, t3q21anu, t2q21bnu, t3q21bnu, t2tot, t3tot))

tab2 <- data.frame(sample=1:27, t2=NA, cg=NA, diff.t2.cg=NA, n.t2part=NA, n.cg=NA)
j=1
for(i in 1:length(openq)){
if(i%%2==0) {
a  <- as.character(open_ended[i/2,1])
tab2[j,] <- c(a,"","","","","")
tab2[(j+1):(j+3),] <- openfunc(openq[,(i-1)], openq[,i])
j= j+4
}
}

write.csv(tab2, file="Ireland/res.open/append.d.t2cg.allp.csv", row.names = TRUE)

##                                                ##
##  Table 2: Learning of Arguments by Attitude    ##
##  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     ##

open3 <- function(q, t2open.s, t3open.s, t2open.o, t3open.o, t2att, t3att, sup) {
##tester: q <- "q18a"; t2open.s <- nireland$t2q18anu; t3open.s <- nireland$t3q18anu; t2open.o <- nireland$t2q18bnu; t3open.o <- nireland$t3q18bnu; t2att <- nireland$t2q1cdum; t3att <- nireland$t3q1cdum; sup <- "opp"; 
resdf <- data.frame(sample=1:4, t2=NA, t3=NA, cg=NA, diff.t2.cg=NA, diff.t3.cg=NA, n.t2part=NA, n.t3part=NA, n.ctrl=NA)

att <- 0
if(sup=="supp") att <- 1
# Not missing at t2 or t3
nona  <- !is.na(nireland$filter) & nireland$filter=='selected' #& nireland$t3partgrp==1
nonac <- nireland$control==1 
## First Row
n.part <- sum(nona)
n.ctrl <- sum(nonac)
resdf[1,] <- c(q, "","","","","","",n.part,n.ctrl)
# Supporters - Avg. Reasons in Support
t2 <- t2open.s[nona  & t2att==att] # use t1 att.
t3 <- t3open.s[nona  & t2att==att] # use t1 att.
cg <- t3open.s[nonac  & t3att==att]
t2t3 <- t.test(cg, t2)
t3cg <- t.test(cg, t3)
t2t3ans <- paste(kros(t2t3$est[2] - t2t3$est[1]), stars(t2t3$p.value), sep="")
t3cgans <- paste(kros(t3cg$est[2] - t3cg$est[1]), stars(t3cg$p.value), sep="")
if(length(t2) < 30){
t2t3.pval <- wilcox.test(cg, t2 = NULL, alternative = c("less"), mu = 0, paired = FALSE, exact = FALSE, correct = TRUE,
conf.int = FALSE, conf.level = 0.95)$p.value
t3cg.pval <- wilcox.test(cg, t2 = NULL, alternative = c("less"), mu = 0, paired = FALSE, exact = FALSE, correct = TRUE,
conf.int = FALSE, conf.level = 0.95)$p.value
t2t3ans <- paste(kros(t2t3$est[2] - t2t3$est[1]), stars(t2t3.pval), sep="")
t3cgans <- paste(kros(t3cg$est[2] - t3cg$est[1]), stars(t3cg.pval),sep="")
}
resdf[2,] <- c("Support", kros(t2t3$est[2]), kros(t3cg$est[2]), kros(t3cg$est[1]), t2t3ans, t3cgans, length(t2), length(t3), length(cg))
t2 <- t2open.o[nona  & t2att==att]
t3 <- t3open.o[nona  & t2att==att] # use t1 att.
cg <- t3open.o[nonac  & t3att==att]
t2t3 <- t.test(cg, t2)
t3cg <- t.test(cg, t3)
t2t3ans <- paste(kros(t2t3$est[2] - t2t3$est[1]), stars(t2t3$p.value), sep="")
t3cgans <- paste(kros(t3cg$est[2] - t3cg$est[1]), stars(t3cg$p.value), sep="")
if(length(t2) < 30){
t2t3.pval <- wilcox.test(cg, t2 = NULL, alternative = c("less"), mu = 0, paired = FALSE, exact = FALSE, correct = TRUE,
conf.int = FALSE, conf.level = 0.95)$p.value
t3cg.pval <- wilcox.test(cg, t2 = NULL, alternative = c("less"), mu = 0, paired = FALSE, exact = FALSE, correct = TRUE,
conf.int = FALSE, conf.level = 0.95)$p.value
t2t3ans <- paste(kros(t2t3$est[2] - t2t3$est[1]), stars(t2t3.pval), sep="")
t3cgans <- paste(kros(t3cg$est[2] - t3cg$est[1]), stars(t3cg.pval), sep="")
}
resdf[3,] <- c("Oppose", kros(t2t3$est[2]) , kros(t3cg$est[2]), kros(t3cg$est[1]), t2t3ans, t3cgans, length(t2), length(t3), length(cg))
t2 <- (t2open.s - t2open.o)[nona  & t2att==att]
t3 <- (t3open.s - t3open.o)[nona  & t2att==att]
cg <- (t3open.s - t3open.o)[nonac  & t3att==att]
t2t3 <- t.test(cg, t2)
t3cg <- t.test(cg, t3)
t2t3ans <- paste(kros(t2t3$est[2] - t2t3$est[1]), stars(t2t3$p.value), sep="")
t3cgans <- paste(kros(t3cg$est[2] - t3cg$est[1]), stars(t3cg$p.value), sep="")
if(length(t2) < 30){
t2t3.pval <- wilcox.test(cg, t2 = NULL, alternative = c("less"), mu = 0, paired = FALSE, exact = FALSE, correct = TRUE,
conf.int = FALSE, conf.level = 0.95)$p.value
t3cg.pval <- wilcox.test(cg, t2 = NULL, alternative = c("less"), mu = 0, paired = FALSE, exact = FALSE, correct = TRUE,
conf.int = FALSE, conf.level = 0.95)$p.value
t2t3ans <- paste(kros(t2t3$est[2] - t2t3$est[1]), stars(t2t3.pval), sep="")
t3cgans <- paste(kros(t3cg$est[2] - t3cg$est[1]), stars(t3cg.pval), sep="")
}
resdf[4,] <- c("Support - Oppose", kros(t2t3$est[2]) , kros(t3cg$est[2]), kros(t3cg$est[1]), t2t3ans, t3cgans, length(t2), length(t3), length(cg))
as.matrix(resdf)
}

j=1
tab3 <- data.frame(q=NA, t2=NA, t3=NA, cg=NA, diff.t2.cg=NA, diff.t3.cg=NA, n.t2part=NA, n.t3part=NA, n.ctrl=NA)
tab3[(j):(j <- j+3),] <- with(nireland, open3("q18",t2q18anu,  t3q18anu, t2q18bnu, t3q18bnu, t1q11cdum, t3q1cdum, "supp"))
tab3[(j+1):(j <- j+4),] <- with(nireland, open3("q18",t2q18anu,  t3q18anu, t2q18bnu, t3q18bnu, t1q11cdum, t3q1cdum, "opp"))
tab3[(j+1):(j <- j+4),] <- with(nireland, open3("q19",t2q19anu,  t3q19anu, t2q19bnu, t3q19bnu, t1q12adum, t3q2adum, "supp"))
tab3[(j+1):(j <- j+4),] <- with(nireland, open3("q19",t2q19anu,  t3q19anu, t2q19bnu, t3q19bnu, t1q12adum, t3q2adum, "opp"))
tab3[(j+1):(j <- j+4),] <- with(nireland, open3("q20",t2q20anu,  t3q20anu, t2q20bnu, t3q20bnu, t1q14cdum, t3q4cdum, "supp"))
tab3[(j+1):(j <- j+4),] <- with(nireland, open3("q20",t2q20anu,  t3q20anu, t2q20bnu, t3q20bnu, t1q14cdum, t3q4cdum, "opp"))
tab3[(j+1):(j <- j+4),] <- with(nireland, open3("q21a",t2q21anu, t3q21anu, t2q21bnu, t3q21bnu, t1q15gdum, t3q5gdum, "supp"))
tab3[(j+1):(j <- j+4),] <- with(nireland, open3("q21a",t2q21anu, t3q21anu, t2q21bnu, t3q21bnu, t1q15gdum, t3q5gdum, "opp"))

write.csv(tab3, file="Ireland/res.open/tab3.csv", row.names = TRUE)

## Table X                                    ##
## Self-Selection; Attrition;                 ##
## Differences Across CG and T2 and T3        ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
# T1 P-NP
summary(glm(I(attend=='participant') ~ t1know + female + ppeduc + cathprot, data=nireland, family="binomial"))
summary(glm(I(attend=='participant') ~ t1know + female + ppeduc + cathprot + t1netaffect, data=nireland, family="binomial"))
summary(glm(I(attend=='participant') ~ t1know + female + ppeduc + cathprot + t1favinout, data=nireland, family="binomial"))

# T2 to T3 Attrition 
summary(glm(I(t3partgrp*1) ~ t2know + female + ppeduc +cathprot, data=subset(nireland, !nireland$control  & attend=='participant'), family="binomial"))
summary(glm(I(t3partgrp*1) ~ t2know + female + ppeduc +cathprot  + t1netaffect, data=subset(nireland, !nireland$control  & attend=='participant'), family="binomial"))
summary(glm(I(t3partgrp*1) ~ t2know + female + ppeduc +cathprot  + t1favinout, data=subset(nireland, !nireland$control  & attend=='participant'), family="binomial"))

# New Var: T1 Know for Parts, T3 for CG
nireland$t2cgknow <- NA
nireland$t2cgknow[!is.na(nireland$controlpart) & nireland$controlpart==1] <- nireland$t1know[!is.na(nireland$controlpart) & nireland$controlpart==1]
nireland$t2cgknow[!is.na(nireland$controlpart) & nireland$controlpart==0] <- nireland$t3know[!is.na(nireland$controlpart) & nireland$controlpart==0]

# New Var: T1 Affect for Parts, T3 for CG
nireland$t2cgaff <- NA
nireland$t2cgaff[!is.na(nireland$controlpart) & nireland$controlpart==1] <- nireland$t1netaffect[!is.na(nireland$controlpart) & nireland$controlpart==1]
nireland$t2cgaff[!is.na(nireland$controlpart) & nireland$controlpart==0] <- nireland$t3netaffect[!is.na(nireland$controlpart) & nireland$controlpart==0]

nireland$t2cgfav <- NA
nireland$t2cgfav[!is.na(nireland$controlpart) & nireland$controlpart==1] <- nireland$t1favinout[!is.na(nireland$controlpart) & nireland$controlpart==1]
nireland$t2cgfav[!is.na(nireland$controlpart) & nireland$controlpart==0] <- nireland$t3favinout[!is.na(nireland$controlpart) & nireland$controlpart==0]

# T2 Vs. CG
# Controlpart: Control Group coded as 0
summary(glm(controlpart   ~ t2cgknow + female + ppeduc + cathprot, data=nireland, family="binomial"))
summary(glm(controlpart   ~ t2cgknow + female + ppeduc + cathprot + t2cgaff, data=nireland, family="binomial"))
summary(glm(controlpart   ~ t2cgknow + female + ppeduc + cathprot + t2cgfav, data=nireland, family="binomial"))

# T3 Vs. CG
summary(glm(I(t3controlpart==0) ~ t2cgknow + female + ppeduc + cathprot, data=nireland, family="binomial"))
summary(glm(I(t3controlpart==0) ~ t2cgknow + female + ppeduc + cathprot + t2cgfav, data=nireland, family="binomial"))

##  Treatment Spillover Issues
##  *******************************************
##  People who had read about DP
mean(nireland$cgq31[nireland$control==1]=='yes')
mean(nireland$cgq35c[nireland$control==1 & nireland$cgq31=='yes']!='nothing at all')
mean(nireland$cgq35c[nireland$control==1 & nireland$cgq31=='no']!='nothing at all')

# Exposure to media etc. predicted by treatment, controlling for education (one can add female, cathprot but no theory)
summary(lm(nireland$exp ~ as.numeric(nireland$ppeduc) + nireland$cgq31r))

##
##  Question Order
## ******************************************* ##

base <- c("18a", "18b", "19a", "19b", "20a", "20b", "21a", "21b")
t2qs <- paste0("t2.q", base, ".sum")
t3qs <- paste0("t3.q", base, ".sum")

# A little to worry. Most of it likely explained by question. See 18b for instance. 
colSums(nireland[, t2qs], na.rm=T)
colSums(nireland[nireland$t3controlpart==0, t3qs], na.rm=T)
colSums(nireland[nireland$t3controlpart==1, t3qs], na.rm=T)


## Estimating Impact of Order of Question
## ******************************************
library(reshape)
irelong <- reshape(data=ireland,  idvar="cserial", varying=list(t2qs, t3qs), v.names=c("t2", "t3"), direction="long")
#irelong <- reshape(data=ireland,  idvar="cserial", varying=list(c(t2qs, t3qs)), v.names=c("q"), direction="long")

library(lme4)
# Estimating order effects within t2
model1 <- glmer(yo~   time +(1|cserial), data=irelong[!irelong$control,], family=poisson(link=log), REML=TRUE)
# Estimating order effects across t3 treat and control
model2 <- glmer(yoma~ control*time +(1|cserial), data=irelong, family=poisson(link=log), REML=TRUE)

##
## Table X: Robust Estimation
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##

model3 <- glmer(yoma~ t3controlpart*time +t2cgknow + female + ppeduc +cathprot +(1|cserial), data=irelong, family=poisson(link=log), REML=TRUE)

## 
## Table X: Learning of Arguments within DP 
## ******************************************************************** ##

# Most of the growth was in support, use att means
# 
sum18 <- with(nireland, rowSums(cbind(t2q18anu, t2q18bnu), na.rm=T))
sum19 <- with(nireland, rowSums(cbind(t2q19anu, t2q19bnu), na.rm=T))
sum20 <- with(nireland, rowSums(cbind(t2q20anu, t2q20bnu), na.rm=T))
sum21 <- with(nireland, rowSums(cbind(t2q21anu, t2q21bnu), na.rm=T))


# Mean Knowledge of Arguments in a group
e1   <- grpmn(sum18, nireland$grp)   
e2   <- grpmn(sum19, nireland$grp)
e3   <- grpmn(sum20, nireland$grp)
e4   <- grpmn(sum21, nireland$grp)
e5   <- grpmn(rowMeans(cbind(sum18, sum19, sum20, sum21), na.rm=T), nireland$grp) 


# Analyses in the paper
library(lme4)
summary(glmer(t2tot ~ t1know + ppeduc +  readbrief + genvar + grppk + (1|grp), data=nireland, family=poisson(link = "log")))
summary(lm(readbrief ~ t1know + ppeduc +female, data=nireland))

# Not Clear

# Attitude
a <- grpmn(nireland$t1q11cre, nireland$grp)
b <- grpmn(nireland$t1q12are, nireland$grp)
c <- grpmn(nireland$t1q14cre, nireland$grp)
d <- grpmn(nireland$t1q15gre, nireland$grp)

# With Mean Knowledge
summary(glmer(sum18 ~ t1know + ppeduc + e + a + (1|grp), data=nireland, family=poisson(link = "log")))
summary(glmer(sum19 ~ t1know + ppeduc + e + b +  (1|grp), data=nireland, family=poisson(link = "log")))
summary(glmer(sum20 ~ t1know + ppeduc + e + c +(1|grp), data=nireland, family=poisson(link = "log")))
summary(glmer(sum21 ~ t1know + ppeduc + e + d + (1|grp), data=nireland, family="poisson"))

summary(glmer(t2q18anu ~ t2know + ppeduc + a + (1|grp), data=nireland, family="poisson"))
summary(glmer(t2q18bnu ~ a + (1|grp), data=nireland, family="poisson"))

summary(glmer(t2q19anu ~ t2know + ppeduc +b + (1|grp), data=nireland, family="poisson"))
summary(glmer(t2q19bnu ~ t2know + ppeduc +b + (1|grp), data=nireland, family="poisson"))

summary(glmer(t2q20anu ~ t2know + ppeduc +c + (1|grp), data=nireland, family="poisson"))
summary(glmer(t2q20bnu ~ t2know + ppeduc +c + (1|grp), data=nireland, family="poisson"))

summary(glmer(t2q21anu ~ t2know + ppeduc +d + (1|grp), data=nireland, family="poisson"))
summary(glmer(t2q21bnu ~ t2know + ppeduc +d + (1|grp), data=nireland, family="poisson"))

## Attitude Change Model: Attitude Change as a function of mean att. of the group
##*************************************************************************************
a <- grpmn(nireland$t1q11cre, nireland$grp)
b <- grpmn(nireland$t1q12are, nireland$grp)
c <- grpmn(nireland$t1q14cre, nireland$grp)
d <- grpmn(nireland$t1q15gre, nireland$grp)

summary(lmer(I(t2q1ar - t1q11cre) ~ a + (1|grp), data=nireland))
summary(lmer(I(t2q2ar - t1q12are) ~ b + (1|grp), data=nireland))
summary(lmer(I(t2q4cr - t1q14cre) ~ c + (1|grp), data=nireland))
summary(lmer(I(t2q5gr - t1q15gre) ~ d + (1|grp), data=nireland))

summary(lmer(I(t2know - t1know) ~ a + (1|grp), data=nireland))

summary(lmer(I(t2q1ar - t1q11cre) ~ t1q11cre + t2know + a + (1|grp), data=nireland))
summary(lmer(I(t2q2ar - t1q12are) ~ t1q12are + t2know + b + (1|grp), data=nireland))
summary(lmer(I(t2q4cr - t1q14cre) ~ t1q14cre + t2know + c + (1|grp), data=nireland))
summary(lmer(I(t2q5gr - t1q15gre) ~ t1q15gre + t2know + d + (1|grp), data=nireland))

# Now lets try to add control group 
ni.cg    <- nireland[(!is.na(nireland$cgq36)),]
ni.part  <- subset(nireland, (nireland$filter =='selected' & !is.na(nireland$filter))) 

dv <- c(nona(ni.cg$t3q18anu) + nona(ni.cg$t3q18bnu), nona(ni.part$t2q18anu) + nona(ni.part$t2q18bnu))
iv <- c(rep(0, nrow(ni.cg)), rep(1, nrow(ni.part)))
iv2 <- c(ni.cg$t3know, ni.part$t2know)
iv3 <- c(ni.cg$t3q1cr, ni.part$t2q1cr) 
iv4 <- c(ni.cg$female, ni.part$female)
summary(glm(dv ~iv*iv2 + iv3 + iv4, family="poisson"))

dv <- c(nona(ni.cg$t3q18bnu), nona(ni.part$t3q18bnu))
iv <- c(rep(0, nrow(ni.cg)), rep(1, nrow(ni.part)))
iv2 <- c(ni.cg$t3know, ni.part$t2know)
iv3 <- c(ni.cg$t3q1cr, ni.part$t2q1cr) 
iv4 <- c(ni.cg$female, ni.part$female)
summary(glm(dv ~iv*iv2 + iv3 + iv4, family="poisson"))


## 
## Table 7: Mediation
## ~~~~~~~~~~~~~~~~~~~~~~ ##

im <- function(x){
# To Test::   x <- "t3q2ar" 

nona <- !is.na(nireland$t3q2ar) & !is.na(nireland$cserial) & ((is.na(nireland$filter) | nireland$filter=='selected') | nireland$t3partgrp)

d = subset(nireland, nona)
x2 <- eval(parse(text=paste("d","$",x, sep="")))
assign("x2", x2, envir = .GlobalEnv)

regc <- summary(lm(x2 ~ I(control==0), data = d))

medceff <- partceff <- NA
model.m <- model.y <- out.1 <- NULL
d$sum <- rowSums(cbind(d$t3q19anu, d$t3q19bnu), na.rm=T)
d$treat <- as.numeric(d$control==0)
model.m <- lm(sum ~treat, data=d)
model.y <- lm(x2 ~ treat + sum, data=d)
out.1 <- mediation::mediate(model.m, model.y, sims = 1000, boot = FALSE, treat = "treat", mediator = "sum")
medceff <- kros(out.1[[1]]) # Mediation Effect
lwr <- out.1[[3]][1]
upr <- out.1[[3]][2]
if(abs(lwr) + abs(upr) == abs(lwr + upr)){medceff <- paste(medceff, "*", sep="") }
partceff <- kros(out.1$z1)
if(abs(out.1$z0.ci[1]) + abs(out.1$z0.ci[2]) == abs( out.1$z0.ci[1] +  out.1$z0.ci[2])){partceff <- paste(partceff, "*", sep="") }
c(medceff, partceff)
}
## Increase in Respect
mean(nireland$t2q2br - nireland$t1q12br, na.rm=T)
mean(nireland$t2q2cr - nireland$t1q12cr, na.rm=T)
rel2(t1q16bre, t2q6bre, t3q6bre, 0)
rel2(t1q16are, t2q6are, t3q6are, 0)

medi <- data.frame(var=NA, ds=NA, ds2=NA)
medi[1,] <- c("Likely to Vote", im("t2q1ar"))
medi[2,] <- c("non-monetary", im("t2q2ar"))
medi[3,] <- c("monetary",   im("t2q4cr")) # Monetary
medi[4,] <- c("Care",  im("t2q5gr"))

