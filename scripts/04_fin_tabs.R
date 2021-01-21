##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##   ##
##      Northern Ireland/Open/Final Tables
##Last Edited: 1.17.13   
##   Gaurav Sood##
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++

# Set Working dir.
setwd(githubdir)
setwd("ireland")

# Load libs
library(goji)
library(mediation)
library(coda)
library(lme4)
library(rstanarm)

# Sourcing Common Functions
source("scripts/00_func.R")
source("scripts/00_polr_func.R")

# Load data
load("data/nireland.rdata")
open_ended <- read.csv("data/open_ended/open_ended.csv")

# Some convenient subsets
# Restricted to Participants and CG
ireland <- subset(nireland, !is.na(nireland$cserial) & ((is.na(nireland$filter) | nireland$filter=='selected') | nireland$t3partgrp))

# Some shorthands for qs
base <- c("18a", "18b", "19a", "19b", "20a", "20b", "21a", "21b")
t2qs <- paste0("t2.q", base, ".sum")
t3qs <- paste0("t3.q", base, ".sum")

# Creating new vars where t2 and cg have same vars.
t2cg  <- paste0("t2cg", base)
for(i in 1:8) ireland[,t2cg[i]] <- ifelse(nona(ireland$t3controlpart)==1, ireland[,t3qs[i]], ireland[,t2qs[i]])

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

# Table 1

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

write.csv(tab1, file="res/open_ended/tab1.mean.reasons.csv", row.names = TRUE)

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

write.csv(tab2, file="res/open_ended/append.docked.reasons.csv", row.names = TRUE)

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
write.csv(tab3, file="res/open_ended/tab3.csv", row.names = TRUE)

##  
##   Appendix C: Learning of Arguments, All T2 Participants    ##
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

write.csv(tab2, file="res/open_ended/append.d.t2cg.allp.csv", row.names = TRUE)

##                                                                     ##
##  Table 4A: Selective Learning: Learning of Arguments by Attitude    ##
##  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     ##

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
cg <- t3open.s[nonac & t3att==att]
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
tab3[(j):(j <- j+3),] <- with(nireland, open3("q18",  t2q18anu,  t3q18anu, t2q18bnu, t3q18bnu, t1q11cdum, t3q1cdum, "supp"))
tab3[(j+1):(j <- j+4),] <- with(nireland, open3("q18",  t2q18anu,  t3q18anu, t2q18bnu, t3q18bnu, t1q11cdum, t3q1cdum, "opp"))
tab3[(j+1):(j <- j+4),] <- with(nireland, open3("q19",  t2q19anu,  t3q19anu, t2q19bnu, t3q19bnu, t1q12adum, t3q2adum, "supp"))
tab3[(j+1):(j <- j+4),] <- with(nireland, open3("q19",  t2q19anu,  t3q19anu, t2q19bnu, t3q19bnu, t1q12adum, t3q2adum, "opp"))
tab3[(j+1):(j <- j+4),] <- with(nireland, open3("q20",  t2q20anu,  t3q20anu, t2q20bnu, t3q20bnu, t1q14cdum, t3q4cdum, "supp"))
tab3[(j+1):(j <- j+4),] <- with(nireland, open3("q20",  t2q20anu,  t3q20anu, t2q20bnu, t3q20bnu, t1q14cdum, t3q4cdum, "opp"))
tab3[(j+1):(j <- j+4),] <- with(nireland, open3("q21a", t2q21anu,  t3q21anu, t2q21bnu, t3q21bnu, t1q15gdum, t3q5gdum, "supp"))
tab3[(j+1):(j <- j+4),] <- with(nireland, open3("q21a", t2q21anu,  t3q21anu, t2q21bnu, t3q21bnu, t1q15gdum, t3q5gdum, "opp"))

write.csv(tab3, file="res/open_ended/tab3.csv", row.names = TRUE)

##                                                                     ##
##  Table 4B: Selective Learning: Learning of Arguments by Attitude    ##
##  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     ##

# Conditions in the CG
summary(lm(I(t3q19bnu) ~ t1cgq12ar, data=nireland[nireland$controlpart==0,]))

# By 
pol18a <- lm(t2cgq18anu ~ controlpart*t1cgq11cr, data=nireland)
pol18b <- lm(t2cgq18bnu ~ controlpart*t1cgq11cr, data=nireland)

pol19a <- lm(t2cgq19anu ~ controlpart*t1cgq12ar, data=nireland)
pol19b <- lm(t2cgq19bnu ~ controlpart*t1cgq12ar, data=nireland)

pol20a <- lm(t2cgq20anu ~ controlpart*t1cgq14cr, data=nireland)
pol20b <- lm(t2cgq20bnu ~ controlpart*t1cgq14cr, data=nireland)

pol21a <- lm(t2cgq21anu ~ controlpart*t1cgq15gr, data=nireland)
pol21b <- lm(t2cgq21bnu ~ controlpart*t1cgq15gr, data=nireland)

# Run ordered versions
summary(polr(as.ordered(t2cgq18bnu) ~ controlpart*t1cgq11cr, data=nireland))
summary(polr(as.ordered(t2cgq19bnu) ~ controlpart*t1cgq12ar, data=nireland))
summary(polr(as.ordered(t2cgq20bnu) ~ controlpart*t1cgq14cr, data=nireland))
summary(polr(as.ordered(t2cgq21bnu) ~ controlpart*t1cgq15gr, data=nireland))

# Diff. Gain version
# T2 P (all) Vs. CG
pol18 <- lm(I(t2cgq18bnu - t2cgq18anu) ~ controlpart*t1cgq11cr, data=nireland)
pol19 <- lm(I(t2cgq19bnu - t2cgq19anu) ~ controlpart*t1cgq12ar, data=nireland)
pol20 <- lm(I(t2cgq20bnu - t2cgq20anu) ~ controlpart*t1cgq14cr, data=nireland)
pol21 <- lm(I(t2cgq21bnu - t2cgq21anu) ~ controlpart*t1cgq15gr, data=nireland)

# T2 P (who also put in T3) Vs. CG
pol18 <- lm(I(t2cgq18bnu - t2cgq18anu) ~ t3controlpart*t1cgq11cr, data=nireland)
pol19 <- lm(I(t2cgq19bnu - t2cgq19anu) ~ t3controlpart*t1cgq12ar, data=nireland)
pol20 <- lm(I(t2cgq20bnu - t2cgq20anu) ~ t3controlpart*t1cgq14cr, data=nireland)
pol21 <- lm(I(t2cgq21bnu - t2cgq21anu) ~ t3controlpart*t1cgq15gr, data=nireland)

# T3 P Vs. T3 CG
summary(lm(I(t3q18bnu - t3q18anu) ~ t3controlpart*t1cgq11cr, data=nireland))
summary(lm(I(t3q19bnu - t3q19anu) ~ t3controlpart*t1cgq12ar, data=nireland))
summary(lm(I(t3q20bnu - t3q20anu) ~ t3controlpart*t1cgq14cr, data=nireland))
summary(lm(I(t3q21bnu - t3q21anu) ~ t3controlpart*t1cgq15gr, data=nireland))

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

# New Var: T1 Know for Parts, T3 for CG - nireland$t2cgknow 

# New Var: T1 Affect for Parts, T3 for CG nireland$t2cgaff; nireland$t2cgfav 

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

##  Question Order Issues
##  *******************************************  
# A little to worry. Most of it likely explained by question. See 18b for instance. 
colSums(nireland[, t2qs], na.rm=T)
colSums(nireland[nireland$t3controlpart==0, t3qs], na.rm=T)
colSums(nireland[nireland$t3controlpart==1, t3qs], na.rm=T)

##  Clustering Issues
##  *******************************************
# Intra-class correlation by small group; Not much is being explained by the small group
library(lme4)
lmnull <- lmer(t2cg18a ~ 1 + (1|grp), data=ireland)
aov1  <- aov(t2cg18a ~ grp, ireland)
summary(aov1)

# Are errors autocorrelated; answer is nearly always yes :-); Here 20b and 21b are exceptions
q18a <- lm(t2cg18a ~ controlpart, data=ireland) 
q18b <- lm(t2cg18b ~ controlpart, data=ireland) 
q19a <- lm(t2cg19a ~ controlpart, data=ireland) 

library(lmtest)
dwtest(q18a, alternative="two.sided")


##  The right distribution (if modeling total #)
##  *******************************************
# The correct distribution for total number of reasons is likely not Poisson; overdispersion issues. Likely zip. Neg. Bin will with apt. theta will do. 
var(nireland$t2tot)
mean(nireland$t2tot)
barplot(table(nireland$t2tot))
barplot(table(nireland$t3tot))

##
## Table 4: Robust Estimation
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ##

# Hierarchical with random effects for item, and person
# Wide to Long
# Further subset of cols
iresub  <- subset(ireland, select=c(t2qs, t2cg, t3qs, "grp", "grppk", "t1know", "readbrief", "genvar", "grpsize", "control", "t2cgknow", "t3controlpart", "controlpart", "cserial", "female", "ppeduc", "cathprot"))

library(reshape)
irelong1 <- reshape(data=iresub,  idvar="cserial", varying=list(t2cg), v.names=c("t2"), direction="long")
irelong2 <- reshape(data=iresub,  idvar="cserial", varying=list(t3qs), v.names=c("t3"), direction="long")

#recast(iresub, measure.var=t2cg, time ~ variable)

# Comparing T2 P with T3 CG
t2mod1   <- lmer(t2 ~ controlpart + female + t2cgknow + ppeduc + cathprot + (1|cserial) + (1|time), data=irelong1)
t2mod2a  <- glmer(t2 ~ controlpart*time + female + t2cgknow + ppeduc + cathprot + (1|cserial) + (1|time), data=irelong1, family=poisson(link=log))
t2mod2b  <- glmer(t2tot ~ controlpart + female + t2cgknow + ppeduc + cathprot + (1|grp), data=nireland, family=poisson(link=log))
stan_lmer(t2tot ~ controlpart + female + t2cgknow + ppeduc + cathprot + (1|grp), data=nireland)

# TABLE 4 (part a)
t2mod3   <- lmer(t2 ~ controlpart*time + female + controlpart*t2cgknow + ppeduc + controlpart*cathprot + (1|cserial) + (1|time), data=irelong1)
confint(t2mod3)

t2mod4a  <- lmer(t2 ~ time + female + t2cgknow + ppeduc +cathprot + (1|cserial) + (1|time) + (1|grp), data=irelong1)
t2mod4b  <- lmer(t2 ~ time + female + t2cgknow + ppeduc +cathprot + (1|cserial) + (1|time), data=irelong1[is.na(irelong1$grp),])

confint(t2mod4a)
confint(t2mod4b)

# Comparing T3 P with T3 CG
t3mod1  <- lmer(t3 ~ I(t3controlpart==0) + female + t2cgknow + ppeduc +cathprot + (1|cserial) + (1|time), data=irelong2)
t3mod2  <- glmer(t3 ~ I(t3controlpart==0)*time + female + t2cgknow + ppeduc +cathprot + (1|cserial) + (1|time), data=irelong2, family=poisson(link=log))
rstanarm::stan_lmer(t3 ~ I(t3controlpart==0)*time + female + t2cgknow + ppeduc +cathprot + (1|cserial) + (1|time), data=irelong2)

# TABLE 4 (part b)
t3mod3  <- lmer(t3 ~ I(t3controlpart==0)*time + female + I(t3controlpart==0)*t2cgknow + I(t3controlpart==0)*ppeduc +I(t3controlpart==0)*cathprot + (1|cserial) + (1|time), data=subset(irelong2, !is.na(t3controlpart)))
confint(t3mod3)
confint(t3mod1)

## 
## Table 5: Learning of Arguments within DP 
## ******************************************************************** ##

# Poissson 
summary(glmer(t2tot  ~ female + t2cgknow + ppeduc + readbrief + genvar + grppk + cathprot + (1|grp), data=nireland, family=poisson(link = "log")))

# Quasi Poisson/Neg. Bin.
library(MASS)
library(nlme)
t2tot.nb <- glm.nb(t2tot ~ t2cgknow + ppeduc + genvar + grppk, data = nireland)
theta.md(nireland$t2tot[!is.na(nireland$t2cgknow) & !is.na(nireland$ppeduc)], fitted(t2tot.nb), dfr = df.residual(t2tot.nb))
# Theta range from .01 to 1.1 leaves findings unchanged; 1.1 to 1.5 (even beyond) consigns grppk to marginal insignificance (p = .11)
summary(glmmPQL(t2tot ~ female + t2cgknow + ppeduc + readbrief + genvar + grppk + cathprot, random = ~ 1 | grp,  family = negative.binomial(.11), data =nireland))

# Zero Inflated Poisson (without random effects for group) and comparing to Poisson
library(pscl)
# Same as above except grppk is non-significant
a <- zeroinfl(t2tot ~ female + t2cgknow + ppeduc + readbrief + genvar + grppk + cathprot | female + t2cgknow,  data =nireland)
summary(a)
# Same as above with grppk significant
b <- glm(t2tot ~ female + t2cgknow + ppeduc + readbrief + genvar + grppk + cathprot,  data =nireland, family="poisson")
summary(b)

# Which model is better: zip or poisson? zip
vuong(a,b)

# Zero Inflated Poisson with random effects for group (same as basically above); 
# Needs lots of its; results for grppk are unstable; ignore for now
#library(MCMCglmm)
## same as above with grppk significant
#c <- MCMCglmm(t2tot ~ t2cgknow + ppeduc + genvar + grppk, random=~grp, rcov=~us(trait):units, family="zipoisson", nitt=100000, burnin=7000, mev=NULL, data=nireland[!is.na(nireland$grp) & !is.na(nireland$ppeduc),])
#summary(c)

# TABLE 5 HERE
# *****************************
# Items nested within individuals
wthnmod <- summary(glmer(t2 ~ time + female + t2cgknow + ppeduc + readbrief + genvar + grppk + cathprot + (1|cserial) + (1|time) + (1|grp), data=irelong1[!is.na(irelong1$grp),]))
confint(wthnmod)
# Who reads the briefing materials
summary(lm(readbrief ~ t1know + ppeduc +female + age, data=nireland))
summary(lm(readbrief ~ t1know + as.factor(ppeduc) +female + zero1(age), data=nireland))

# Interpreting age
range(nireland$age, na.rm=T)
range(nireland$age[!is.na(nireland$grp)], na.rm=T)


## 
## Table 7: Mediation
## ~~~~~~~~~~~~~~~~~~~~~~ ##

# Did Att. Change on Relevant Policy Items?

# Compare T1 to T2
summary(lm(nireland$t2q1cr - nireland$t1q11cre ~ 1))
summary(lm(nireland$t2q2ar - nireland$t1q12are ~ 1))
summary(lm(nireland$t2q4cr - nireland$t1q14cre ~ 1)) 
summary(lm(nireland$t2q5gr - nireland$t1q15gre ~ 1))

# Compare T1 to T3
summary(lm(nireland$t3q1cr - nireland$t1q11cre ~ 1))
summary(lm(nireland$t3q2ar - nireland$t1q12are ~ 1)) # Sig.
summary(lm(nireland$t3q4cr - nireland$t1q14cre ~ 1)) 
summary(lm(nireland$t3q5gr - nireland$t1q15gre ~ 1))

# Compare T3 to T2
summary(lm(nireland$t3q1cr - nireland$t2q1cr ~ 1))
summary(lm(nireland$t3q2ar - nireland$t2q2ar ~ 1)) # Sig.
summary(lm(nireland$t3q4cr - nireland$t2q4cr ~ 1)) 
summary(lm(nireland$t3q5gr - nireland$t2q5gr ~ 1))

# Compare T3 attitude (P Vs. CG)
summary(lm(nireland$t3q1cr ~ nireland$controlpart))
summary(lm(nireland$t3q2ar ~ nireland$controlpart)) # Sig.
summary(lm(nireland$t3q4cr ~ nireland$controlpart))
summary(lm(nireland$t3q5gr ~ nireland$controlpart))

# Favorability of In/Out Group
summary(lm(I(nireland$t2favinout - nireland$t1favinout) ~ 1))
summary(lm(I(nireland$t3favinout - nireland$t1favinout) ~ 1))
summary(lm(I(nireland$t3favinout) ~ nireland$controlpart))    # Sig.
summary(lm(I(nireland$t2cgfavinout) ~ nireland$controlpart)) 

# Out Fav.
summary(lm(I(nireland$t2outfav - nireland$t1outfav) ~ 1)) 
summary(lm(I(nireland$t3outfav - nireland$t1outfav) ~ 1)) # Sig.
summary(lm(I(nireland$t3outfav) ~ nireland$controlpart))  

# In Traits
summary(lm(I(nireland$t2inopen - nireland$t1inopen) ~ 1)) # Sig.
summary(lm(I(nireland$t3inopen - nireland$t1inopen) ~ 1)) # Sig.
summary(lm(I(nireland$t3inopen) ~ nireland$controlpart))  # Sig.

summary(lm(I(nireland$t2intrust - nireland$t1intrust) ~ 1)) # Sig.
summary(lm(I(nireland$t3intrust - nireland$t1intrust) ~ 1)) # Sig.
summary(lm(I(nireland$t3intrust) ~ nireland$controlpart))  # Sig.

# Out Traits
summary(lm(I(nireland$t2outopen - nireland$t1outopen) ~ 1)) # Sig.
summary(lm(I(nireland$t3outopen - nireland$t1outopen) ~ 1)) # Sig.
summary(lm(I(nireland$t3outopen) ~ nireland$controlpart))  # Sig.
summary(lm(I(nireland$t2cgoutopen) ~ nireland$controlpart))  # p of .103

summary(lm(I(nireland$t2outtrust - nireland$t1outtrust) ~ 1)) # Sig.
summary(lm(I(nireland$t2outtrust - nireland$t1outtrust) ~ 1)) # Sig.
summary(lm(I(nireland$t3outtrust) ~ nireland$controlpart))  # Sig.
summary(lm(I(nireland$t2cgouttrust) ~ nireland$controlpart))  

# In n Out Traits
summary(lm(I(nireland$t2rsninout - nireland$t1rsninout) ~ 1))
summary(lm(I(nireland$t3rsninout - nireland$t1rsninout) ~ 1))
summary(lm(I(nireland$t3rsninout) ~ nireland$controlpart))    
summary(lm(I(nireland$t2cgrsninout) ~ nireland$controlpart))    



im <- function(x, args){
# To Test::   x <- "t3q2ar"; args <- "t3sum19"

# Initializing Few Vars
medceff <- partceff <- NA
model.m <- model.y <- out.1 <- NULL

# Subset without missing
if(grepl("t2", x)) {
d = nireland[!is.na(nireland[,x]),]
d$treat <- nona(d$controlpart)
}

if(grepl("t3", x)) {
d = nireland[!is.na(nireland[,x]),]
d$treat <- as.numeric(d$t3controlpart==0)
}

d$sum   <- d[,args]

model.m <- lm(d[,args] ~ treat, data=d)
model.y <- lm(d[,x]    ~ treat + sum, data=d)
out.1   <- mediation::mediate(model.m, model.y, sims = 1000, boot = FALSE, treat = "treat", mediator = "sum")

medceff <- kros(out.1[[1]]) # Mediation Effect
lwr <- out.1[[3]][1]
upr <- out.1[[3]][2]

# Stars
if(abs(lwr) + abs(upr) == abs(lwr + upr)){medceff <- paste(medceff, "*", sep="") }
partceff <- kros(out.1$z1)
if(abs(out.1$z0.ci[1]) + abs(out.1$z0.ci[2]) == abs( out.1$z0.ci[1] +  out.1$z0.ci[2])){partceff <- paste(partceff, "*", sep="") }

# Out
c(medceff, partceff)
}



# Learning of Arguments causes Attitude; Beliefs etc.

medi <- data.frame(var=NA, ds=NA, ds2=NA, ds3=NA, ds4=NA)
medi[1,] <- c("All Ability",     im("t2cgq1cr", "t2cgsum18"),   im("t3q1cr", "t3sum18"))
medi[2,] <- c("Sectarian School", im("t2cgq2ar", "t2cgsum19"),   im("t3q2ar", "t3sum19"))
medi[3,] <- c("School Partner",   im("t2cgq4cr", "t2cgsum20"),   im("t3q4cr", "t3sum20")) 
medi[4,] <- c("Consolidate",  im("t2cgq5gr", "t2cgsum21"),   im("t3q5gr", "t3sum21"))
medi[5,] <- c("Fav.",          im("t2cgfavinout",  "t2cgtot"),    im("t3favinout",  "t3tot"))
medi[6,] <- c("Trust",          im("t2cgtrstinout", "t2cgtot"),    im("t3trstinout", "t3tot"))
medi[7,] <- c("Open to Rsn",  im("t2cgrsninout",  "t2cgtot"),    im("t3rsninout",  "t3tot"))
medi[8,] <- c("Out Trust",      im("t2cgouttrust",  "t2cgtot"),    im("t3outtrust",  "t3tot"))
medi[9,] <- c("Out Rsn",      im("t2cgoutopen",   "t2cgtot"),    im("t3outopen",   "t3tot"))
medi[10,]<- c("Out Fav.",      im("t2cgoutfav",    "t2cgtot"),    im("t3outfav",    "t3tot"))


### Bias 
# Any reduction? - Not sig. but coef. for T3 in wrong dir. (t3controlpart is rev. coded)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# (T2)
with(nireland, summary(lm(t2cg18bias ~ controlpart)))
with(nireland, summary(lm(t2cg19bias ~ controlpart)))
with(nireland, summary(lm(t2cg20bias ~ controlpart)))
with(nireland, summary(lm(t2cg21bias ~ controlpart)))

# T3 (most p values are pretty close to the .1 mark)
with(nireland, summary(lm(t3q18bias ~ t3controlpart)))
with(nireland, summary(lm(t3q19bias ~ t3controlpart)))
with(nireland, summary(lm(t3q20bias ~ t3controlpart)))
with(nireland, summary(lm(t3q21bias ~ t3controlpart)))


# Bias and total argument relation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# In CG corr. is insig.
with(nireland[nireland$control==1,], summary(lm(t3q18bias ~ t3sum18)))
with(nireland[nireland$control==1,], summary(lm(t3q19bias ~ t3sum19)))
with(nireland[nireland$control==1,], summary(lm(t3q20bias ~ t3sum20)))
with(nireland[nireland$control==1,], summary(lm(t3q21bias ~ t3sum21)))

# In T3 part corr. is insig.
with(nireland[nireland$t3controlpart==0,], summary(lm(t3q18bias ~ t3sum18)))
with(nireland[nireland$t3controlpart==0,], summary(lm(t3q19bias ~ t3sum19)))
with(nireland[nireland$t3controlpart==0,], summary(lm(t3q20bias ~ t3sum20)))
with(nireland[nireland$t3controlpart==0,], summary(lm(t3q21bias ~ t3sum21)))

# In T2 part corr. is insig..
with(nireland[nireland$controlpart==1,], summary(lm(t2cg18bias ~ t2cgsum18)))
with(nireland[nireland$controlpart==1,], summary(lm(t2cg19bias ~ t2cgsum19)))
with(nireland[nireland$controlpart==1,], summary(lm(t2cg20bias ~ t2cgsum20)))
with(nireland[nireland$controlpart==1,], summary(lm(t2cg21bias ~ t2cgsum21)))

# Reduction? - None on T2 or T3
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
with(nireland, summary(lm(t2cg18bias ~ t2cgsum18*controlpart)))
with(nireland, summary(lm(t2cg19bias ~ t2cgsum19*controlpart)))
with(nireland, summary(lm(t2cg20bias ~ t2cgsum20*controlpart)))
with(nireland, summary(lm(t2cg21bias ~ t2cgsum21*controlpart)))

with(nireland, summary(lm(t3q18bias ~ t3sum18*t3controlpart)))
with(nireland, summary(lm(t3q19bias ~ t3sum19*t3controlpart)))
with(nireland, summary(lm(t3q20bias ~ t3sum20*t3controlpart)))
with(nireland, summary(lm(t3q21bias ~ t3sum21*t3controlpart)))


## MISC: Did Mean attitude position affect growth of arguments (or att. change) 
## ******************************************************************** ##

# Argument Learning Model
summary(glmer(t2q18anu ~ t1know + ppeduc + q11cgrp + t1q11cre +  (1|grp),  data=nireland, family=poisson(link = "log")))

# Attitude Change Model
summary(lmer(I(t2q1ar - t1q11cre) ~ q11cgrp + t1q11cre + (1|grp), data=nireland))
summary(lmer(I(t2q2ar - t1q12are) ~ q12agrp + (1|grp), data=nireland))
summary(lmer(I(t2q4cr - t1q14cre) ~ q14cgrp + (1|grp), data=nireland))
summary(lmer(I(t2q5gr - t1q15gre) ~ q15ggrp + (1|grp), data=nireland))

