######################################################
##   												##
##     Northern Ireland, Last Edited: 12.10.11 		##
##	   Final Tables									##
##   												##
######################################################

setwd(githubdir)
setwd("ireland")

# Load data
load("data/nireland.rdata")
items <- read.csv("data/n.ireland.indices.csv")

# Religion and Unionist/Nationalist

nireland$t1q33
nireland$t1q34
##***************************##
##  Table 1 Knowledge Gain   ##
##***************************##

# May want to use chi squared for dummies; And Non-parametric where cell sizes are small (suggested and rejected)

paired <- function(x,y)
{
	#tester: x <- t1q21ans=="correct"; y <- t2q11ans
	nona <- !is.na(x) & !is.na(y)
	n <- sum(nona)
	mu.x <- round(mean(x[nona]),3)
	mu.y <- round(mean(y[nona]),3)
	test <- t.test(y,x, paired=T)
	c(mu.x, mu.y, round(test$est,3), round(test$p.value,3), n)
}

#t1know, t2know, t3know 
#t1q21ans, t2q11ans What percentage of majority-Protestant or majority-Catholic schools in Northern Ireland have at least 10% of the other religion in their enrolment?
#t1q22ans, t2q12ans	By approximately what percentage has the number of children entering Omagh schools increased or decreased over the past five years
#t1q23ans, t2q13ans	The new entitlement framework requires that… 
#t1q24ans, t2q14ans	The new entitlement framework requires that… 
#t1q25ans, t2q15ans Which of the following is true of what pupils in Northern Ireland do after they leave school?
#t1q26ans, t2q16ans	Which of the following is true of current school funding?
#t1q27ans, t2q17ans	Which of the following is true of the employing authority in the schools?

tab1.know <- data.frame(qno=NA, t1=NA, t2=NA, diff=NA, p=NA, n=1:8)
tab1.know[1,] <- c("21 Maj. Prot.", with(nireland, paired(t1q21ans=="correct", t2q11ans)))
tab1.know[2,] <- c("22 Omagh",with(nireland, paired(t1q22ans=="correct", t2q12ans)))
tab1.know[3,] <- c("23 New Entit",with(nireland, paired(t1q23ans=="correct", t2q13ans)))
tab1.know[4,] <- c("24 New Entit",with(nireland, paired(t1q24ans=="correct", t2q14ans)))
tab1.know[5,] <- c("25 Leave Sch.",with(nireland, paired(t1q25ans=="correct", t2q15ans)))
tab1.know[6,] <- c("26 Funding",with(nireland, paired(t1q26ans=="correct", t2q16ans)))
tab1.know[7,] <- c("27 Auth.",with(nireland, paired(t1q27ans=="correct", t2q17ans)))
tab1.know[8,] <- c("Avg.",with(nireland, paired(t1know, t2know)))

write.csv(tab1.know, file="res/tab1.know.csv")

##***********************************##
##  Table 2 School Policy Attitudes  ##
##***********************************##

pa2r <- function(x,y)
{
	nona <- !is.na(x) & !is.na(y) #& !is.na(nireland$attend) 
	n <- sum(nona)
	mu.x <- round(mean(x[nona]),3)
	mu.y <- round(mean(y[nona]),3)
	test <- t.test(y,x, paired=T)
	test1 <- round(t.test(x[nona], mu=.5)$p.value ,3) # Diff. Than .5
	test2 <- round(t.test(y[nona], mu=.5)$p.value, 3)
	c(mu.x, test1, mu.y, test2, round(test$est,3), round(test$p.value,3), n)
}

tab2.att <- data.frame(qno=NA, t1=NA, t1p=NA, t2=NA, t2p=NA, diff=NA, p=NA, n=1:17)

# Age-Grouping
tab2.att[1,] <-  c("15d", with(nireland, pa2r(t1q15dre, t2q5d/10)))
tab2.att[2,] <-  c("15e", with(nireland, pa2r(t1q15ere, t2q5e/10)))
tab2.att[3,] <-  c("15f", with(nireland, pa2r(t1q15fre, t2q5f/10)))
tab2.att[4,] <-  c("15g", with(nireland, pa2r(t1q15gre, t2q5g/10)))

# School-Types
tab2.att[5,] <- c("11a", with(nireland, pa2r(t1q11are, t2q1a/10)))
tab2.att[6,] <-  c("11b", with(nireland, pa2r(t1q11bre, t2q1b/10)))
tab2.att[7,] <-  c("11c", with(nireland, pa2r(t1q11cre, t2q1c/10)))

# School Collaboration
tab2.att[8,] <-  c("11d", with(nireland, pa2r(t1q11dre, t2q1d/10)))
tab2.att[9,] <-  c("11e", with(nireland, pa2r(t1q11ere, t2q1e/10)))
tab2.att[10,] <-  c("11f", with(nireland, pa2r(t1q11fre, t2q1f/10)))
tab2.att[11,] <-  c("11g", with(nireland, pa2r(t1q11gre, t2q1gre)))

# Religious Mixing
tab2.att[12,] <-  c("15a", with(nireland, pa2r(t1q15are, t2q5are)))
tab2.att[13,] <-  c("14b", with(nireland, pa2r(t1q14bre, t2q4bre)))
tab2.att[14,] <-  c("14a", with(nireland, pa2r(t1q14are, t2q4are)))
tab2.att[15,] <-  c("14c", with(nireland, pa2r(t1q14cre, t2q4cre)))
tab2.att[16,] <-  c("15b", with(nireland, pa2r(t1q15bre, t2q5b/10)))
tab2.att[17,] <-  c("15c", with(nireland, pa2r(t1q15cre, t2q5c/10)))
tab2.att$seq <- 1:17
tab2.att <- merge(items, tab2.att, by="qno", all.x=F, sort=F)
# Sorting Isssues
tab2.att <- tab2.att[order(tab2.att$seq),]
write.csv(tab2.att, file="res/tab2.att.csv")

##************************************##
##  Table 3: Cathl. Versus Protestant ##
##************************************##

tab3 <- function(x,y)
{
	nona <- !is.na(x) & !is.na(y) & !is.na(nireland$cathprot)	
	n <- sum(nona)
	                               
	t1 <- t.test(x[nona] ~ nireland$cathprot[nona])
	t1.prot <- round(t1$est[1],3)
	t1.cath <- round(t1$est[2],3)
	t1.diff <- round(t1$est[1] - t1$est[2],3)
	t1.p  	<- round(t1$p.value,3)
	
	t2 		<- t.test(y[nona] ~ nireland$cathprot[nona])
	t2.prot <- round(t2$est[1],3)
	t2.cath <- round(t2$est[2],3)
	t2.diff <- round(t2$est[1] - t2$est[2],3)
	t2.p  	<- round(t2$p.value,3)
	
	t1t2.prot <- t.test(y[nireland$cathprot==0], x[nireland$cathprot==0], paired=T)
	t1t2.cath <- t.test(y[nireland$cathprot==1], x[nireland$cathprot==1], paired=T)
	
	t1t2.prot.est 	<- t1t2.prot$est
	t1t2.prot.pval 	<- t1t2.prot$p.value
	t1t2.cath.est 	<- t1t2.cath$est
	t1t2.cath.pval 	<- t1t2.cath$p.value
	
	t1t2 <- t.test((y-x)[nona & nireland$cathprot==0], (y - x)[nona & nireland$cathprot==1])
	round(c(t1.cath, t1.prot, t1.diff, t1.p,t2.cath, t2.prot, t2.diff, t2.p, 
			t1t2.cath.est, t1t2.cath.pval, t1t2.prot.est, t1t2.prot.pval, t1t2$est[1] - t1t2$est[2], t1t2$p.value, 
			abs(t2.diff) - abs(t1.diff)),3)
}

tab3.rel <- data.frame(qno=NA, t1.cath=NA, t1.prot=NA, t1.diff=NA, t1.p=NA,t2.cath=NA, t2.prot=NA, t2.diff=NA, t2.p=NA, 
		t1t2.cath.est=NA, t1t2.cath.pval=NA, t1t2.prot.est=NA, t1t2.prot.pval=NA, t1t2=NA, t1t2.pval=NA, abs=NA)

# Age-Grouping
tab3.rel[1,]  <-  c("15d", with(nireland, tab3(t1q15dre, t2q5d/10)))
tab3.rel[2,]  <-  c("15e", with(nireland, tab3(t1q15ere, t2q5e/10)))
tab3.rel[3,]  <-  c("15f", with(nireland, tab3(t1q15fre, t2q5f/10)))
tab3.rel[4,]  <-  c("15g", with(nireland, tab3(t1q15gre, t2q5g/10)))

# School-Types
tab3.rel[5,]  <- c("11a",  with(nireland, tab3(t1q11are, t2q1a/10)))
tab3.rel[6,]  <-  c("11b", with(nireland, tab3(t1q11bre, t2q1b/10)))
tab3.rel[7,]  <-  c("11c", with(nireland, tab3(t1q11cre, t2q1c/10)))

# School Collaboration
tab3.rel[8,]  <-  c("11d", with(nireland, tab3(t1q11dre, t2q1d/10)))
tab3.rel[9,]  <-  c("11e", with(nireland, tab3(t1q11ere, t2q1e/10)))
tab3.rel[10,] <-  c("11f", with(nireland, tab3(t1q11fre, t2q1f/10)))
tab3.rel[11,] <-  c("11g", with(nireland, tab3(t1q11gre, t2q1gre)))

# Religious Mixing
tab3.rel[12,] <-  c("15a", with(nireland, tab3(t1q15are, t2q5are)))
tab3.rel[13,] <-  c("14b", with(nireland, tab3(t1q14bre, t2q4bre)))
tab3.rel[14,] <-  c("14a", with(nireland, tab3(t1q14are, t2q4are)))
tab3.rel[15,] <-  c("14c", with(nireland, tab3(t1q14cre, t2q4cre)))
tab3.rel[16,] <-  c("15b", with(nireland, tab3(t1q15bre, t2q5b/10)))
tab3.rel[17,] <-  c("15c", with(nireland, tab3(t1q15cre, t2q5c/10)))
tab3.rel$seq <- 1:17
tab3.rel <- merge(items, tab3.rel, by="qno", all.x=F, sort=F)
# Sorting Isssues
tab3.rel <- tab3.rel[order(tab3.rel$seq),]
write.csv(tab3.rel, file="res/tab3.rel.csv")

##*************************************************##
##  Table 4: Inter-Community Beliefs and Attitudes ##
##*************************************************##

#C re P; #P re C; #(C re P) – (P re C)

rel <- function(t1,t2,grp)
{
	nona <- !is.na(t1) & !is.na(t2) & !is.na(nireland$cathprot) & nireland$cathprot==grp #Not missing at t1, t2, and grp=grp
	n <- sum(nona)
	mu.t1 <- round(mean(t1[nona]),3)
	mu.t2 <- round(mean(t2[nona]),3)
	test <- t.test(t2[nona],t1[nona], paired=T)
	t1.pval <- round(t.test(t1[nona], mu=.5)$p.value, 3)
	t2.pval <- round(t.test(t2[nona], mu=.5)$p.value, 3)
	c(mu.t1, t1.pval, mu.t2, t2.pval, round(test$est,3), round(test$p.value,3), n)
}

tab4a <- function(t1x, t2x, t1y, t2y){
    #tester: t1x <- nireland$t1q19bre; t2x <- nireland$t2q9bre; t1y <- nireland$t1q19are; t2y <- nireland$t2q9are
	nona.x <- !is.na(t1x) & !is.na(t2x) & !is.na(nireland$cathprot) & nireland$cathprot==0 #x not missing at t1 or t2, prot
	nona.y <- !is.na(t1y) & !is.na(t2y) & !is.na(nireland$cathprot) & nireland$cathprot==1 #y not missing at t1 or t2, cath
	dv.t1 <- c(t1x[nona.x], t1y[nona.y])
	dv.t2 <- c(t2x[nona.x], t2y[nona.y])
	dv.t2t1 <- c((t2x[nona.x] - t1x[nona.x]), (t2y[nona.y] - t1y[nona.y]))
	grp <- c(rep(1,sum(nona.x)),rep(0,sum(nona.y)))
	t1 <- t.test(dv.t1 ~ grp)
	t2 <- t.test(dv.t2 ~ grp)
	t2t1 <- t.test(dv.t2t1 ~ grp)
	round(c(t1$est[2] - t1$est[1], t1$p.value, t2$est[2] - t2$est[1], t2$p.value, t2t1$est[2] - t2t1$est[1], t2t1$p.value, sum(nona.x, nona.y)),3)
}

#29 18a		Openness of  most Protestants to reason
#30 18b		Openness of  most Catholics to reason
#31 19a		Trustworthiness of most Protestants
#32 19b		Trustworthiness of most Catholics
#24 16a		How favorably or unfavorably do you feel about Protestants 
#25 16b		How favorably or unfavorably do you feel about Catholics

# Opposing community ratings
nireland$t1outopen <- ifelse(nireland$cathprot==1, nireland$t1q18are, nireland$t1q18bre)
nireland$t2outopen <- ifelse(nireland$cathprot==1, nireland$t2q8are, nireland$t2q8bre)

nireland$t1inopen <- ifelse(nireland$cathprot==0, nireland$t1q18are, nireland$t1q18bre)
nireland$t2inopen <- ifelse(nireland$cathprot==0, nireland$t2q8are, nireland$t2q8bre)

nireland$t1outtrust <- ifelse(nireland$cathprot==1, nireland$t1q19are, nireland$t1q19bre)
nireland$t2outtrust <- ifelse(nireland$cathprot==1, nireland$t2q9are, nireland$t2q9bre)

nireland$t1intrust <- ifelse(nireland$cathprot==0, nireland$t1q19are, nireland$t1q19bre)
nireland$t2intrust <- ifelse(nireland$cathprot==0, nireland$t2q9are, nireland$t2q9bre)

mean(nireland$t2intrust - nireland$t1intrust, na.rm=T)
mean(nireland$t2outtrust - nireland$t1outtrust, na.rm=T)

# In community ratings
### Secondary Analyses *************###
#***********************************###

c  <- subset(nireland, !is.na(nireland$attend) & nireland$attend=='participant' & !is.na(nireland$cathprot))
c1 <- subset(nireland, !is.na(nireland$attend) & nireland$attend=='participant' & !is.na(nireland$cathprot) & is.na(nireland$t1q18bre))
c2 <- subset(nireland, !is.na(nireland$attend) & nireland$attend=='participant' & !is.na(nireland$cathprot) & is.na(nireland$t2q8bre))

# Assign midpoint to .5
a <- ifelse(is.na(c$t1q18bre), .5, c$t1q18bre)
b <- ifelse(is.na(c$t2q8bre), .5, c$t2q8bre)

# All data at t1, and t2
mean(c$t1q18bre[c$cathprot==0], na.rm=T)
mean(c$t2q8bre[c$cathprot==0], na.rm=T)

# Only missing at t2
mean(c2$t1q18bre[c$cathprot==0], na.rm=T)
mean(c2$t2q8bre[c$cathprot==0], na.rm=T)
# Only missing at t1
mean(c1$t1q18bre[c$cathprot==0], na.rm=T)
mean(c1$t2q8bre[c$cathprot==0], na.rm=T)
#***********************************###

tab4a.int <- data.frame(qno=NA, t1=NA, t1.pval=NA, t2=NA, t2.pval=NA, diff=NA, p=NA, n=1:18)

tab4a.int[1,] <-  c("18b", with(nireland, rel(t1q18bre, t2q8bre, 0)))
tab4a.int[2,] <-  c("18a", with(nireland, rel(t1q18are, t2q8are, 1)))
tab4a.int[3,] <-  c("18ab", with(nireland, tab4a(t1q18bre, t2q8bre,t1q18are, t2q8are)))

tab4a.int[4,] <-  c("19b", with(nireland, rel(t1q19bre, t2q9bre, 0)))
tab4a.int[5,] <-  c("19a", with(nireland, rel(t1q19are, t2q9are, 1)))
tab4a.int[6,] <-  c("19ab", with(nireland, tab4a(t1q19bre, t2q9bre,t1q19are, t2q9are)))

tab4a.int[7,] <-  c("16b", with(nireland, rel(t1q16bre, t2q6bre, 0)))
tab4a.int[8,] <-  c("16a", with(nireland, rel(t1q16are, t2q6are, 1)))
tab4a.int[9,] <-  c("16ab", with(nireland, tab4a(t1q16bre, t2q6bre, t1q16are, t2q6are)))

## Beliefs C re P, P re C etc.

tab4a.int[10,] <- c("2b", with(nireland, rel(t1q12cre, t2q2cr, 0)))
tab4a.int[11,] <- c("2a", with(nireland, rel(t1q12bre, t2q2br, 1)))
tab4a.int[12,] <-  c("2ab", with(nireland, tab4a(t1q12cre, t2q2cr, t1q12bre, t2q2br)))

tab4a.int[13,] <- c("3b", with(nireland, rel(t1q13cre, t2q3cre, 0)))
tab4a.int[14,] <- c("3a", with(nireland, rel(t1q13bre, t2q3bre, 1)))
tab4a.int[15,] <-  c("3ab", with(nireland, tab4a(t1q13cre,t2q3cre, t1q13bre, t2q3bre)))

tab4a.int[16,] <- c("7b", with(nireland, rel(t1q17cre, t2q7cre, 0)))
tab4a.int[17,] <- c("7a", with(nireland, rel(t1q17bre, t2q7bre, 1)))
tab4a.int[18,] <-  c("7ab", with(nireland, tab4a(t1q17cre, t2q7cre,t1q17bre, t2q7bre)))

## Difference Scores
##############################

tab4a.int[19,] <-  c("18b", with(nireland, rel(t1q18are - t1q18bre, t2q8are - t2q8bre, 0)))
tab4a.int[20,] <-  c("18a", with(nireland, rel(t1q18bre - t1q18are, t2q8bre - t2q8are, 1)))

tab4a.int[21,] <-  c("19b", with(nireland, rel(t1q19are - t1q19bre, t2q9are - t2q9bre, 0)))
tab4a.int[22,] <-  c("19a", with(nireland, rel(t1q19bre - t1q19are, t2q9bre - t2q9are, 1)))

tab4a.int[23,] <-  c("16b", with(nireland, rel(t1q16are - t1q16bre, t2q6are - t2q6bre, 0)))
tab4a.int[24,] <-  c("16a", with(nireland, rel(t1q16bre - t1q16are, t2q6bre - t2q6are, 1)))

## Beliefs C re P, P re C etc.
tab4a.int[25,] <- c("2b", with(nireland, rel(t1q12bre - t1q12cre, t2q2br - t2q2cr, 0)))
tab4a.int[26,] <- c("2b", with(nireland, rel(t1q12cre - t1q12bre, t2q2cr - t2q2br, 1)))

tab4a.int[27,] <- c("3b", with(nireland, rel(t1q13bre - t1q13cre, t2q3bre -t2q3cre, 0)))
tab4a.int[28,] <- c("3a", with(nireland, rel(t1q13cre - t1q13bre, t2q3cre -t2q3bre, 1)))

tab4a.int[29,] <- c("3b", with(nireland, rel(t1q17bre - t1q17cre, t2q7bre -t2q7cre, 0)))
tab4a.int[30,] <- c("3a", with(nireland, rel(t1q17cre - t1q17bre, t2q7cre -t2q7bre, 1)))

write.csv(tab4a.int, file="res/tab4a.int.csv")
#11 13a                        Own views Changes equally benefit children from both communities
#12 13b                     Protestants views Changes equally benefit children both communities
#13 13c                       Catholics views Changes equally benefit children both communities

#26 17a                              Own views Better relations only come about through mixing 
#27 17b                                      Protestants views Better relations through mixing 
#28 17c                                         Catholics views Better relations through mixing
#33 20a                               Mixed education promotes mutual respect and understanding

tab4b <- function(t1, t2)
{
 nona <- !is.na(t1) & !is.na(t2) & !is.na(nireland$cathprot)
 p.nona <- nona & nireland$cathprot==0
 c.nona <- nona & nireland$cathprot==1
 n <- sum(nona)
 all.t1 <- mean(t1[nona])
 all.t1.p <- t.test(t1[nona], mu=.5)$p.value
 all.t2 <- mean(t2[nona])
 all.t2.p <- t.test(t2[nona], mu=.5)$p.value
 
 p.t1 <- mean(t1[p.nona])
 p.t1.p <- t.test(t1[p.nona], mu=.5)$p.value
 p.t2 <- mean(t2[p.nona])
 p.t2.p <- t.test(t2[p.nona], mu=.5)$p.value
 
 c.t1 <- mean(t1[c.nona])
 c.t1.p <- t.test(t1[c.nona], mu=.5)$p.value
 c.t2 <- mean(t2[c.nona])	
 c.t2.p <- t.test(t2[c.nona], mu=.5)$p.value
 
 pc.t1.test <- t.test(t1[p.nona], t1[c.nona])
 pc.t1 <-  pc.t1.test$est[1] -  pc.t1.test$est[2]
 pc.t1.p <- pc.t1.test$p.value
 pc.t2.test <- t.test(t2[p.nona], t2[c.nona])
 pc.t2 <-  pc.t2.test$est[1] -  pc.t2.test$est[2]
 pc.t2.p <- pc.t2.test$p.value
 
 all.test <- t.test(t2, t1,paired=T)
 p.test  <- t.test(t2[p.nona], t1[p.nona], paired=T)
 c.test <- t.test(t2[c.nona], t1[c.nona],paired=T)
 pc.test <- t.test((t2 -t1)[p.nona], (t2 -t1)[c.nona])
 
 res <- data.frame(t1.mu=1:4, t1.p=NA, t2.mu=NA, t2.p=NA, t2t1.mu=NA, t2t1.p=NA, n=NA)
 res[1,] <- round(c(all.t1, all.t1.p, all.t2, all.t2.p, all.test$est, all.test$p.value,n),3)
 res[2,] <- round(c(p.t1, p.t1.p, p.t2, p.t2.p, p.test$est, p.test$p.value, sum(p.nona)),3)
 res[3,] <- round(c(c.t1, c.t1.p, c.t2, c.t2.p, c.test$est, c.test$p.value, sum(c.nona)),3)
 res[4,] <- round(c(pc.t1, pc.t1.p, pc.t2, pc.t2.p, pc.test$est[2] - pc.test$est[1], pc.test$p.value,n),3)
 res
}

tab4b.int <- data.frame(qno=NA, t1.mu=1:16, t1.p=NA, t2.mu=NA, t2.p=NA, t2t1.mu=NA, t2t1.p=NA, n=NA)
tab4b.int[1:4,1] <- "2a"
tab4b.int[1:4,2:8] <-  with(nireland, tab4b(t1q12are, t2q2are))
tab4b.int[5:8,1] <- "13a"
tab4b.int[5:8,2:8] <-  with(nireland, tab4b(t1q13are, t2q3are))
tab4b.int[9:12,1] <- "17a"
tab4b.int[9:12,2:8] <-  with(nireland, tab4b(t1q17are, t2q7are))
tab4b.int[13:16,1] <- "20a"
tab4b.int[13:16,2:8] <-  with(nireland, tab4b(t1q20are, t2q10are))

write.csv(tab4b.int, file="res/tab4b.int.csv")

##**********************************************##
##  Table 5: Control Group. Pol. Attitudes 		##
##**********************************************##

tab5 <- function(t1,t2,t3)
{
	nona <- !is.na(t1) & !is.na(t2) & !is.na(t3) & !is.na(nireland$controlpart)
	cg <- !is.na(t3) & !is.na(nireland$controlpart) & nireland$controlpart==0
	n <- sum(nona)
	mu.t1  <- round(mean(t1[nona]),3)
	mu.t2  <- round(mean(t2[nona]),3)
	mu.t3  <- round(mean(t3[nona]),3)
	t2t1  <- t.test(t2[nona],t1[nona], paired=T)
	t3t2  <- t.test(t3[nona],t2[nona], paired=T)
	mu.t3c <- t2t3c <- t3t3c.diff <- t3t3c.pval <- NA
	if(sum(t3[cg], na.rm=T)!=0)
	{ 
	mu.t3c <- round(mean(t3[cg]),3)
	t3t3c <- t.test(t3[nona], t3[cg])
	t3t3c.diff <-  round(t3t3c$est[2] - t3t3c$est[1],3)
	t3t3c.pval <- round(t3t3c$p.value,3)
	}
	t2t1.diff  <- round(t2t1$est,3)
	t2t1.pval  <- round(t2t1$p.value,3)
	t3t2.diff  <- round(t3t2$est,3)
	t3t2.pval  <- round(t3t2$p.value,3)
	
	c(mu.t1, mu.t2, mu.t3, mu.t3c, t2t1.diff, t2t1.pval, t3t2.diff, t3t2.pval, t3t3c.diff, t3t3c.pval, n)
}

tab5.ctrl <- data.frame(qno=NA, t1=NA, t2=NA, t3=NA, t3c=NA, t2t1=NA, t2t2.p=NA, t3t2=NA, t3t2.p=NA, t3t3c=NA, 
		t3t3c.p=NA, n=NA)

#t.test(nireland$t3q5d[!is.na(nireland$controlpart)] ~ nireland$controlpart[!is.na(nireland$controlpart)])
# Age-Grouping
tab5.ctrl[1,] <-  c("15d", with(nireland, tab5(t1q15dre, t2q5d/10, t3q5d/10)))
tab5.ctrl[2,] <-  c("15e", with(nireland, tab5(t1q15ere, t2q5e/10, t3q5e/10)))
tab5.ctrl[3,] <-  c("15f", with(nireland, tab5(t1q15fre, t2q5f/10, t3q5f/10)))
tab5.ctrl[4,] <-  c("15g", with(nireland, tab5(t1q15gre, t2q5g/10, t3q5g/10)))

# School-Types
tab5.ctrl[5,] <- c("11a", with(nireland,  tab5(t1q11are, t2q1a/10, t3q1a/10)))
tab5.ctrl[6,] <-  c("11b", with(nireland, tab5(t1q11bre, t2q1b/10, t3q1b/10)))
tab5.ctrl[7,] <-  c("11c", with(nireland, tab5(t1q11cre, t2q1c/10, t3q1c/10)))

# School Collaboration
tab5.ctrl[8,]  <-  c("11d", with(nireland,  tab5(t1q11dre, t2q1dre, t3q1dre)))
tab5.ctrl[9,]  <-  c("11e", with(nireland,  tab5(t1q11ere, t2q1ere, t3q1ere)))
tab5.ctrl[10,] <-  c("11f", with(nireland, tab5(t1q11fre, t2q1fre, t3q1fre)))
tab5.ctrl[11,] <-  c("11g", with(nireland, tab5(t1q11gre, t2q1gre, t3q1gre)))

# Religious Mixing
tab5.ctrl[12,] <-  c("15a", with(nireland, tab5(t1q15are, t2q5are, t3q5are)))
tab5.ctrl[13,] <-  c("14b",with(nireland,  tab5(t1q14bre, t2q4bre, t3q4bre)))
tab5.ctrl[14,] <-  c("14a", with(nireland, tab5(t1q14are, t2q4are, t3q4are)))
tab5.ctrl[15,] <-  c("14c", with(nireland, tab5(t1q14cre, t2q4cre, t3q4cr)))
tab5.ctrl[16,] <-  c("15b", with(nireland, tab5(t1q15bre, t2q5b/10, t3q5b/10)))
tab5.ctrl[17,] <-  c("15c", with(nireland, tab5(t1q15cre, t2q5c/10, t3q5c/10)))

#Knowledge
tab5.ctrl[18,] <- c("pk11", with(nireland, tab5(t1q21ans=="correct", t2q11ans, t3q11ans)))
tab5.ctrl[19,] <- c("pk12", with(nireland, tab5(t1q22ans=="correct", t2q12ans, t3q12ans)))
tab5.ctrl[20,] <- c("pk13", with(nireland, tab5(t1q23ans=="correct", t2q13ans, t3q13ans)))
tab5.ctrl[21,] <- c("pk14", with(nireland, tab5(t1q24ans=="correct", t2q14ans, t3q14ans)))
tab5.ctrl[22,] <- c("pk15", with(nireland, tab5(t1q25ans=="correct", t2q15ans, t3q15ans)))
tab5.ctrl[23,] <- c("pk16", with(nireland, tab5(t1q26ans=="correct", t2q16ans, t3q16ans)))
tab5.ctrl[24,] <- c("pk17", with(nireland, tab5(t1q27ans=="correct", t2q17ans, t3q17ans)))
tab5.ctrl[25,] <- c("know", with(nireland, tab5(t1know, t2know, t3know)))

#Beliefs about community
tab5.ctrl[26,] <- c("12a", with(nireland, tab5(t1q12are, t2q2are, t3q2are)))
tab5.ctrl[27,] <- c("13a", with(nireland, tab5(t1q13are, t2q3are, t3q3are)))
tab5.ctrl[28,] <-  c("17a", with(nireland, tab5(t1q17are, t2q7are, t3q7are)))
tab5.ctrl[29,] <-  c("17a", with(nireland, tab5(t1q20are, t2q10are, t3q10are)))
tab5.ctrl$seq <- 1:29
tab5.ctrl <- merge(items, tab5.ctrl, by="qno", all.x=F, all.y=T, sort=F)
# Sorting Isssues
tab5.ctrl<- tab5.ctrl[order(tab5.ctrl$seq),]
write.csv(tab5.ctrl, file="res/tab5.ctrl.csv")

rel2 <- function(t1,t2,t3,grp)
{
	# tester t1 <- nireland$t1q18bre; t2 <- nireland$t2q8bre; t3 <- nireland$t3q8bre; grp <- 0
	nona <- !is.na(t1) & !is.na(t2) & !is.na(t3) & !is.na(nireland$cathprot) & nireland$cathprot==grp #Not missing at t1, t2, and grp=grp
	cg <- !is.na(t3) & !is.na(nireland$controlpart) & nireland$controlpart==0 & !is.na(nireland$cathprot) & nireland$cathprot==grp
	
	n <- sum(nona)
	
	mu.t1  <- round(mean(t1[nona]),3)
	mu.t2  <- round(mean(t2[nona]),3)
	mu.t3  <- round(mean(t3[nona]),3)
	mu.t3c <- round(mean(t3[cg]),3)
	
	test1  <- t.test(t2[nona],t1[nona], paired=T)
	test2  <- t.test(t3[nona],t2[nona], paired=T)
	test3  <- t.test(t3[nona],t3[cg])
	t1.pval <- round(t.test(t1[nona], mu=.5)$p.value, 3)
	t2.pval <- round(t.test(t2[nona], mu=.5)$p.value, 3)
	t3.pval <- round(t.test(t3[nona], mu=.5)$p.value, 3)
	t3c.pval <- round(t.test(t3[cg], mu=.5)$p.value, 3)
	c(mu.t1, t1.pval, mu.t2, t2.pval, mu.t3, t3.pval, mu.t3c, t3c.pval, round(test1$est,3), round(test1$p.val,3), 
			round(test2$est,3), round(test2$p.val,3), round(test3$est[1] - test3$est[2],3), round(test3$p.val,3), n)
}

tab5re <- function(t1c, t1p, t2c, t2p, t3c, t3p){
	# tester 
	#t1c <- nireland$t1q18bre; t2c <- nireland$t2q8bre; t3c <- nireland$t3q8bre; 
	#t1p <- nireland$t1q18are; t2p <- nireland$t2q8are; t3p <- nireland$t3q8are
	nona.c <- !is.na(t1c) & !is.na(t2c) & !is.na(t3c) & !is.na(nireland$cathprot) & nireland$cathprot==0 #x not missing at t1 or t2, prot
	nona.p <- !is.na(t1p) & !is.na(t2p) & !is.na(t3p) & !is.na(nireland$cathprot) & nireland$cathprot==1 #y not missing at t1 or t2, cath
	
	cg.c <- !is.na(t3c) & !is.na(nireland$controlpart) & nireland$controlpart==0 & !is.na(nireland$cathprot) & nireland$cathprot==0
	cg.p <- !is.na(t3p) & !is.na(nireland$controlpart) & nireland$controlpart==0 & !is.na(nireland$cathprot) & nireland$cathprot==1
	
	dv.t1  <- c(t1c[nona.c], t1p[nona.p])
	dv.t2  <- c(t2c[nona.c], t2p[nona.p])
	dv.t3  <- c(t3c[nona.c], t3p[nona.p])
	dv.t3c <- c(t3c[cg.c],	 t3p[cg.p])
	
	dv.t2t1 <- c((t2c - t1c)[nona.c], (t2p - t1p)[nona.p])
	dv.t3t2 <- c((t3c - t2c)[nona.c], (t3p - t2p)[nona.p])
	#dv.t3t1 <- c((t3 - t1)[nona.c], (t3 - t1)[nona.p]) # Not Included
	dv.t3t3c <- c(dv.t3, dv.t3c)
	
	grp <- c(rep(1,sum(nona.c)), rep(0,sum(nona.p)))
	grpc <- c(rep(1,sum(cg.c)),  rep(0,sum(cg.p)))
	grp3c <- c(rep(1,sum(nona.c)),rep(0,sum(nona.p)), rep(1,sum(cg.c)),rep(0,sum(cg.p)))
	
	cp <- c(rep(1, length(dv.t3)), rep(0, length(dv.t3c))) 
	#p-c in t3 versus p-c in t3c
	
	t1 <- t.test(dv.t1 ~ grp)
	t2 <- t.test(dv.t2 ~ grp)
	t3cp <- t.test(dv.t3 ~ grp)
	t3c <- t.test(dv.t3c ~ grpc)
	
	t2t1 <- t.test(dv.t2t1 ~ grp)
	t3t2 <- t.test(dv.t3t2 ~ grp)
	#t3t1 <- t.test(dv.t3t1 ~ grp) (Not Included)
	
	ct <- cg.c | cg.p | nona.c | nona.p
	t3t3c <- summary(lm(dv.t3t3c ~ grp3c*cp))
	
	round(c(t1$est[2] - t1$est[1], t1$p.value, t2$est[2] - t2$est[1], t2$p.value, t3cp$est[2] - t3cp$est[1], t3cp$p.value, 
					t3c$est[2] - t3c$est[1], t3c$p.value, t2t1$est[2] - t2t1$est[1], t2t1$p.value, t3t2$est[2] - t3t2$est[1], 
					t3t2$p.value, t3t3c$coeff[4,1], t3t3c$coeff[4,4],
					sum(nona.c, nona.p)),3)
}

tab5a.rel <- data.frame(qno=NA, t1=NA, t1.p=NA, t2=NA, t2.p=NA, t3=NA, t3.p=NA, t3c=NA, t3c.p=NA, t2t1=NA, t2t1.p=NA, 
					   t3t2=NA, t3t2.p=NA, t3t3c=NA,t3t3c.p=NA, n=NA)
#Perceptions and feeling towards the other community
tab5a.rel[1,] <- c("18b", with(nireland, rel2(t1q18bre, t2q8bre, t3q8bre, 0))) # Protestant
tab5a.rel[2,] <- c("18a", with(nireland, rel2(t1q18are, t2q8are, t3q8are, 1)))
tab5a.rel[3,] <-  c("18ab", with(nireland, tab5re(t1q18bre, t1q18are, t2q8bre, t2q8are, t3q8bre, t3q8are)))

tab5a.rel[4,] <- c("19b", with(nireland, rel2(t1q19bre, t2q9bre, t3q9bre, 0)))
tab5a.rel[5,] <- c("19a", with(nireland, rel2(t1q19are, t2q9are, t3q9are, 1)))
tab5a.rel[6,] <-  c("19ab", with(nireland, tab5re(t1q19bre, t1q19are, t2q9bre, t2q9are, t3q9bre, t3q9are)))

tab5a.rel[7,] <- c("16b", with(nireland, rel2(t1q16bre, t2q6bre, t3q6bre, 0)))
tab5a.rel[8,] <- c("16a", with(nireland, rel2(t1q16are, t2q6are, t3q6are, 1)))
tab5a.rel[9,] <-  c("16ab", with(nireland, tab5re(t1q16bre,t1q16are, t2q6bre, t2q6are, t3q6bre, t3q6are)))

## Beliefs C re P, P re C etc.

tab5a.rel[10,] <- c("2b", with(nireland, rel2(t1q12cre, t2q2cr, t3q2c/10, 0)))
tab5a.rel[11,] <- c("2a", with(nireland, rel2(t1q12bre, t2q2br, t3q2b/10, 1)))
tab5a.rel[12,] <-  c("2ab", with(nireland, tab5re(t1q12cre,t1q12bre, t2q2cr, t2q2br, t3q2c/10, t3q2b/10)))

tab5a.rel[13,] <- c("3b", with(nireland, rel2(t1q13cre, t2q3cre, t3q3cre, 0)))
tab5a.rel[14,] <- c("3a", with(nireland, rel2(t1q13bre, t2q3bre, t3q3bre, 1)))
tab5a.rel[15,] <-  c("3ab", with(nireland, tab5re(t1q13cre,t1q13bre, t2q3cre, t2q3bre, t3q3cre, t3q3bre)))

tab5a.rel[16,] <- c("7b", with(nireland, rel2(t1q17cre, t2q7cre, t3q7cre, 0)))
tab5a.rel[17,] <- c("7a", with(nireland, rel2(t1q17bre, t2q7bre, t3q7bre, 1)))
tab5a.rel[18,] <-  c("7ab", with(nireland, tab5re(t1q17cre,t1q17bre, t2q7cre, t2q7bre, t3q7cre, t3q7bre)))

## Difference Scores
##############################

tab5a.rel[19,] <-  c("18b", with(nireland, rel2(t1q18are - t1q18bre, t2q8are - t2q8bre, t3q8are - t3q8bre, 0)))
tab5a.rel[20,] <-  c("18a", with(nireland, rel2(t1q18bre - t1q18are, t2q8bre - t2q8are, t3q8bre - t3q8are, 1)))

tab5a.rel[21,] <-  c("19b", with(nireland, rel2(t1q19are - t1q19bre, t2q9are - t2q9bre, t3q9are - t3q9bre, 0)))
tab5a.rel[22,] <-  c("19a", with(nireland, rel2(t1q19bre - t1q19are, t2q9bre - t2q9are, t3q9bre - t3q9are, 1)))

tab5a.rel[23,] <-  c("16b", with(nireland, rel2(t1q16are - t1q16bre, t2q6are - t2q6bre, t3q6are - t3q6bre, 0)))
tab5a.rel[24,] <-  c("16a", with(nireland, rel2(t1q16bre - t1q16are, t2q6bre - t2q6are, t3q6bre - t3q6are, 1)))

tab5a.rel[25,] <-  c("18b", with(nireland, rel2(t1q18are > t1q18bre, t2q8are > t2q8bre, t3q8are > t3q8bre, 0)))
tab5a.rel[26,] <-  c("18a", with(nireland, rel2(t1q18bre > t1q18are, t2q8bre > t2q8are, t3q8bre > t3q8are, 1)))

tab5a.rel[27,] <-  c("19b", with(nireland, rel2(t1q19are > t1q19bre, t2q9are > t2q9bre, t3q9are > t3q9bre, 0)))
tab5a.rel[28,] <-  c("19a", with(nireland, rel2(t1q19bre > t1q19are, t2q9bre > t2q9are, t3q9bre > t3q9are, 1)))

tab5a.rel[29,] <-  c("16b", with(nireland, rel2(t1q16are > t1q16bre, t2q6are > t2q6bre, t3q6are > t3q6bre, 0)))
tab5a.rel[30,] <-  c("16a", with(nireland, rel2(t1q16bre > t1q16are, t2q6bre > t2q6are, t3q6bre > t3q6are, 1)))

#tab5a.rel[19,] <- c("10b", with(nireland, rel2(t1q20cre, t2q10cre, t3q10cre, 0)))
#tab5a.rel[20,] <- c("10a", with(nireland, rel2(t1q20bre, t2q10bre, t3q10bre, 1)))
#tab5a.rel[21,] <-  c("10ab", with(nireland, tab5re(t1q20cre,t1q20bre, t2q10cre, t2q10bre, t3q10cre, t3q10bre)))

write.csv(tab5a.rel, file="res/tab5a.rel.csv")

tab5b <- function(t1, t2, t3)
{
	# tester: t1 <- nireland$t1q12are; t2 <- nireland$t2q2are; t3 <- nireland$t3q2are
	
	nona <- !is.na(t1) & !is.na(t2) & !is.na(t3) & !is.na(nireland$cathprot)
	cg   <- !is.na(t3) & !is.na(nireland$controlpart) & nireland$controlpart==0 & !is.na(nireland$cathprot)
	
	p.nona <- nona & nireland$cathprot==0
	c.nona <- nona & nireland$cathprot==1
	
	p.cg <- cg & nireland$cathprot==0
	c.cg <- cg & nireland$cathprot==1
	
	n <- sum(nona)
	
	all.t1 <- mean(t1[nona])
	all.t1.p <- t.test(t1[nona], mu=.5)$p.value
	all.t2 <- mean(t2[nona])
	all.t2.p <- t.test(t2[nona], mu=.5)$p.value
	all.t3 <- mean(t3[nona])
	all.t3.p <- t.test(t3[nona], mu=.5)$p.value
	
	p.t1 <- mean(t1[p.nona])
	p.t1.p <- t.test(t1[p.nona], mu=.5)$p.value
	p.t2 <- mean(t2[p.nona])
	p.t2.p <- t.test(t2[p.nona], mu=.5)$p.value
	p.t3 <- mean(t3[p.nona])
	p.t3.p <- t.test(t3[p.nona], mu=.5)$p.value
	
	c.t1 <- mean(t1[c.nona])
	c.t1.p <- t.test(t1[c.nona], mu=.5)$p.value
	c.t2 <- mean(t2[c.nona])	
	c.t2.p <- t.test(t2[c.nona], mu=.5)$p.value
	c.t3 <- mean(t3[c.nona])
	c.t3.p <- t.test(t3[c.nona], mu=.5)$p.value
	
	pc.t1.test <- t.test(t1[p.nona], t1[c.nona])
	pc.t1 <-  pc.t1.test$est[1] -  pc.t1.test$est[2]
	pc.t1.p <- pc.t1.test$p.value
	pc.t2.test <- t.test(t2[p.nona], t2[c.nona])
	pc.t2 <-  pc.t2.test$est[1] -  pc.t2.test$est[2]
	pc.t2.p <- pc.t2.test$p.value
	pc.t3.test <- t.test(t3[p.nona], t3[c.nona])
	pc.t3 <-  pc.t3.test$est[1] -  pc.t3.test$est[2]
	pc.t3.p <- pc.t3.test$p.value
	
	all.t3c <- all.t3c.p <- p.t3c <- p.t3c.p <- c.t3c <- c.t3c.p <- pc.t3c.test <- pc.t3c <- pc.t3c.p <- t3t3c.all.test <-
			t3t3c.p.test <- t3t3c.c.test <- NA
	t3t3c.all.est.p  <- t3t3c.p.est.p <-  t3t3c.c.est.p <- t3t3c.pc.est.p <- c(NA, NA)
	if(sum(p.cg)!=0){
	all.t3c <- mean(t3[cg])
	all.t3c.p <- t.test(t3[cg], mu=.5)$p.value
	p.t3c <- mean(t3[p.cg])
	p.t3c.p <- t.test(t3[p.cg], mu=.5)$p.value
	c.t3c <- mean(t3[c.cg])
	c.t3c.p <- t.test(t3[c.cg], mu=.5)$p.value
	pc.t3c.test <- t.test(t3[p.cg], t3[c.cg])
	pc.t3c <-  pc.t3c.test$est[1] -  pc.t3c.test$est[2]
	pc.t3c.p <- pc.t3c.test$p.value
	t3t3c.all.test <- t.test(t3[nona], t3[cg])
	t3t3c.p.test  <- t.test(t3[p.nona], t3[p.cg])
	t3t3c.c.test  <- t.test(t3[c.nona], t3[c.cg])
	ncg <- nona | cg
	t3t3c.pc.test <- summary(lm(t3[ncg] ~ nireland$controlpart[ncg]*nireland$cathprot[ncg])) 
	t3t3c.all.est.p <- c(t3t3c.all.test$est[2] -  t3t3c.all.test$est[1], t3t3c.all.test$p.value)
	t3t3c.p.est.p 	<- c(t3t3c.p.test$est[2] -  t3t3c.p.test$est[1], t3t3c.p.test$p.value)
	t3t3c.c.est.p 	<- c(t3t3c.c.test$est[2] -  t3t3c.c.test$est[1], t3t3c.c.test$p.value)
	t3t3c.pc.est.p 	<- c(t3t3c.pc.test$coeff[4,1], t3t3c.pc.test$coeff[4,4])
	}
	
	t2t1.all.test <- t.test(t2[nona], t1[nona], paired=T)
	t3t2.all.test <- t.test(t3[nona], t2[nona], paired=T)
	
	t2t1.p.test  <- t.test(t2[p.nona], t1[p.nona], paired=T)
	t3t2.p.test  <- t.test(t3[p.nona], t2[p.nona], paired=T)
	
	t2t1.c.test  <- t.test(t2[c.nona], t1[c.nona],paired=T)
	t3t2.c.test  <- t.test(t3[c.nona], t2[c.nona],paired=T)
	
	t2t1.pc.test <- t.test((t2 -t1)[p.nona], (t2 -t1)[c.nona])
	t3t2.pc.test <- t.test((t3 -t2)[p.nona], (t3 -t2)[c.nona])
	
	t3t3c.pc.test <- t.test((t3 -t1)[p.nona], (t3 -t1)[c.nona])
	
	res <- data.frame(t1.mu=1:4, t1.p=NA, t2.mu=NA, t2.p=NA, t3=NA, t3.p=NA, t3c=NA, t3c.p=NA, t2t1.mu=NA, t2t1.p=NA, 
					  t3t2=NA, t3t2.p=NA, t3t3c=NA, t3t3c.p=NA, n=NA)
	res[1,] <- round(c(all.t1, all.t1.p, all.t2, all.t2.p, all.t3, all.t3.p, all.t3c, all.t3c.p, 
					   t2t1.all.test$est, t2t1.all.test$p.value, t3t2.all.test$est, t3t2.all.test$p.value, 
					   t3t3c.all.est.p, n),3)
	res[2,] <- round(c(p.t1, p.t1.p, p.t2, p.t2.p, p.t3, p.t3.p, p.t3c, p.t3c.p, t2t1.p.test$est, t2t1.p.test$p.value, t3t2.p.test$est, t3t2.p.test$p.value, t3t3c.p.est.p, sum(p.nona)),3)
	res[3,] <- round(c(c.t1, c.t1.p, c.t2, c.t2.p, c.t3, c.t3.p, c.t3c, c.t3c.p, t2t1.c.test$est, t2t1.c.test$p.value, t3t2.c.test$est, t3t2.c.test$p.value, t3t3c.c.est.p, sum(c.nona)),3)
	res[4,] <- round(c(pc.t1, pc.t1.p, pc.t2, pc.t2.p, pc.t3, pc.t3.p, pc.t3c, pc.t3c.p, t2t1.pc.test$est[2] - t2t1.pc.test$est[1], t2t1.pc.test$p.value, t3t2.pc.test$est[2] - t3t2.pc.test$est[1], t3t2.pc.test$p.value, t3t3c.pc.est.p, n),3)
	res
}

tab5b.int <- data.frame(qno=NA, t1=NA, t1.p=NA, t2=NA, t2.p=NA, t3=NA, t3.p=NA, t3c=NA, t3c.p=NA, t2t1=NA, t2t1.p=NA, 
		t3t2=NA, t3t2.p=NA, t3t3c=NA,t3t3c.p=NA, n=NA)
#Beliefs about Inter-Community Relations
tab5b.int[1:4,1] <- "2a"
tab5b.int[1:4,2:16] <-  with(nireland, tab5b(t1q12are, t2q2are, t3q2are))
tab5b.int[5:8,1] <- "13a"
tab5b.int[5:8,2:16] <-  with(nireland, tab5b(t1q13are, t2q3are, t3q3are))
tab5b.int[9:12,1] <- "17a"
tab5b.int[9:12,2:16] <-  with(nireland, tab5b(t1q17are, t2q7are, t3q7are))
tab5b.int[13:16,1] <- "20a"
tab5b.int[13:16,2:16] <-  with(nireland, tab5b(t1q20are, t2q10are, t3q10are))

write.csv(tab5b.int, file="res/tab5b.int.csv")

#####################################$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
## Appendix A1 - NINIS, MORI, Accepters, Decliners, Parts, NP, P2, P3, CG ##
#####################################$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

app.a <- function(x)
{
	#tester: y <- 1; x <- nireland$t1q15dre
	nona <- !is.na(x) 
	
	a  <- !is.na(nireland$acceptndecliners) & nireland$acceptndecliners == 1 & nona 
	ns <- !is.na(nireland$noshow) & nireland$noshow ==1 & nona
	p  <- !is.na(nireland$attend) & nireland$attend =='participant' & nona
	np <- ((!is.na(nireland$acceptndecliners) & nireland$acceptndecliners == 0) | (!is.na(nireland$noshow) & nireland$noshow ==1)) & nona
	d <-  !is.na(nireland$attend) & nireland$attend =='non participant' & nona
	cg <- !is.na(nireland$controlpart) & nireland$controlpart==0 & nona
	p2 <- !is.na(nireland$attend) & nireland$attend =='participant' & nona
	p3 <- nireland$t3part & nona
	w  <- !is.na(nireland$whsample) & nireland$whsample & nona
	
	snp <- sum(np)
	
	mu.w  <- round(mean(x[w]),3)
	mu.a  <- round(mean(x[a]),3)
	mu.d  <- round(mean(x[d]),3)
	mu.ns <- round(mean(x[ns]),3)
	mu.p  <- round(mean(x[p]),3)
	mu.np <- round(mean(x[np]),3)
	mu.p3 <- round(mean(x[p3]),3)
	mu.cg <- round(mean(x[cg]),3)
	
	ad  <- t.test(x[a], x[d])
	pd  <- t.test(x[p], x[d])
	pd  <- t.test(x[p], x[d])
	pnp <- t.test(x[p], x[np])
	pcg <- t.test(x[p], x[cg])
	p3cg <- t.test(x[p3], x[cg])
	
	c(mu.w, mu.np, mu.d, mu.ns, mu.a, mu.p, mu.p3, mu.cg, "", round(ad$p.value,3), round(pd$p.value,3), round(pnp$p.value,3), round(pcg$p.value,3), round(p3cg$p.value,3), snp)
}

append.a <- data.frame(qno=NA, w=NA, np=NA, d=NA, ns=NA, a=NA, p=NA, p3=NA, cg=NA, nip=NA, ad.p=NA, pd.p=NA, pnp.p=NA, pcg=NA, p3cg=NA, np=NA)
append.a[1,] <- c("female", app.a(nireland$female))
append.a[2,] <- c("age", app.a(nireland$age))
# Religion
append.a[3,] <- c("catholic", app.a(nireland$cathprot==1))
append.a[4,] <- c("protestant", app.a(nireland$cathprot==0))
# Marital
append.a[5,] <- c("single", app.a(nireland$single==1))
append.a[6,] <- c("married", app.a(nireland$married==1))
append.a[7,] <- c("divorced", app.a(nireland$sdw==1))
# Work
append.a[8,]  <- c("fulltime", app.a(nireland$fulltime==1))
append.a[9,]  <- c("partime", app.a(nireland$partime==1))
append.a[10,] <- c("seekwork", app.a(nireland$seekwork==1))
append.a[11,] <- c("student", app.a(nireland$student==1))
append.a[12,] <- c("retired", app.a(nireland$retired==1))
append.a[13,] <- c("homemaker", app.a(nireland$homemaker==1))
append.a[14,] <- c("other", app.a(nireland$other==1))
# S Work
append.a[15,]  <- c("s fulltime", app.a(nireland$fulltimep==1))
append.a[16,]  <- c("s parttime", app.a(nireland$partimep==1))
append.a[17,]  <- c("s seekwork", app.a(nireland$seekworkp==1))
append.a[18,]  <- c("s student", app.a(nireland$studentp==1))
append.a[19,]  <- c("s retired", app.a(nireland$retiredp==1))
append.a[20,]  <- c("s homemaker", app.a(nireland$homemakerp==1))
append.a[21,]  <- c("s other", app.a(nireland$otherp==1))
# Education
append.a[22,]  <- c("degree", app.a(nireland$degree==1))
append.a[23,]  <- c("btechhigh", app.a(nireland$btechigh==1))
append.a[24,]  <- c("gce", app.a(nireland$gce==1))
append.a[25,]  <- c("btechnat", app.a(nireland$btechnat==1))
append.a[26,]  <- c("cse", app.a(nireland$gcse==1))
append.a[27,]  <- c("cse", app.a(nireland$cse==1))
append.a[28,]  <- c("no educ", app.a(nireland$noeduc==1))
append.a[29,]  <- c("other", app.a(nireland$othered==1))

write.csv(append.a, file="res/append.a.csv")


#####################################$$$$$$$$$$$$$$$$$
## Appendix A2 - Accepters, Decliners, Parts, and NP ##
#####################################$$$$$$$$$$$$$$$$$

app.a1 <- function(x, y=10)
{
	#tester: y <- 1; x <- nireland$t1q15dre
	nona <- !is.na(x) 
	if(y==1){nona <- !is.na(x) & !is.na(nireland$cathprot) & nireland$cathprot==y}
	if(y==0){nona <- !is.na(x) & !is.na(nireland$cathprot) & nireland$cathprot==y}
	
	a  <- !is.na(nireland$acceptndecliners) & nireland$acceptndecliners == 1 & nona 
    ns <- !is.na(nireland$noshow) & nireland$noshow ==1 & nona
	p  <- !is.na(nireland$attend) & nireland$attend =='participant' & nona
	np <- nona & ((!is.na(nireland$acceptndecliners) & nireland$acceptndecliners == 0) | (!is.na(nireland$noshow) & nireland$noshow ==1))
	d <- !is.na(nireland$attend) & nireland$attend =='non participant' & nona
	
	snp <- sum(np)
	
	mu.a  <- round(mean(x[a]),3)
	mu.d  <- round(mean(x[d]),3)
	mu.ns <- round(mean(x[ns]),3)
	mu.p  <- round(mean(x[p]),3)
	mu.np <- round(mean(x[np]),3)
	
	ad  <- t.test(x[a], x[d])
	pd  <- t.test(x[p], x[d])
	pd  <- t.test(x[p], x[d])
	pnp <- t.test(x[p], x[np])
	
	c(mu.np, mu.d, mu.ns, mu.a, mu.p,  round(ad$p.value,3), round(pd$p.value,3), round(pnp$p.value,3),snp)
}

append.a1 <- data.frame(qno=NA, np=NA, d=NA, ns=NA, a=NA, p=NA, ad.diff=NA, pd.diff=NA, pnp.diff=1:17, np=NA)

# Age-Grouping
append.a1[1,] <-  c("15d", with(nireland, app.a1(t1q15dre)))
append.a1[2,] <-  c("15e", with(nireland, app.a1(t1q15ere)))
append.a1[3,] <-  c("15f", with(nireland, app.a1(t1q15fre)))
append.a1[4,] <-  c("15g", with(nireland, app.a1(t1q15gre)))

# School-Types
append.a1[5,] <- c("11a", with(nireland, app.a1(t1q11are)))
append.a1[6,] <-  c("11b", with(nireland, app.a1(t1q11bre)))
append.a1[7,] <-  c("11c", with(nireland, app.a1(t1q11cre)))

# School Collaboration
append.a1[8,] <-  c("11d", with(nireland, app.a1(t1q11dre)))
append.a1[9,] <-  c("11e", with(nireland, app.a1(t1q11ere)))
append.a1[10,] <-  c("11f", with(nireland, app.a1(t1q11fre)))
append.a1[11,] <-  c("11g", with(nireland, app.a1(t1q11gre)))

# Religious Mixing
append.a1[12,] <-  c("15a", with(nireland, app.a1(t1q15are)))
append.a1[13,] <-  c("14b", with(nireland, app.a1(t1q14bre)))
append.a1[14,] <-  c("14a", with(nireland, app.a1(t1q14are)))
append.a1[15,] <-  c("14c", with(nireland, app.a1(t1q14cre)))
append.a1[16,] <-  c("15b", with(nireland, app.a1(t1q15bre)))
append.a1[17,] <-  c("15c", with(nireland, app.a1(t1q15cre)))
append.a1$seq <- 1:17
append.a1 <- merge(items, append.a1, by="qno", all.x=F, sort=F)
# Sorting Isssues
append.a1 <- append.a1[order(append.a1$seq),]

append.a1[,1] <- as.character(append.a1[,1]) 
append.a1[,2] <- as.character(append.a1[,2])

# A
append.a1[18,] <- c("18b",  "open to reason", with(nireland, app.a1(t1q18bre,0)), "18")
append.a1[19,] <- c("18a",  "open to reason", with(nireland, app.a1(t1q18are,1)), "18")
append.a1[20,] <- c("19b", "trustworthy", with(nireland, app.a1(t1q19bre,0)) ,"19")
append.a1[21,] <- c("19a", "trustworthy", with(nireland, app.a1(t1q19are,1)) ,"19")
append.a1[22,] <- c("16b", "favorable", with(nireland, app.a1(t1q16bre,0)), "20")
append.a1[23,] <- c("16a", "favorable", with(nireland, app.a1(t1q16are,1)), "20")

# B
append.a1[24,] <- c("2a",  "balanced enrollment", with(nireland, app.a1(t1q12are)), "18")
append.a1[25,] <- c("13a", "equal benefit", with(nireland, app.a1(t1q13are)) ,"19")
append.a1[26,] <- c("17a", "mixing", with(nireland, app.a1(t1q17are)), "20")
append.a1[27,] <- c("20a", "respect", with(nireland, app.a1(t1q20are)), "21")

# C
append.a1[28,] <- c("2c",  "balanced enrollment", with(nireland, app.a1(t1q12cre, 0)), "18")
append.a1[29,] <- c("2b",  "balanced enrollment", with(nireland, app.a1(t1q12bre, 1)), "18")
append.a1[30,] <- c("13c", "equal benefit", with(nireland, app.a1(t1q13cre, 0)) ,"19")
append.a1[31,] <- c("13b", "equal benefit", with(nireland, app.a1(t1q13bre, 1)) ,"19")
append.a1[32,] <- c("17c", "mixing", with(nireland, app.a1(t1q17cre, 0)), "20")
append.a1[33,] <- c("17b", "mixing", with(nireland, app.a1(t1q17bre, 1)), "20")

write.csv(append.a1, file="res/append.a1.csv")

#####################################
## Appendix B - Male Versus Female ##
#####################################

app.b <- function(x,y)
{
	#tester: x <- nireland$t1q15dre ; y <- nireland$t2q5d/10
	nona <- !is.na(x) & !is.na(y) #& !is.na(nireland$attend) 
	m <- nireland$female ==0 & nona 
	f <- nireland$female ==1 & nona
    
	nm <- sum(m)
	nf <- sum(f)
	mu.m <- round(mean(y[m] - x[m]),3)
	mu.f <- round(mean(y[f] - x[f]),3)
	test <- t.test(y[m] - x[m], y[f] - x[f])
	c(mu.m, mu.f, round(test$est[2] - test$est[1],3), round(test$p.value,3), nm, nf)
}

append.b <- data.frame(qno=NA, m=NA, f=NA, diff=NA, p=NA, nm=1:17, nf=NA)

# Age-Grouping
append.b[1,] <-  c("15d", with(nireland, app.b(t1q15dre, t2q5d/10)))
append.b[2,] <-  c("15e", with(nireland, app.b(t1q15ere, t2q5e/10)))
append.b[3,] <-  c("15f", with(nireland, app.b(t1q15fre, t2q5f/10)))
append.b[4,] <-  c("15g", with(nireland, app.b(t1q15gre, t2q5g/10)))

# School-Types
append.b[5,] <- c("11a", with(nireland, app.b(t1q11are, t2q1a/10)))
append.b[6,] <-  c("11b", with(nireland, app.b(t1q11bre, t2q1b/10)))
append.b[7,] <-  c("11c", with(nireland, app.b(t1q11cre, t2q1c/10)))

# School Collaboration
append.b[8,] <-  c("11d", with(nireland, app.b(t1q11dre, t2q1d/10)))
append.b[9,] <-  c("11e", with(nireland, app.b(t1q11ere, t2q1e/10)))
append.b[10,] <-  c("11f", with(nireland, app.b(t1q11fre, t2q1f/10)))
append.b[11,] <-  c("11g", with(nireland, app.b(t1q11gre, t2q1gre)))

# Religious Mixing
append.b[12,] <-  c("15a", with(nireland, app.b(t1q15are, t2q5are)))
append.b[13,] <-  c("14b", with(nireland, app.b(t1q14bre, t2q4bre)))
append.b[14,] <-  c("14a", with(nireland, app.b(t1q14are, t2q4are)))
append.b[15,] <-  c("14c", with(nireland, app.b(t1q14cre, t2q4cre)))
append.b[16,] <-  c("15b", with(nireland, app.b(t1q15bre, t2q5b/10)))
append.b[17,] <-  c("15c", with(nireland, app.b(t1q15cre, t2q5c/10)))
append.b$seq <- 1:17
append.b <- merge(items, append.b, by="qno", all.x=F, sort=F)
# Sorting Isssues
append.b <- append.b[order(append.b$seq),]
write.csv(append.b, file="res/append.b.csv")


# Appendix C: Impact of over-representation of women and educated
# ******************************************************************
# for each of the two characteristics, educ and gender, divide the groups into those small groups for which
# the proportion of women and DLQ or higher respectively is .8 or greater (and then all other groups. 
# We mean this separately for gender and for education.  Please then compute the mean attitude change 
# within each of those categories. And then the difference of the changes and the p value associated 
# with that difference based on a two tailed test. We need that for all 17 policy attitude items and for 
# the intercommunity attitude items. "

# Gender
nireland$pfemdum <- ifelse(nireland$pfem > .79, 1, 0)

# Recode Education 
# Taking out those who said other
nireland$t1q8r   <- ifelse(as.numeric(nireland$t1q8)==8, NA,  nireland$t1q8)
# Dummy by DLQ
nireland$peddum  <- ifelse((as.numeric(nireland$t1q8r) < 6), 1, 0)
# prop.table(xtabs( ~ nireland$peddum + nireland$grp),2)*100
# aggregate(list(nireland$peddum), list(nireland$grp), function(x) mean(x, na.rm=T))*100
# table(nireland$grp, round(grpfun(nireland$peddum, nireland$grp, "mean")*100,2))

nireland$peddumy  <- ifelse(grpfun(nireland$peddum, nireland$grp, "mean") > .79, 1, 0)

tab3 <- function(x,y, hilo)
{
	nona <- !is.na(x) & !is.na(y) & !is.na(hilo)	
	n <- sum(nona)
	
	t1t2.lo <- t.test(y[hilo==0], x[hilo==0], paired=T)
	t1t2.hi <- t.test(y[hilo==1], x[hilo==1], paired=T)
	
	t1t2.lo.est 	<- t1t2.lo$est
	t1t2.lo.pval	<- t1t2.lo$p.value
	t1t2.hi.est 	<- t1t2.hi$est
	t1t2.hi.pval	<- t1t2.hi$p.value
	
	t1t2 <- t.test((y - x)[nona & hilo==0], (y - x)[nona & hilo==1])
	round(c(t1t2.lo.est, t1t2.lo.pval,  t1t2.hi.est, t1t2.hi.pval, t1t2$est[1] - t1t2$est[2], t1t2$p.value),3)
}


append.c <- data.frame(qno=NA, lo=NA, plo=NA, hi=NA, phi=NA, diff=NA, p=NA)
# Age-Grouping
append.c[1,]  <-  c("15d", with(nireland, tab3(t1q15dre, t2q5d/10, pfemdum)))
append.c[2,]  <-  c("15e", with(nireland, tab3(t1q15ere, t2q5e/10, pfemdum)))
append.c[3,]  <-  c("15f", with(nireland, tab3(t1q15fre, t2q5f/10, pfemdum)))
append.c[4,]  <-  c("15g", with(nireland, tab3(t1q15gre, t2q5g/10, pfemdum)))

# School-Types
append.c[5,]  <-  c("11a", with(nireland,  tab3(t1q11are, t2q1a/10, pfemdum)))
append.c[6,]  <-  c("11b", with(nireland, tab3(t1q11bre, t2q1b/10,  pfemdum)))
append.c[7,]  <-  c("11c", with(nireland, tab3(t1q11cre, t2q1c/10,  pfemdum)))

# School Collaboration
append.c[8,]  <-  c("11d", with(nireland, tab3(t1q11dre, t2q1d/10, pfemdum)))
append.c[9,]  <-  c("11e", with(nireland, tab3(t1q11ere, t2q1e/10, pfemdum)))
append.c[10,] <-  c("11f", with(nireland, tab3(t1q11fre, t2q1f/10, pfemdum)))
append.c[11,] <-  c("11g", with(nireland, tab3(t1q11gre, t2q1gre,  pfemdum)))

# Religious Mixing
append.c[12,] <-  c("15a", with(nireland, tab3(t1q15are, t2q5are, pfemdum)))
append.c[13,] <-  c("14b", with(nireland, tab3(t1q14bre, t2q4bre, pfemdum)))
append.c[14,] <-  c("14a", with(nireland, tab3(t1q14are, t2q4are, pfemdum)))
append.c[15,] <-  c("14c", with(nireland, tab3(t1q14cre, t2q4cre, pfemdum)))
append.c[16,] <-  c("15b", with(nireland, tab3(t1q15bre, t2q5b/10, pfemdum)))
append.c[17,] <-  c("15c", with(nireland, tab3(t1q15cre, t2q5c/10, pfemdum)))
append.c$seq <- 1:17
append.c <- merge(items, append.c, by="qno", all.x=F, sort=F)

# Sorting Isssues
append.c <- append.c[order(append.c$seq),]
write.csv(append.c, file="res/append.c.fem.csv")

# How did Men change in Female Dominated Groups verus Male Dominated Groups
# ************************************************************************

tab3 <- function(x,y, hilo)
{
	nona <- !is.na(x) & !is.na(y) & !is.na(hilo) & !is.na(nireland$female)	
	
	t1t2.lo <- t.test(y[hilo==0 & nireland$female==0], x[hilo==0  & nireland$female==0], paired=T)
	t1t2.hi <- t.test(y[hilo==1 & nireland$female==0], x[hilo==1  & nireland$female==0], paired=T)
	
	t1t2.lo.est 	<- t1t2.lo$est
	t1t2.lo.pval	<- t1t2.lo$p.value
	t1t2.hi.est 	<- t1t2.hi$est
	t1t2.hi.pval	<- t1t2.hi$p.value
	
	t1t2 <- t.test((y-x)[nona & hilo==0   & nireland$female==0], (y - x)[nona & hilo==1   & nireland$female==0])
	round(c(t1t2.lo.est, t1t2.lo.pval,  t1t2.hi.est, t1t2.hi.pval, t1t2$est[1] - t1t2$est[2], t1t2$p.value),3)
}

append.d <- data.frame(qno=NA, lo=NA, plo=NA, hi=NA, phi=NA, diff=NA, p=NA)
# Age-Grouping
append.d[1,]  <-  c("15d", with(nireland, tab3(t1q15dre, t2q5d/10, pfemdum)))
append.d[2,]  <-  c("15e", with(nireland, tab3(t1q15ere, t2q5e/10, pfemdum)))
append.d[3,]  <-  c("15f", with(nireland, tab3(t1q15fre, t2q5f/10, pfemdum)))
append.d[4,]  <-  c("15g", with(nireland, tab3(t1q15gre, t2q5g/10, pfemdum)))

# School-Types
append.d[5,]  <-  c("11a", with(nireland,  tab3(t1q11are, t2q1a/10, pfemdum)))
append.d[6,]  <-  c("11b", with(nireland, tab3(t1q11bre, t2q1b/10,  pfemdum)))
append.d[7,]  <-  c("11c", with(nireland, tab3(t1q11cre, t2q1c/10,  pfemdum)))

# School Collaboration
append.d[8,]  <-  c("11d", with(nireland, tab3(t1q11dre, t2q1d/10, pfemdum)))
append.d[9,]  <-  c("11e", with(nireland, tab3(t1q11ere, t2q1e/10, pfemdum)))
append.d[10,] <-  c("11f", with(nireland, tab3(t1q11fre, t2q1f/10, pfemdum)))
append.d[11,] <-  c("11g", with(nireland, tab3(t1q11gre, t2q1gre,  pfemdum)))

# Religious Mixing
append.d[12,] <-  c("15a", with(nireland, tab3(t1q15are, t2q5are, pfemdum)))
append.d[13,] <-  c("14b", with(nireland, tab3(t1q14bre, t2q4bre, pfemdum)))
append.d[14,] <-  c("14a", with(nireland, tab3(t1q14are, t2q4are, pfemdum)))
append.d[15,] <-  c("14c", with(nireland, tab3(t1q14cre, t2q4cre, pfemdum)))
append.d[16,] <-  c("15b", with(nireland, tab3(t1q15bre, t2q5b/10, pfemdum)))
append.d[17,] <-  c("15c", with(nireland, tab3(t1q15cre, t2q5c/10, pfemdum)))
append.d$seq <- 1:17
append.d <- merge(items, append.d, by="qno", all.x=F, sort=F)

# Sorting Isssues
append.d <- append.d[order(append.d$seq),]
write.csv(append.d, file="res/append.d.csv")