##
##   														##
##     Northern Ireland Open-Ended, Last Edited: 4.28.11   	##
##   														##
##

# Set Working dir.
setwd(githubdir)
setwd("ireland")

##Get the data
open <- read.csv("data/open_ended/fin.csv", na.strings = c("","NA"))

#t2.q18.a1.ch, t2.q18.a1.la, t2.q18.a1.monty

fin <- function(ch, la, monty){
	temp <- as.character(ch)
	temp[!is.na(ch) & !is.na(la) & as.character(ch)!=as.character(la)] <- as.character(monty[!is.na(ch) & !is.na(la) & as.character(ch)!=as.character(la)])
	temp
}

# T2 #####

open$t2.q18.a1.fin <- with(open, fin(t2.q18.a1.ch, t2.q18.a1.la, t2.q18.a1.monty))
open$t2.q18.a2.fin <- with(open, fin(t2.q18.a2.ch, t2.q18.a2.la, t2.q18.a2.monty))
open$t2.q18.a3.fin <- with(open, fin(t2.q18.a3.ch, t2.q18.a3.la, t2.q18.a3.monty))
open$t2.q18.a4.fin <- with(open, fin(t2.q18.a4.ch, t2.q18.a4.la, t2.q18.a4.monty))
open$t2.q18.a5.fin <- with(open, fin(t2.q18.a5.ch, t2.q18.a5.la, t2.q18.a5.monty))

open$t2.q18.b1.fin <- with(open, fin(t2.q18.b1.ch, t2.q18.b1.la, t2.q18.b1.monty))
open$t2.q18.b2.fin <- with(open, fin(t2.q18.b2.ch, t2.q18.b2.la, t2.q18.b2.monty))
open$t2.q18.b3.fin <- with(open, fin(t2.q18.b3.ch, t2.q18.b3.la, t2.q18.b3.monty))
open$t2.q18.b4.fin <- with(open, fin(t2.q18.b4.ch, t2.q18.b4.la, t2.q18.b4.monty))
open$t2.q18.b5.fin <- with(open, fin(t2.q18.b5.ch, t2.q18.b5.la, t2.q18.b5.monty))

open$t2.q19.a1.fin <- with(open, fin(t2.q19.a1.ch, t2.q19.a1.la, t2.q19.a1.monty))
open$t2.q19.a2.fin <- with(open, fin(t2.q19.a2.ch, t2.q19.a2.la, t2.q19.a2.monty))
open$t2.q19.a3.fin <- with(open, fin(t2.q19.a3.ch, t2.q19.a3.la, t2.q19.a3.monty))
open$t2.q19.a4.fin <- with(open, fin(t2.q19.a4.ch, t2.q19.a4.la, t2.q19.a4.monty))
open$t2.q19.a5.fin <- with(open, fin(t2.q19.a5.ch, t2.q19.a5.la, t2.q19.a5.monty))

open$t2.q19.b1.fin <- with(open, fin(t2.q19.b1.ch, t2.q19.b1.la, t2.q19.b1.monty))
open$t2.q19.b2.fin <- with(open, fin(t2.q19.b2.ch, t2.q19.b2.la, t2.q19.b2.monty))
open$t2.q19.b3.fin <- with(open, fin(t2.q19.b3.ch, t2.q19.b3.la, t2.q19.b3.monty))
open$t2.q19.b4.fin <- with(open, fin(t2.q19.b4.ch, t2.q19.b4.la, t2.q19.b4.monty))
open$t2.q19.b5.fin <- with(open, fin(t2.q19.b5.ch, t2.q19.b5.la, t2.q19.b5.monty))

open$t2.q20.a1.fin <- with(open, fin(t2.q20.a1.ch, t2.q20.a1.la, t2.q20.a1.monty))
open$t2.q20.a2.fin <- with(open, fin(t2.q20.a2.ch, t2.q20.a2.la, t2.q20.a2.monty))
open$t2.q20.a3.fin <- with(open, fin(t2.q20.a3.ch, t2.q20.a3.la, t2.q20.a3.monty))
open$t2.q20.a4.fin <- with(open, fin(t2.q20.a4.ch, t2.q20.a4.la, t2.q20.a4.monty))
open$t2.q20.a5.fin <- with(open, fin(t2.q20.a5.ch, t2.q20.a5.la, t2.q20.a5.monty))

open$t2.q20.b1.fin <- with(open, fin(t2.q20.b1.ch, t2.q20.b1.la, t2.q20.b1.monty))
open$t2.q20.b2.fin <- with(open, fin(t2.q20.b2.ch, t2.q20.b2.la, t2.q20.b2.monty))
open$t2.q20.b3.fin <- with(open, fin(t2.q20.b3.ch, t2.q20.b3.la, t2.q20.b3.monty))
open$t2.q20.b4.fin <- with(open, fin(t2.q20.b4.ch, t2.q20.b4.la, t2.q20.b4.monty))
open$t2.q20.b5.fin <- with(open, fin(t2.q20.b5.ch, t2.q20.b5.la, t2.q20.b5.monty))

open$t2.q21.a1.fin <- with(open, fin(t2.q21.a1.ch, t2.q21.a1.la, t2.q21.a1.monty))
open$t2.q21.a2.fin <- with(open, fin(t2.q21.a2.ch, t2.q21.a2.la, t2.q21.a2.monty))
open$t2.q21.a3.fin <- with(open, fin(t2.q21.a3.ch, t2.q21.a3.la, t2.q21.a3.monty))
open$t2.q21.a4.fin <- with(open, fin(t2.q21.a4.ch, t2.q21.a4.la, t2.q21.a4.monty))
open$t2.q21.a5.fin <- with(open, fin(t2.q21.a5.ch, t2.q21.a5.la, t2.q21.a5.monty))

open$t2.q21.b1.fin <- with(open, fin(t2.q21.b1.ch, t2.q21.b1.la, t2.q21.b1.monty))
open$t2.q21.b2.fin <- with(open, fin(t2.q21.b2.ch, t2.q21.b2.la, t2.q21.b2.monty))
open$t2.q21.b3.fin <- with(open, fin(t2.q21.b3.ch, t2.q21.b3.la, t2.q21.b3.monty))
open$t2.q21.b4.fin <- with(open, fin(t2.q21.b4.ch, t2.q21.b4.la, t2.q21.b4.monty))
open$t2.q21.b5.fin <- with(open, fin(t2.q21.b5.ch, t2.q21.b5.la, t2.q21.b5.monty))

## T3 #######
#$$$$$$$$$$$$$

open$t3.q18.a1.fin <- with(open, fin(t3.q18.a1.ch, t3.q18.a1.la, t3.q18.a1.monty))
open$t3.q18.a2.fin <- with(open, fin(t3.q18.a2.ch, t3.q18.a2.la, t3.q18.a2.monty))
open$t3.q18.a3.fin <- with(open, fin(t3.q18.a3.ch, t3.q18.a3.la, t3.q18.a3.monty))
open$t3.q18.a4.fin <- with(open, fin(t3.q18.a4.ch, t3.q18.a4.la, t3.q18.a4.monty))
open$t3.q18.a5.fin <- with(open, fin(t3.q18.a5.ch, t3.q18.a5.la, t3.q18.a5.monty))

open$t3.q18.b1.fin <- with(open, fin(t3.q18.b1.ch, t3.q18.b1.la, t3.q18.b1.monty))
open$t3.q18.b2.fin <- with(open, fin(t3.q18.b2.ch, t3.q18.b2.la, t3.q18.b2.monty))
open$t3.q18.b3.fin <- with(open, fin(t3.q18.b3.ch, t3.q18.b3.la, t3.q18.b3.monty))
open$t3.q18.b4.fin <- with(open, fin(t3.q18.b4.ch, t3.q18.b4.la, t3.q18.b4.monty))
open$t3.q18.b5.fin <- with(open, fin(t3.q18.b5.ch, t3.q18.b5.la, t3.q18.b5.monty))

open$t3.q19.a1.fin <- with(open, fin(t3.q19.a1.ch, t3.q19.a1.la, t3.q19.a1.monty))
open$t3.q19.a2.fin <- with(open, fin(t3.q19.a2.ch, t3.q19.a2.la, t3.q19.a2.monty))
open$t3.q19.a3.fin <- with(open, fin(t3.q19.a3.ch, t3.q19.a3.la, t3.q19.a3.monty))
open$t3.q19.a4.fin <- with(open, fin(t3.q19.a4.ch, t3.q19.a4.la, t3.q19.a4.monty))
open$t3.q19.a5.fin <- with(open, fin(t3.q19.a5.ch, t3.q19.a5.la, t3.q19.a5.monty))

open$t3.q19.b1.fin <- with(open, fin(t3.q19.b1.ch, t3.q19.b1.la, t3.q19.b1.monty))
open$t3.q19.b2.fin <- with(open, fin(t3.q19.b2.ch, t3.q19.b2.la, t3.q19.b2.monty))
open$t3.q19.b3.fin <- with(open, fin(t3.q19.b3.ch, t3.q19.b3.la, t3.q19.b3.monty))
open$t3.q19.b4.fin <- with(open, fin(t3.q19.b4.ch, t3.q19.b4.la, t3.q19.b4.monty))
open$t3.q19.b5.fin <- with(open, fin(t3.q19.b5.ch, t3.q19.b5.la, t3.q19.b5.monty))

open$t3.q20.a1.fin <- with(open, fin(t3.q20.a1.ch, t3.q20.a1.la, t3.q20.a1.monty))
open$t3.q20.a2.fin <- with(open, fin(t3.q20.a2.ch, t3.q20.a2.la, t3.q20.a2.monty))
open$t3.q20.a3.fin <- with(open, fin(t3.q20.a3.ch, t3.q20.a3.la, t3.q20.a3.monty))
open$t3.q20.a4.fin <- with(open, fin(t3.q20.a4.ch, t3.q20.a4.la, t3.q20.a4.monty))
open$t3.q20.a5.fin <- with(open, fin(t3.q20.a5.ch, t3.q20.a5.la, t3.q20.a5.monty))

open$t3.q20.b1.fin <- with(open, fin(t3.q20.b1.ch, t3.q20.b1.la, t3.q20.b1.monty))
open$t3.q20.b2.fin <- with(open, fin(t3.q20.b2.ch, t3.q20.b2.la, t3.q20.b2.monty))
open$t3.q20.b3.fin <- with(open, fin(t3.q20.b3.ch, t3.q20.b3.la, t3.q20.b3.monty))
open$t3.q20.b4.fin <- with(open, fin(t3.q20.b4.ch, t3.q20.b4.la, t3.q20.b4.monty))
open$t3.q20.b5.fin <- with(open, fin(t3.q20.b5.ch, t3.q20.b5.la, t3.q20.b5.monty))

open$t3.q21.a1.fin <- with(open, fin(t3.q21.a1.ch, t3.q21.a1.la, t3.q21.a1.monty))
open$t3.q21.a2.fin <- with(open, fin(t3.q21.a2.ch, t3.q21.a2.la, t3.q21.a2.monty))
open$t3.q21.a3.fin <- with(open, fin(t3.q21.a3.ch, t3.q21.a3.la, t3.q21.a3.monty))
open$t3.q21.a4.fin <- with(open, fin(t3.q21.a4.ch, t3.q21.a4.la, t3.q21.a4.monty))
open$t3.q21.a5.fin <- with(open, fin(t3.q21.a5.ch, t3.q21.a5.la, t3.q21.a5.monty))

open$t3.q21.b1.fin <- with(open, fin(t3.q21.b1.ch, t3.q21.b1.la, t3.q21.b1.monty))
open$t3.q21.b2.fin <- with(open, fin(t3.q21.b2.ch, t3.q21.b2.la, t3.q21.b2.monty))
open$t3.q21.b3.fin <- with(open, fin(t3.q21.b3.ch, t3.q21.b3.la, t3.q21.b3.monty))
open$t3.q21.b4.fin <- with(open, fin(t3.q21.b4.ch, t3.q21.b4.la, t3.q21.b4.monty))
open$t3.q21.b5.fin <- with(open, fin(t3.q21.b5.ch, t3.q21.b5.la, t3.q21.b5.monty))

##********************************************
# Summing up the scores across each question
##############################################


na90 <- function(data){
	temp <- matrix(ncol=ncol(data), nrow=nrow(data))
	for(i in 1:ncol(data)){
	hold <- as.integer(gsub("c", "", gsub(", ", "", gsub(",","", data[,i]))))
	temp[,i] <- as.integer(hold < 91 | hold ==94)
	}
	temp
}

# 90 + Reasons that give too little credit; 18b, 19a, 20b,
na902 <- function(data){
	temp <- matrix(ncol=ncol(data), nrow=nrow(data))
	for(i in 1:ncol(data)){
		hold <- as.integer(gsub("c", "", gsub(", ", "", gsub(",","", data[,i]))))
		temp[,i] <- as.integer(hold!=3 & (hold < 91 | hold ==94))
	}
	temp
}

uniq <- function(data){
	# data <- with(open, data.frame(t2.q18.a1.fin, t2.q18.a2.fin, t2.q18.a3.fin, t2.q18.a4.fin, t2.q18.a5.fin))
	temp <- matrix(ncol=ncol(data), nrow=nrow(data))
	for(i in 1:ncol(data)){
		temp[,i] <- as.integer(gsub("c", "", gsub(", ", "", gsub(",","", data[,i]))))
	}	
	res <- list()
	for(i in 1:nrow(temp))
	{
		res[[i]] <- temp[i,][!(temp[i,] %in%  c(NA,91,92,93))]
	}
	unlist(lapply(lapply(res, unique), length))
}

open$t2.q18a.sum <- with(open, rowSums(na90(data.frame(t2.q18.a1.fin, t2.q18.a2.fin, t2.q18.a3.fin, t2.q18.a4.fin, t2.q18.a5.fin)), na.rm=T))
open$t2.q18b.sum <- with(open, rowSums(na902(data.frame(t2.q18.b1.fin, t2.q18.b2.fin, t2.q18.b3.fin, t2.q18.b4.fin, t2.q18.b5.fin)), na.rm=T))
open$t2.q19a.sum <- with(open, rowSums(na902(data.frame(t2.q19.a1.fin, t2.q19.a2.fin, t2.q19.a3.fin, t2.q19.a4.fin, t2.q19.a5.fin)), na.rm=T))
open$t2.q19b.sum <- with(open, rowSums(na90(data.frame(t2.q19.b1.fin, t2.q19.b2.fin, t2.q19.b3.fin, t2.q19.b4.fin, t2.q19.b5.fin)), na.rm=T))
open$t2.q20a.sum <- with(open, rowSums(na90(data.frame(t2.q20.a1.fin, t2.q20.a2.fin, t2.q20.a3.fin, t2.q20.a4.fin, t2.q20.a5.fin)), na.rm=T))
open$t2.q20b.sum <- with(open, rowSums(na902(data.frame(t2.q20.b1.fin, t2.q20.b2.fin, t2.q20.b3.fin, t2.q20.b4.fin, t2.q20.b5.fin)), na.rm=T))
open$t2.q21a.sum <- with(open, rowSums(na90(data.frame(t2.q21.a1.fin, t2.q21.a2.fin, t2.q21.a3.fin, t2.q21.a4.fin, t2.q21.a5.fin)), na.rm=T))
open$t2.q21b.sum <- with(open, rowSums(na90(data.frame(t2.q21.b1.fin, t2.q21.b2.fin, t2.q21.b3.fin, t2.q21.b4.fin, t2.q21.b5.fin)), na.rm=T))

open$t3.q18a.sum <- with(open, rowSums(na90(data.frame(t3.q18.a1.fin, t3.q18.a2.fin, t3.q18.a3.fin, t3.q18.a4.fin, t3.q18.a5.fin)), na.rm=T))
open$t3.q18b.sum <- with(open, rowSums(na902(data.frame(t3.q18.b1.fin, t3.q18.b2.fin, t3.q18.b3.fin, t3.q18.b4.fin, t3.q18.b5.fin)), na.rm=T))
open$t3.q19a.sum <- with(open, rowSums(na902(data.frame(t3.q19.a1.fin, t3.q19.a2.fin, t3.q19.a3.fin, t3.q19.a4.fin, t3.q19.a5.fin)), na.rm=T))
open$t3.q19b.sum <- with(open, rowSums(na90(data.frame(t3.q19.b1.fin, t3.q19.b2.fin, t3.q19.b3.fin, t3.q19.b4.fin, t3.q19.b5.fin)), na.rm=T))
open$t3.q20a.sum <- with(open, rowSums(na90(data.frame(t3.q20.a1.fin, t3.q20.a2.fin, t3.q20.a3.fin, t3.q20.a4.fin, t3.q20.a5.fin)), na.rm=T))
open$t3.q20b.sum <- with(open, rowSums(na902(data.frame(t3.q20.b1.fin, t3.q20.b2.fin, t3.q20.b3.fin, t3.q20.b4.fin, t3.q20.b5.fin)), na.rm=T))
open$t3.q21a.sum <- with(open, rowSums(na90(data.frame(t3.q21.a1.fin, t3.q21.a2.fin, t3.q21.a3.fin, t3.q21.a4.fin, t3.q21.a5.fin)), na.rm=T))
open$t3.q21b.sum <- with(open, rowSums(na90(data.frame(t3.q21.b1.fin, t3.q21.b2.fin, t3.q21.b3.fin, t3.q21.b4.fin, t3.q21.b5.fin)), na.rm=T))

open$t2totr <-  with(open, rowSums(cbind(t2.q18a.sum, t2.q18b.sum, t2.q19a.sum,t2.q19b.sum, t2.q20a.sum, t2.q20b.sum,t2.q21a.sum,t2.q21b.sum), na.rm=T))
open$t3totr <-  with(open, rowSums(cbind(t3.q18a.sum, t3.q18b.sum, t3.q19a.sum,t3.q19b.sum, t3.q20a.sum, t3.q20b.sum,t3.q21a.sum,t3.q21b.sum), na.rm=T))	

open$t2.q18a.uniq <- uniq(with(open, data.frame(t2.q18.a1.fin, t2.q18.a2.fin, t2.q18.a3.fin, t2.q18.a4.fin, t2.q18.a5.fin)))
open$t2.q18a.uniq <- uniq(with(open, data.frame(t2.q18.a1.fin, t2.q18.a2.fin, t2.q18.a3.fin, t2.q18.a4.fin, t2.q18.a5.fin)))
open$t2.q18b.uniq <- uniq(with(open, data.frame(t2.q18.b1.fin, t2.q18.b2.fin, t2.q18.b3.fin, t2.q18.b4.fin, t2.q18.b5.fin)))
open$t2.q19a.uniq <- uniq(with(open, data.frame(t2.q19.a1.fin, t2.q19.a2.fin, t2.q19.a3.fin, t2.q19.a4.fin, t2.q19.a5.fin)))
open$t2.q19b.uniq <- uniq(with(open, data.frame(t2.q19.b1.fin, t2.q19.b2.fin, t2.q19.b3.fin, t2.q19.b4.fin, t2.q19.b5.fin)))
open$t2.q20a.uniq <- uniq(with(open, data.frame(t2.q20.a1.fin, t2.q20.a2.fin, t2.q20.a3.fin, t2.q20.a4.fin, t2.q20.a5.fin)))
open$t2.q20b.uniq <- uniq(with(open, data.frame(t2.q20.b1.fin, t2.q20.b2.fin, t2.q20.b3.fin, t2.q20.b4.fin, t2.q20.b5.fin)))
open$t2.q21a.uniq <- uniq(with(open, data.frame(t2.q21.a1.fin, t2.q21.a2.fin, t2.q21.a3.fin, t2.q21.a4.fin, t2.q21.a5.fin)))
open$t2.q21b.uniq <- uniq(with(open, data.frame(t2.q21.b1.fin, t2.q21.b2.fin, t2.q21.b3.fin, t2.q21.b4.fin, t2.q21.b5.fin)))

open$t3.q18a.uniq <- uniq(with(open, data.frame(t3.q18.a1.fin, t3.q18.a2.fin, t3.q18.a3.fin, t3.q18.a4.fin, t3.q18.a5.fin)))
open$t3.q18b.uniq <- uniq(with(open, data.frame(t3.q18.b1.fin, t3.q18.b2.fin, t3.q18.b3.fin, t3.q18.b4.fin, t3.q18.b5.fin)))
open$t3.q19a.uniq <- uniq(with(open, data.frame(t3.q19.a1.fin, t3.q19.a2.fin, t3.q19.a3.fin, t3.q19.a4.fin, t3.q19.a5.fin)))
open$t3.q19b.uniq <- uniq(with(open, data.frame(t3.q19.b1.fin, t3.q19.b2.fin, t3.q19.b3.fin, t3.q19.b4.fin, t3.q19.b5.fin)))
open$t3.q20a.uniq <- uniq(with(open, data.frame(t3.q20.a1.fin, t3.q20.a2.fin, t3.q20.a3.fin, t3.q20.a4.fin, t3.q20.a5.fin)))
open$t3.q20b.uniq <- uniq(with(open, data.frame(t3.q20.b1.fin, t3.q20.b2.fin, t3.q20.b3.fin, t3.q20.b4.fin, t3.q20.b5.fin)))
open$t3.q21a.uniq <- uniq(with(open, data.frame(t3.q21.a1.fin, t3.q21.a2.fin, t3.q21.a3.fin, t3.q21.a4.fin, t3.q21.a5.fin)))
open$t3.q21b.uniq <- uniq(with(open, data.frame(t3.q21.b1.fin, t3.q21.b2.fin, t3.q21.b3.fin, t3.q21.b4.fin, t3.q21.b5.fin)))

open.c <- open[,c(1, grep("sum", names(open)), grep("uniq", names(open)), 434, 435)]

save(open.c, file="data/open_ended/open.c.rdata")
