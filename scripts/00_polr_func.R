## ******************************************##
## Common Functions to Polarization			 ##
## 2/18/11									 ##
## Author: Gaurav						     ##
## ******************************************##

#Weighted Proportion
wt.prop <- function(var, weight=NULL){
	if(is.null(weight)) return(prop.table(xtabs( ~ var))) 
	prop.table(xtabs(weight ~ var))
}

# Test of Proportions
prop.t <- function(x, y, wtx=NULL, wty=NULL){
	# x <- rep$marry.d ==1;wtx <- rep$weight;y <- dem$marry.r==1;wty <- dem$weight
	grp  <- c(rep(1, length(x)), rep(0, length(y)))
	wt   <- c(wtx, wty)
	if(all(is.null(wt))){ 
		diff <- mean(x, na.rm=T) - mean(y, na.rm=T)
		return(paste(kros(diff), stars(fisher.test(table(c(x,y), grp))$p.val), sep=""))
	}
	diff <- weighted.mean(x, wtx, na.rm=T) - weighted.mean(y, wty, na.rm=T)
	paste(kros(diff), stars(chisq.test(round(xtabs(wt ~ c(x,y) + grp)))$p.val), sep="")
}

prop.all <- function(x,y,wtx=NULL, wty=NULL){
	# x <- rep$marry.d ;wtx <- NULL;y <- dem$marry.r;wty <- NULL
	# x <- v.rep$r.ignorant; wtx <- NULL; y <- v.rep$d.ignorant;wty <- NULL
	len <- ifelse(length(levels(factor(x))) > length(levels(factor(y))), length(levels(factor(x))), length(levels(factor(y))))
	
	res <- matrix(ncol=3, nrow=len)
	res[,1] <- kros(wt.prop(x, wtx))
	res[,2] <- kros(wt.prop(y, wty))
	j <- 1
	for(i in levels(factor(x))){
		res[j,3] <- prop.t(x==i, y==i, wtx, wty)	
		j <- j + 1
	}
	res
}

wt.all <- function(x,y,wtx=NULL, wty=NULL){
	nonax <- !is.na(x) & !is.na(wtx)
	nonay <- !is.na(y) & !is.na(wty)
	tx 	  <- x[nonax]
	wx 	  <- wtx[nonax]
	ty    <- y[nonay]
	wy 	  <- wty[nonay]
	mx    <- weighted.mean(tx, w=wx)
	my    <- weighted.mean(ty, w=wy)
	diff <- summary(lm(c(tx,ty) ~ c(rep(1, length(tx)), rep(0, length(ty))), weight=c(wx, wy)))
	c(kros(mx), kros(my), paste(kros(diff$coeff[2,1]), stars(diff$coeff[2,4]), sep=""))
}

# Weighted Mean of two variables
wt.mean <- function(x, y, wt){
	c(round(weighted.mean(x,wt, na.rm=T),2), round(weighted.mean(y,wt, na.rm=T),2))
}

# Combining variables
out <- function(var1, var2, grp1, grp2){
	# tester var1 <- verba$r.welfare; var2 <- verba$d.welfare; grp1 <- verba$pid5 %in% c(1,2); grp2 <- verba$pid5 %in% c(4,5)
	res <- rep(NA, length(var1))
	res[grp1] <- var1[grp1]
	res[grp2] <- var2[grp2]
	res
}

#a2004$fthermout <- NA 
#a2004$fthermout[which(as.numeric(a2004$demrep)<4)] <- a2004$fthermrep[which(as.numeric(a2004$demrep)<4)]
#a2004$fthermout[which(as.numeric(a2004$demrep)>4)] <- a2004$fthermdem[which(as.numeric(a2004$demrep)>4)]
