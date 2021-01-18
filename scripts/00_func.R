#
#  Common Functions
#  Last Edited: Oct 12, 2011             
#  Gaurav Sood
#

# Easy recoder (use with recode in car)
l5r <- "5 = 1; 4 = 2; 3 = 3; 2 = 4; 1 = 5"

## Sweave Related Functions
##*******************************
printer <- function(out, caption="caption", align=NULL, label=NULL, size="\\scriptsize") {
     print(xtable::xtable(out, caption=caption, align = align, label=label), 
        caption.placement="top", sanitize.text.function = function(x){x}, floating.environment="table", 
        table.placement = "ht",latex.environment="center", tabular.environment = "tabular", 
        include.rownames=FALSE, add.to.row=addtorow, size=size)
}

## Read SPSS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  spss <- function(file.name.and.path){
    temp <- foreign::read.spss(file.name.and.path, use.value.labels = FALSE, to.data.frame = FALSE, 
        trim.factor.names = TRUE, reencode = NA, use.missings = TRUE)
    temp <- as.data.frame(temp)
    names(temp) <- tolower(names(temp))
    temp
  }
  
  spss2 <- function(file)
  {
    a <- memisc::spss.system.file(file)
    b <- as.data.set(a)
    c <- as.data.frame(b)
    names(c) <- tolower(names(c))
    c
  }

## Variable Manipulation
##***********************************
  # Center by mean
  meanc <- function(x){
    x - mean(x, na.rm=T)
  }
    
  ## Categorical to Dummies
  dumcat <- function(x)
  {
    vec <- levels(as.factor(b))
    temp <- as.data.frame(matrix(rep(0, length(vec)*length(x)), nrow=length(x), ncol=length(vec)))
    names(temp) <- vec
    for(i in 1:length(vec))
    {
    temp[,i] <- as.numeric(x==vec[i])
    }
    temp
  }


## Reverse a string split
  strRev <- function(x) {
          sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
 }

## Dataframe of vectors and list of functions, each applied to respective one
  applier <- function(dfVec,listFunc)
  {
    out <- NA
    if(length(dfVec) < length(listFunc))
    listFunc <- listFunc[1:length(dfVec)]
    if(length(dfVec)==length(listFunc))
    {
    for(i in 1:length(dfVec)){
    out[i] <- eval(listFunc[[i]])(dfVec[i])
    #out[i] <- do.call(listFunc[i], list(dfVec[i]))
      }
    }
    out
  }

#df <- data.frame(x=factor(c(1,1,2,2,2,3,3,3)), y=c(0,2,0,4,8,0,3,6))
  applier2 <- function(df,listFunc){
    newdf <- plyr::ddply(df, "x", transform, y.mean=mean(y), y.sd=sd(y))
    newdf
  }

###################
# 3 Ways to Rename
#####################
##Memisc Rename version
## pkanestv <- memisc::rename(pkanestv, vcf0004="year", vcf0006a="id", vcf0009 ="wt1")

## Change the entire names vector to your satisfaction and then reassign it to original dataset
# renamev(pkanestv, list(c("year", "year1"),c("id", "id1"),c("wt1", "wt11")))
renamev <- function(data, x)
{
  nv <- names(data)  
  for(i in 1:length(x)){
    nv[nv==x[[i]][1]] <- x[[i]][2]
  }  
  return(nv)
}

## Group Related Functions
##*******************************
    wtgrp <- function(data, grp, x, wt){
      plyr::ddply(data,grp, function(X) data.frame(wmn=weighted.mean(X[,x], X[,wt], na.rm=T)))
    }
      
    nf <- function(a){
      len <- length(a) - 1
      ans <- rep(NA, length(a))
       for(i in 1:length(a))
      {
        ans[i] <- (sum(a, na.rm=T) - a[i])/len
      }
      ans
    }
  
### Speciality Functions 
##*******************************
  ## Poll Group Maker; Ensures Group has 2 digits, and adds to pollid
  pgroup <- function(group, pollid){
    temp <- group
    temp <- ifelse(temp < 10, paste("0", temp, sep=""), temp)
    as.numeric(paste(pollid, temp, sep=""))
  }
  
  
  ## Function to Correct for Guessing. Codes to 0 if someone is right at t1 but wrong at t2 or t3
  pkcor <- function(t1pk, t2pk, t3pk=NULL)
  {
    #if(all(c(unique(t1pk) %in% c(NA, 0, 1), unique(t2pk) %in% c(NA, 0, 1)))){
    temp <- t1pk
    t1   <- nona(t1pk)
    t2    <-  nona(t2pk)
    temp[t1 & !t2] <- 0
    if(!is.null(t3pk)) 
    {
      t3 <-  nona(t3pk)
      temp[t1 & !t3] <- 0
    }
    temp
  }

  ## Function for Group Gain
  groupgain <- function(knowindex, group, numrow, numitems, grpsize) 
  {
    # Initializing data frame
    groupmean <-  as.data.frame(matrix(rep(0, numrow*length(knowindex)), nrow=numrow, ncol=length(knowindex)))
    # Initializing column with data.frame number of rows
    netmean <-c(rep(0, numrow))
    #For all the knowledge items
    for(i in 1:length(knowindex)) 
    {
      #Group's mean score on the item 
      groupmean[,i] <- unsplit(lapply(split(knowindex[,i], group), function(a) mean (c(a), na.rm=TRUE)), group)
      #For all rows
      for(j in 1:numrow) 
      {
        if(!knowindex[j,i]) #if item not answered correctly, group mean of the jth item
          netmean[j] <- netmean[j] + (groupmean[j,i]*grpsize[j]) /(numitems[j]*(grpsize[j] -1))
        else netmean[j] <- netmean[j] + 0
      }
    }
    netmean
  }

# Append Multiple objects
  append.Rda <- function(x, file) 
  {
     old.objects <- load(file, new.env())
     save(list = c(old.objects, deparse(substitute(x))), file = file)
  }


## Krosnick Number format generator
##**********************************
  kros <- function(x){
    # tester: x <- c(.200, .123, 2.2)
    if(length(x) > 1){ 
      temp <- c(); 
      for(i in 1:length(x)){
        temp[i] <- kros(x[i])
      } 
      return(temp)
    }
    temp <- sprintf("%1.2f", as.numeric(x))
    # format(c(0.0, 13.1), digits = 2, nsmall = 2)
    if(as.numeric(temp)==0) temp <- "0.00"
    splits <- strsplit(temp, "\\.")[[1]]
    if(splits[1]=="0")  temp <- paste(".",splits[2], sep="")
    #grep("\\.+", 4.22)
    #format()
    if(splits[1]=="-0") temp <- paste("-.", splits[2], sep="")  
    temp
  }
