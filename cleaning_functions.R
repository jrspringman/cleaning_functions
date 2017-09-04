split_duplicates = function(x,y){
  # Add frequency column to identify duplicates
  n_occur = data.frame(table(x[,y]))
  n_occur$Var1  = as.character(n_occur$Var1)
  a = merge(x,n_occur, by.x = y, by.y = "Var1")
  # Remove and subset duplicates
  assign(x = paste(deparse(substitute(x)),"_duplicates", sep = ""), value = a[which(a[,"Freq"] > 1),], envir = globalenv())
  assign(x = paste(deparse(substitute(x))), value = a[which(a$Freq == 1),], envir = globalenv())
}

split_duplicates(hu_ubos,"FName")
split_duplicates(hu_moh,"name")


datdum <- function(x, data, name){
  data$rv <- rnorm(dim(data)[1],1,1)
  mm <- data.frame(model.matrix(lm(data$rv~-1+factor(data[,x]))))
  names(mm) <- paste(name,1:dim(mm)[2],sep=".")
  data$rv <- NULL
  data <- cbind(data,mm)
  return(data)
}

pol_data <-  datdum(x="share2006_tertile",data=pol_data,name="share_tercile")
pol_data <-  datdum(x="distance_tertile",data=pol_data,name="tercile")

# Functions to correct SEs
hcrobust <- function(fit,vcov=vcovHAC){
  coeftest(fit,vcov)
}
hccluster<-function(fit,vcov=cluster.vcov(fit, pol_data$DNAME_2002)){
  coeftest(fit,vcov)
}

# Functions to extract coefficients and corrected SEs from models 
mfx_robust <- function(mod,var,vcov=vcovHAC){ #mod is an lm() object, var is the name of the main effect that was interacted, vcov is the type of variance covariance method you want to use 
  #Based on code from :https://www.r-bloggers.com/recovering-marginal-effects-and-standard-errors-from-interaction-terms-in-r/
  #Extract Coefficient names create 'beta names' to feed to deltaMethod()
  cnames<-coef(mod)
  pnams<-data.frame('b'=paste('b',0:(length(cnames)-1),sep=""),'est'=cnames) 
  #assign parameter names so that deltaMethod does not throw an error
  
  #Extract the specific parameters of interest
  vars<-grep(var,names(cnames),value=T)
  var1<-vars[1]
  intvars<-vars[2:length(vars)]
  bi<-pnams[var1,'b']
  
  #--Create Data Frame to store Main Effect
  int<-hcrobust(mod,vcov=vcov)[var1,c('Estimate','Std. Error')]
  int<-as.data.frame(t(int))
  names(int)<-c('Estimate','SE')
  row.names(int)<-var1
  
  #Loop through and store the results in a data.frame
  for(i in 1:length(intvars)){
    bint<-pnams[intvars[i],'b']
    eq<-paste(bi,bint,sep="+")
    interac<-deltaMethod(mod,eq,parameterNames=pnams[,1],vcov=vcov)
    row.names(interac)<-intvars[i]
    interac$`2.5 %` <- NULL
    interac$`97.5 %` <- NULL
    a <- list(int, interac)
    int<-rbind.fill(int,interac)
    rownames(int) <- unlist(lapply(a, row.names))
  }
  return(int)
}
mfx_cluster<-function(mod, var, vcov = cluster.vcov(mod, pol_data$DNAME_2002)){ #mod is an lm() object, var is the name of the main effect that was interacted, vcov is the type of variance covariance method you want to use 
  #Extract Coefficient names create 'beta names' to feed to deltaMethod()
  cnames<-coef(mod)
  pnams<-data.frame('b'=paste('b',0:(length(cnames)-1),sep=""),'est'=cnames) 
  #assign parameter names so that deltaMethod does not throw an error
  
  #Extract the specific parameters of interest
  vars<-grep(var,names(cnames),value=T)
  var1<-vars[1]
  intvars<-vars[2:length(vars)]
  bi<-pnams[var1,'b']
  
  #--Create Data Frame to store Main Effect
  int<-hccluster(mod,vcov=vcov)[var1,c('Estimate','Std. Error')]
  int<-as.data.frame(t(int))
  names(int)<-c('Estimate','SE')
  row.names(int)<-var1
  
  #Loop through and store the results in a data.frame
  for(i in 1:length(intvars)){
    bint<-pnams[intvars[i],'b']
    eq<-paste(bi,bint,sep="+")
    interac<-deltaMethod(mod,eq,parameterNames=pnams[,1],vcov=vcov)
    row.names(interac)<-intvars[i]
    interac$`2.5 %` <- NULL
    interac$`97.5 %` <- NULL
    a <- list(int, interac)
    int<-rbind.fill(int,interac)
    rownames(int) <- unlist(lapply(a, row.names))
  }
  return(int)
}

# Remove double whitespace
gsub(' +',' ',foo) 

# Rename variable
names(df)[names(df) == 'old.var.name'] <- 'new.var.name'
