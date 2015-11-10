### Model_Select.R
### Tom Bird
### Nov 9 2015

### Description:
## This function runs a cross validation procedure on all
## combinations of data within a dataset.  
## For now it only does linear models

Model_Select=function(dat, frac=0.8, it, covs, resp,mincov=1, maxcov){
#initialize empty lists
AICs=pr2=r2=npar=iter=dev=numeric()
block=var=mods=typ=character()

for(i in 1:it){  # replicate 
  subind=sample(1:nrow(dat), nrow(dat)*frac)
  subdat=dat[subind,]
  preddat=dat[-subind,]
  for(n in mincov:maxcov){
	combs=combn(covs, n)
      for(n in 1:ncol(combs)){
      mod=paste(combs[,n], collapse="+")
      lmmod=formula(paste(resp,"~", mod))
      lmout=lm(lmmod, data=subdat)
      r2=c(r2, cor(lmout$fitted, subdat[[resp]])^2)
      pr2=c(pr2, cor(predict(lmout, newdata=preddat), preddat[[resp]])^2)
      AICs=c(AICs, AIC(lmout))
      dev=c(dev, deviance(lmout))
      typ=c(typ, "lm")
      mods=c(mods, mod)
      npar=c(npar, n)
       }
  }
  
}

outputs=data.frame(r2=r2, pr2=pr2, mod=mod, AICs=AICs, typ=typ, npar=npar)
op=ddply(outputs, .(mod, typ), summarise,
         r2=mean(r2),
         pr2=mean(pr2),
         AICs=mean(AICs),
         typ=typ[1],
         npar=npar[1])
op=op[order(op$pr2, decreasing=T),]

## return sorted values
return(op)
}


## example of usage

### example dataset
data(mtcars)

cars_select=Model_Select(dat=mtcars, covs=colnames(mtcars)[-1], resp=colnames(mtcars)[1], frac=0.8, it=10, maxcov=5)


