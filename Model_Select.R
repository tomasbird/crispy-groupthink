dat=dist.df
AICs=pr2=r2=npar=iter=dev=numeric()
block=var=mod=typ=character()

for(i in 1:10){
  subind=sample(1:nrow(dist.df), nrow(dist.df)*frac)
  subdat=dat[subind,]
  preddat=dat[-subind,]
  for(b in 1:length(blocklist)){
    for(v in 1:length(blocklist[[b]])){
      srch=blocklist[[b]][v]
      lmmod=formula(paste("dist.Faith~", srch))
      lmout=lm(lmmod, data=subdat)
      r2=c(r2, cor(lmout$fitted, subdat$dist.Faith)^2)
      pr2=c(pr2, cor(predict(lmout, newdata=preddat), preddat$dist.Faith)^2)
      AICs=c(AICs, AIC(lmout))
      dev=c(dev, deviance(lmout))
      typ=c(typ, "lm")
      mod=c(mod, srch)
      npar=c(npar, 1)
      if(!srch %in% c(Regblocks,Contblocks,ODblocks)){
        gammod=formula(paste("dist.Faith~s(", srch, ")"))
        gamout=gam(gammod, data=subdat)
        r2=c(r2, cor(predict(gamout, newdata=subdat), subdat$dist.Faith)^2)
        pr2=c(pr2, cor(predict(gamout, newdata=preddat), preddat$dist.Faith)^2)
        AICs=c(AICs, AIC(lmout))
        dev=c(dev, deviance(gamout))
        typ=c(typ, "glm")
        mod=c(mod, srch)
        npar=c(npar, 1)}
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
