# ===========================================================
# Toolbox: Fatigue limit estimation
#
# Thomas Svensson & Pär Johannesson
#
# Functions:
#   fatlim.est
#   fatlim.plot
#
# ===========================================================


# ===========================================================
# Function:
#   fatlim.est
#
# Estimation of endurance limit.
#
#
# ===========================================================

fatlim.est <- function(data, RO=2e6, conf.level=0.95, int="conf",
                   plot=1, title=NA,
                   xlab="Specimen number", ylab="Load level", ylim=NULL, lwd=1, cex=1, cex.txt=1)
  
# data    = The data must be a matrix with three columns and the number of rows equal to the number of specimens. 
#  Each row contains the run order in the first column, the recorded load amplitude in the second and the result 
#  failure/no failure, represented by "1"/"0", in the last column.
# RO      = Run-out level (number of cycles)
# int		  =  "none" / "conf"
# conf.level    = Confidence level
# plot          = Plot if >0
# title, xlab, ylab, ylim = Plot parameters

  
    #bestämmer en prediktionfördelning för utmattningsgränsen
    #med hjälp av predictive profile likelihood, samt ritar ut provningsresultatet.
    #
    # by Thomas Svensson
    # modified by Pär Johannesson
  {
  RR <- data
  
    #Först sorterar vi om en ordning är angiven
    if (length(RR)>2)
    {
      index<-sort.int(RR[[1]],index.return=T)$ix   
      RR<-RR[index,2:3] 
    } 
    
    #Vi bildar nu en kolumn med nollor och ettor från eventuella textidentiteter
    RR[[3]]<-as.character(RR[[2]])
    RR[[2]]<-as.integer(RR[[2]])
    
    #Om första tecknet i kolumn 2 är 1, f eller F så är det en failure, annars överlevare
    for (i in 1:length(RR[[3]]))
      RR[[2]][i]<-ifelse(substring(RR[[3]][i],1,1)%in%c("1","f","F"),1,0)
    
    #sedan kollar vi om nollor och ettor är rimligt valda
    plot_and_return = FALSE
    fail<-RR[[1]][RR[[2]]==1]
    nofail<-RR[[1]][RR[[2]]==0]
    
    if (length(fail)==0) {
      print("Only run-outs, thus it is not possible to estimate fatigue limit.")
      plot_and_return = TRUE
    }
    else if (length(nofail)==0) {
      print("Only failures, thus it is not possible to estimate fatigue limit.")
      plot_and_return = TRUE
    }
    else if (median(fail)<=median(nofail)) {
      print("Unrealistic test result, perhaps survivers and fails are mixed up!")
      plot_and_return = TRUE
    }
    
    if (plot_and_return) {
      
      #Vi plottar nu själva provningsresultatet
      nofailures<-which(RR[,2]==0)
      failures<-which(RR[,2]==1)
      symbol<-numeric(0)
      symbol[failures]<-4
      symbol[nofailures]<-1
      n_of_levels<-diff(range(RR[,1]))/min(abs(diff(RR[,1])))
      
      plot(1:length(RR[,1]),RR[,1],pch=symbol,lwd=1,cex=1,xlab=xaxel,ylab=yaxel,main=title, ylim=ylim)
      grid()
      
      #return("Unrealistic test result, perhaps survivers and fails are mixed up!")
      result <- list("my"=NA, "sigma"=NA, "mylim"=c(NA,NA), "sigmalim"=c(NA,NA), "predstdmy"=NA, "predlow"=NA)
      return(result)
    }
    
    
    
    # -------------------------------------------------------------------------------
    # Funktion för predictive likelihood för enskilt predikterat värde S
    #
    #
    
    fatlim_pred<-function(S,RR,fail=RR[[1]][RR[[2]]==1],nofail=RR[[1]][RR[[2]]==0],my=median(c(fail,nofail)),sigma=0.1*my,prior=c(0,1),conf.level)
      #Predictive profile likelihood.
      #Vi beräknar prediktionslikelihood för S givet RR
      #Genom att göra detta för alla S och normera får vi frekvensfunktionen
      #för predikterad utmattningsgräns inklusive parameterosäkerhet
      
      #först bildar vi funktionen för likelihoodberäkning
    {
      
      fi<-function(theta,S,fail,nofail)
      {-dnorm(S,theta[1],exp(theta[2]),log=TRUE)-sum(pnorm(fail,theta[1],exp(theta[2]),log.p=T))-sum(pnorm(nofail,theta[1],exp(theta[2]),log.p=T,lower.tail=FALSE))}
      
      #Vi skattar sedan utmattningsgränsen och dess standardavvikelse
      res<-nlm(fi,c(my,log(sigma)),S,fail,nofail,ndigit=4,stepmax=my/100)
      
      #Det minimala loglikelihoodvärdet returneras
      lopt<-fi(res$estimate,S,fail,nofail)
      
      return(exp(-lopt))
    }
    
    # END: fatlim_pred 
    # -------------------------------------------------------------------------------
    
    
    # -------------------------------------------------------------------------------
    # Funktion för allmän utmattningsgränsberäkning
    
    fatlim<-function(RR,conf.level,int)
      #utmattningsgränsberäkning. Notera att man kan välja att ha en data.frame som input eller ha
      #två variabler med fail respektive nofail.
      
    {
      
      chilim<-qchisq(conf.level,1)/2
      
      #först bildar vi funktioner för ML-minimering
      
      
      fail<-RR[[1]][RR[[2]]==1]
      nofail<-RR[[1]][RR[[2]]==0]
      my<-median(c(fail,nofail))  #förhandsgissning av väntevärde
      sigma<-0.1*my     #förhandsgissning av standardavvikelse
      
      
      #Vi bildar tre funktioner för likelihoodberäkningar
      f<-function(theta,fail,nofail)
      {-sum(pnorm(fail,theta[1],exp(theta[2]),log.p=T))-sum(pnorm(nofail,theta[1],exp(theta[2]),log.p=T,lower.tail=FALSE))}
      
      f1<-function(my,sigma,fail,nofail)
      {-sum(pnorm(fail,my,sigma,log.p=T))-sum(pnorm(nofail,my,sigma,lower.tail=FALSE,log.p=T))}
      
      f2<-function(lsigma,my,fail,nofail)
      {-sum(pnorm(fail,my,exp(lsigma),log.p=T))-sum(pnorm(nofail,my,exp(lsigma),lower.tail=FALSE,log.p=T))}
      
      #Vi skattar sedan utmattningsgränsen och dess standardavvikelse
      res<-nlm(f,c(my,log(sigma)),fail,nofail)
      
      #Det minimala loglikelihoodvärdet tas tillvara
      #lopt<-f(res$estimate,fail,nofail)
      lopt<-res$minimum
      
      #Vi använder nu profile likelihood för att ta fram konfidensgränser.
      #Först för väntevärdet
      
      #notera först den optimala log s-skattningen
      res2<-res$estimate[2]
      
      #initiera variabler
      myref<-0
      lsig<-0
      
      #ange längden på referensvektorn
      len1<-1001
      
      #gör en referensvektor av standardavvikelser
      steps<-seq(-10,10,len=len1)*exp(res$estimate[2])
      mid1<-(len1+1)/2
      
      #Vi kommer nu att minimera med avseende på standardavvikelsen för olika värden på väntevärdet
      #Den dubbla skillanden mellan sådana loglikelihoodvärden och det globalt optimala är chi2-fördelat
      K<-1.1
      #Vi stegar ut från mitten i två omgångar:
      for (i in seq(mid1,1,-1))
      {
        myref[i]<-res$estimate[1]+steps[i] #bestäm ett förskjutet väntevärde
        
        res2<-nlm(f2,res2,myref[i],fail,nofail,ndigit=4)$estimate #minimera likelihooden map sigma
        lsig[i]<-f(c(myref[i],res2),fail,nofail)     #spara likelihoodvärdet
        if (lsig[i]-lopt>K*chilim) break      #sluta när vi passerat gränsen för 95% konfidens enligt Chi2
      }
      #Här kommer omgång två
      for (i in seq(mid1,len1,1))
      {
        myref[i]<-res$estimate[1]+steps[i]
        
        res2<-nlm(f2,res2,myref[i],fail,nofail,ndigit=4)$estimate
        lsig[i]<-f(c(myref[i],res2),fail,nofail)
        if (lsig[i]-lopt>K*chilim) break
      }
      
      
      #Vi gör nu likadant med sigma
      res1<-res$estimate[1]
      lsigref<-0
      lmy<-0

      len2<-1001
#      steps<-seq(-log(4+0),log(20+0),length=len2)
      steps<-seq(-log(100),log(100),length=len2)
      mid2<-(len2+1)/2
      #mid2<-which.min(abs(steps))
      for (i in seq(mid2,1,-1))
      {
        lsigref[i]<-res$estimate[2]+steps[i]
        res1<-nlm(f1,res1,exp(lsigref[i]),fail,nofail,ndigit=4)$estimate
        lmy[i]<-f(c(res1,lsigref[i]),fail,nofail)
        if (lmy[i]-lopt>K*chilim) break
      }
      for (i in seq(mid2,len2,1))
      {
        lsigref[i]<-res$estimate[2]+steps[i]
        res1<-nlm(f1,res1,exp(lsigref[i]),fail,nofail,ndigit=4)$estimate
        lmy[i]<-f(c(res1,lsigref[i]),fail,nofail)
        if (lmy[i]-lopt>K*chilim) break
      }
      
      
      #Vi betämmer nu konfidensgränserna för my och sigma
      
      lsigref<-lsigref[lmy>0]
      myref<-myref[lsig>0]
      lsig<-lsig[lsig>0]
      lmy<-lmy[lmy>0]
      
      x<-lsig-lopt
      mitt<-which.min(x)
      limmy<-c(approx(x[1:mitt],myref[1:mitt],chilim)$y,approx(x[mitt:length(x)],myref[mitt:length(x)],chilim)$y)
      
#      if(is.na(limmy[1]))  mylow<-paste("<",signif(myref[1],3),sep="")  else mylow<-as.character(signif(limmy[1],3))
#      if(is.na(limmy[2]))  myhigh<-paste(">",signif(myref[length(x)],3),sep="")  else myhigh<-as.character(signif(limmy[2],3))
      
      x<-lmy-lopt
      mitt<-which.min(x)
      limsig<-exp(c(approx(x[1:mitt],lsigref[1:mitt],chilim)$y,approx(x[mitt:length(x)],lsigref[mitt:length(x)],chilim)$y))
      #limsig<-c(NA,NA) # Don't compute CI for sigma
      
#      if(is.na(limsig[1]))  siglow<-paste("<",signif(exp(lsigref[1]),3),sep="")  else siglow<-as.character(signif(limsig[1],3))
#      if(is.na(limsig[2]))  sighigh<-paste(">",signif(exp(lsigref[length(x)]),3),sep="")  else sighigh<-as.character(signif(limsig[2],3))
      
      # For debugging: Plot profile likelihoods
      if(plot>1) {
        plot(myref,lsig-lopt,type='l', main="Profile likelihood for my"); abline(h=chilim, col="red")
        plot(lsigref,lmy-lopt,type='l', main="Profile likelihood for sigma"); abline(h=chilim, col="red")
      }

#      list("my"=res$estimate[1],"sigma"=exp(res$estimate[2]),"mylim"=c(mylow,myhigh),"sigmalim"=c(siglow,sighigh))
      list("my"=res$estimate[1],"sigma"=exp(res$estimate[2]),"mylim"=limmy,"sigmalim"=limsig)
      
    } 
    # END: fatlim 
    # -------------------------------------------------------------------------------
    
    
    
    
    #Vi skall nu räkna ut predictive likelihood för många värden för att kunna normera och
    #beräkna varians
    
    #bestäm först utmattningsgränsen på vanligt sätt
    
    result<-fatlim(RR,conf.level,int)
    
    
    print(result)
    
    
    #bilda en lämplig referensvektor av laster att prediktera för
    
    ref<-seq(max(result$my-5*result$sigma,0),result$my+5*result$sigma,length=300)
    
    #skapa en tom likelihoodvektor
    L<-0
    
    #beräkna likelihood för alla referensvärden
    for (i in 1:length(ref))L[i]<-fatlim_pred(ref[i],RR,conf.level)
    
    #normera till en frekvensfunktion
    d<-L/(sum(L))
    
    predlow<-ref[max(which(cumsum(d)<=(1-conf.level)/2))]
    
    
    #bestäm väntevärdet ur frekvensfunktionen
    
    E<-sum(ref*d)
    
    #bestäm standardavvikelsen ur frekvensfunktionen
    predstd<-sqrt(sum((ref-E)^2*d))
    
#    result$my<-signif(result$my,3)
#    result$sigma<-signif(result$sigma,3)
    
#    result$predstdmy<-signif(predstd/result$my,2)
#    result$predlow<-signif(predlow,3)

    result$predstdmy<-predstd/result$my
    result$predlow<-predlow
    
    # result.out<-c(as.character(c(result$my,result$sigma)),result$mylim[1],result$mylim[2],
    #               result$sigmalim[1],result$sigmalim[2],as.character(c(result$predstdmy,result$predlow,RO,conf.level)))
    
    #Ordning på data i utfil:
    #my, estimate of the fatigue limit
    #sigma, estimate of its standard deviation
    #mylim, confidence limits for the fatigue limit
    #sigmalim, confidence limits for the standard deviation
    #estimated coefficient of variation of the predicted fatigue limit
    #predlow, estimated lower prediction limit
    #RO, maximum number of cycles for failures 
    #conf.level, confidence level
    
    #  result.out  
    
    result$data <- RR
    result$RO <- RO
    result$conf.level <- conf.level 
    result$int <- int
    
#    print(result)
    
    # Plot: Stair case test results
    if(plot) {
      if(is.na(title)) {title=paste0("Endurance limit with ", 100*conf.level, "% confidence limits")}
      fatlim.plot(result, int=int, title=title, xlab=xlab, ylab=ylab, ylim=ylim, lwd=lwd, cex=cex, cex.txt=cex.txt)
    }
    
    return(result)
    
  }
  # END: fatlim.est
  # -------------------------------------------------------------------------------


# ===========================================================
# Function:
#   fatlim.plot
#
#
# ===========================================================

fatlim.plot <- function(FL, int="conf", title="Endurance limit with 95% confidence limits",
                       xlab="Specimen number", ylab="Load level", ylim=NULL, lwd=1, cex=1, cex.txt=1)
  
{
  RR <- FL$data

  #Vi plottar nu själva provningsresultatet
  nofailures<-which(RR[,2]==0)
  failures<-which(RR[,2]==1)
  symbol<-numeric(0)
  symbol[failures]<-4
  symbol[nofailures]<-1
  n_of_levels<-diff(range(RR[,1]))/min(abs(diff(RR[,1])))
  
  plot(1:length(RR[,1]),RR[,1],pch=symbol, xlab=xlab,ylab=ylab,main=title, ylim=ylim, 
       lwd=lwd, cex=cex, cex.main=cex.txt, cex.lab=cex.txt, cex.axis=cex.txt)
  grid()
  
  # Plot estimate and confidence limits of median fatigue limit
  abline(h=FL$my[1],col="blue",lwd=1.5*lwd)
  if (FL$int!="none")
  {
    abline(h=FL$mylim[1],col="blue",lwd=1.5*lwd,lty="dashed")
    abline(h=FL$mylim[2],col="blue",lwd=1.5*lwd,lty="dashed")
  }
  
  mtext("RISE Fatigue Design Tool",3,0,font=21,cex=0.7*cex.txt,col="blue",adj=1)
  
  
}

# END: fatlim.plot
# -------------------------------------------------------------------------------

  