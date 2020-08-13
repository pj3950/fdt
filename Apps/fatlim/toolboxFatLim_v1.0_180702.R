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
## SNfunktion för spektrumutmattning. Beräknar en Wöhlerkurva och plottar den 
#tillsammans med 95% prediktionsintervall. Överlevare tas med med hjälp av 
#Maximum-likelihood-skattning, då prediktionsintervallen bestäms med hjälp av 
#Lawless approximativa eqvivalenta antal frihetsgrader.
#Förhandsgissning av lutning inkluderas i regressionen. Om den skall fixeras
#sätts såväl låg som hög gräns till det fixa värdet.
#S  	      lastnivåer
#N		      livslängder
#RO		      överlevare="RO"
#int		    alernativ: "none","predict" eller "conf"
#conf.level täckningssannolikhet för figurintervall och för begärd styrka
#betaint	  förhandsgissning om lutning som ett intervall
#predS	    Lastnivåer som används för att prediktera livslängder
#Nstrength	Cykelantal för att bestämma utmattningsstyrka
#title, ylabel, xlabel	titel och axelangivelser
#formel	anger om formeln skall skrivas ut
#Nrange	    Skalintervall för antal cykler
#Srange	    Skalintervall för last
#plottype   alternativ: "p" for points, "l" for lines, "b" for both,
#
# ===========================================================

fatlim.est <- function(data, RO=2e6, conf.level=0.95, int="conf",
                   plot=TRUE, title="Stair case test result with 95% confidence limits",
                   xlab="specimen number", ylab="load level", ylim=NULL)
  
#  fat_lim_web<-function(RR,title="Stair case test result with 95% confidence limits",
#                        xlabel="specimen number",ylabel="load level",
#                        conf.level=0.95,int="conf",RO=2e6,ylim=NULL)
  
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
      lopt<-f(res$estimate,fail,nofail)
      
      #Vi använder nu profile likelihood för att ta fram konfidensgränser.
      #Först för väntevärdet
      
      #notera först den optimala log s-skattningen
      res2<-res$estimate[2]
      
      #initiera variabler
      myref<-0
      lsig<-0
      
      #ange längden på referensvektorn
      #    len1<-500
      len1<-1001
      
      #gör en referensvektor av standardavvikelser
      #    steps<-seq(-3*exp(res$estimate[2]),3*exp(res$estimate[2]),len=len1)
      #    steps<-seq(-500*exp(res$estimate[2]),500*exp(res$estimate[2]),len=len1)
      steps0<-seq(-50,50,len=len1)
      steps<-sign(steps0)*steps0^2*exp(res$estimate[2])
      
      #Vi kommer nu att minimera med avseende på standardavvikelsen för olika värden på väntevärdet
      #Den dubbla skillanden mellan sådana loglikelihoodvärden och det globalt optimala är chi2-fördelat
      #Vi stegar ut från mitten i två omgångar:
      for (i in seq((length(steps)+1)/2,1,-1))
      {
        myref[i]<-res$estimate[1]+steps[i] #bestäm ett förskjutet väntevärde
        
        res2<-nlm(f2,res2,myref[i],fail,nofail,ndigit=4)$estimate #minimera likelihooden map sigma
        lsig[i]<-f(c(myref[i],res2),fail,nofail)     #spara likelihoodvärdet
        if (lsig[i]-lopt>chilim) break      #sluta när vi passerat gränsen för 95% konfidens enligt Chi2
      }
      #Här kommer omgång två
      for (i in seq((length(steps)+1)/2,length(steps),1))
      {
        myref[i]<-res$estimate[1]+steps[i]
        
        res2<-nlm(f2,res2,myref[i],fail,nofail,ndigit=4)$estimate
        lsig[i]<-f(c(myref[i],res2),fail,nofail)
        if (lsig[i]-lopt>chilim) break
      }
      
      #    plot(myref,lsig-lopt,type='l')
      
      #Vi gör nu likadant med sigma
      res1<-0
      lsigref<-0
      lmy<-0
      #    len2<-600
      #    steps<-seq(-log(4),log(20),length=len2)
      len2<-1001
      steps<-seq(-log(4+0),log(20+0),length=len2)
      
      
      mid<-(length(steps)+1)/2
      #mid<-which.min(abs(steps))
      for (i in seq(mid,1,-1))
      {
        lsigref[i]<-res$estimate[2]+steps[i]
        res1<-nlm(f1,res$estimate[1],exp(lsigref[i]),fail,nofail,ndigit=4)$estimate
        lmy[i]<-f(c(res1,lsigref[i]),fail,nofail)
        if (lmy[i]-lopt>chilim) break
      }
      for (i in seq(mid,length(steps),1))
      {
        lsigref[i]<-res$estimate[2]+steps[i]
        res1<-nlm(f1,res$estimate[1],exp(lsigref[i]),fail,nofail,ndigit=4)$estimate
        lmy[i]<-f(c(res1,lsigref[i]),fail,nofail)
        if (lmy[i]-lopt>chilim) break
      }
      
      #Vi betämmer nu konfidensgränserna för my och sigma
      
      lsigref<-lsigref[lmy>0]
      myref<-myref[lsig>0]
      lsig<-lsig[lsig>0]
      lmy<-lmy[lmy>0]
      
      x<-lsig-lopt
      mitt<-which.min(x)
      #    mitt<-(length(steps)-1)/2 
      
      #    plot(x[1:mitt],myref[1:mitt],'l')
      #    plot(x[mitt:length(x)],myref[mitt:length(x)],'l')
      
      limmy<-c(approx(x[1:mitt],myref[1:mitt],chilim)$y,approx(x[mitt:length(x)],myref[mitt:length(x)],chilim)$y)
      
      if(is.na(limmy[1]))  mylow<-paste("<",signif(myref[1],3),sep="")  else mylow<-as.character(signif(limmy[1],3))
      if(is.na(limmy[2]))  myhigh<-paste(">",signif(myref[length(x)],3),sep="")  else myhigh<-as.character(signif(limmy[2],3))
      
      x<-lmy-lopt
      mitt<-which.min(x)
      #    limsig<-exp(c(approx(x[1:mitt],lsigref[1:mitt],chilim)$y,approx(x[mitt:length(x)],lsigref[mitt:length(x)],chilim)$y))
      limsig<-c(NA,NA) # Don't compute CI for sigma
      
      if(is.na(limsig[1]))  siglow<-paste("<",signif(exp(lsigref[1]),3),sep="")  else siglow<-as.character(signif(limsig[1],3))
      if(is.na(limsig[2]))  sighigh<-paste(">",signif(exp(lsigref[length(x)]),3),sep="")  else sighigh<-as.character(signif(limsig[2],3))
      
      list("my"=res$estimate[1],"sigma"=exp(res$estimate[2]),"mylim"=c(mylow,myhigh),"sigmalim"=c(siglow,sighigh))
      
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
    
    result$predstdmy<-signif(predstd/result$my,2)
#    result$predlow<-signif(predlow,3)
    
    
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
    result$RO <- 2e6
    result$conf.level <- 0.95 
    result$int <- "conf"
    
    fatlim.plot(result, title=title,
                 xlab=xlab, ylab=ylab, ylim=ylim)
    
    return(result)
    
  }
  # END: fat_lim_web
  # -------------------------------------------------------------------------------


# ===========================================================
# Function:
#   fatlim.print
#
## SNfunktion för spektrumutmattning. Beräknar en Wöhlerkurva och plottar den 
#tillsammans med 95% prediktionsintervall. Överlevare tas med med hjälp av 
#Maximum-likelihood-skattning, då prediktionsintervallen bestäms med hjälp av 
#Lawless approximativa eqvivalenta antal frihetsgrader.
#Förhandsgissning av lutning inkluderas i regressionen. Om den skall fixeras
#sätts såväl låg som hög gräns till det fixa värdet.
#S  	      lastnivåer
#N		      livslängder
#RO		      överlevare="RO"
#int		    alernativ: "none","predict" eller "conf"
#conf.level täckningssannolikhet för figurintervall och för begärd styrka
#betaint	  förhandsgissning om lutning som ett intervall
#predS	    Lastnivåer som används för att prediktera livslängder
#Nstrength	Cykelantal för att bestämma utmattningsstyrka
#title, ylabel, xlabel	titel och axelangivelser
#formel	anger om formeln skall skrivas ut
#Nrange	    Skalintervall för antal cykler
#Srange	    Skalintervall för last
#plottype   alternativ: "p" for points, "l" for lines, "b" for both,
#
# ===========================================================

fatlim.plot <- function(FL, title="Stair case test result with 95% confidence limits",
                       xlab="specimen number", ylab="load level", ylim=NULL)
  
{
  RR <- FL$data

  #Vi plottar nu själva provningsresultatet
  nofailures<-which(RR[,2]==0)
  failures<-which(RR[,2]==1)
  symbol<-numeric(0)
  symbol[failures]<-4
  symbol[nofailures]<-1
  n_of_levels<-diff(range(RR[,1]))/min(abs(diff(RR[,1])))
  
  plot(1:length(RR[,1]),RR[,1],pch=symbol,lwd=1,cex=1,xlab=xlab,ylab=ylab,main=title, ylim=ylim)
  grid()
  
  # Plot estimate and confidence limits of median fatigue limit
  abline(h=FL$my[1],col="blue",lwd=1.5)
  if (FL$int!="none")
  {
    abline(h=FL$mylim[1],col="blue",lwd=1.5,lty="dashed")
    abline(h=FL$mylim[2],col="blue",lwd=1.5,lty="dashed")
  }
  
  mtext("RISE Fatigue Design Tool",3,0,font=21,cex=0.7,col="blue",adj=1)
  
  
}

# END: fat_lim_web
# -------------------------------------------------------------------------------

  