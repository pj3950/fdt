SNw<-function(S, N, RO, title="", ylabel="", xlabel="", formel=TRUE, int="pred", conf.level=0.95,
              beta_low=NA,beta_high=NA,S_prior=NA,S_prior_high=NA, Nstrength=2e6, Npred=2e6, 
              preds_low=NA,preds_high=NA,Srange=NA,Nrange=NA,
              plottype="b",
              name_R=NA,figname=figname,DNVplot=FALSE)
## SNfunktion för spektrumutmattning. Beräknar en Wöhlerkurva och plottar den 
#tillsammans med 95% prediktionsintervall. Överlevare tas med med hjälp av 
#Maximum-likelihood-skattning, då prediktionsintervallen bestäms med hjälp av 
#Lawless approximativa eqvivalenta antal frihetsgrader.
#Förhandsgissning av lutning inkluderas i regressionen. Om den skall fixeras
#sätts såväl låg som hög gräns till det fixa värdet.
#S  	      lastnivåer
#N		      livslängder
#RO		      överlevare="RO"
#int		    alernativ: "none","pred" eller "conf"
#conf.level täckningssannolikhet för figurintervall och för begärd styrka
#betaint	  förhandsgissning om lutning som ett intervall
#predS	    Lastnivåer som används för att prediktera livslängder
#Nstrength	Cykelantal för att bestämma utmattningsstyrka
#title, ylabel, xlabel	titel och axelangivelser
#formel	anger om formeln skall skrivas ut
#Nrange	    Skalintervall för antal cykler
#Srange	    Skalintervall för last
#plottype   alternativ: "p" for points, "l" for lines, "b" for both,

{
  # Input; int convert to lower case and extract first 4 char (alernativ: "none","pred" eller "conf")
  int <- substr(tolower(int), 1, 4)
  
  #Intervall för förhandsgissning
  betaint <- c(beta_low, beta_high)
  #Om inget intervall är angivet ansätts ett:
  if (any(is.na(betaint))) {
    w=0
    #betaint<-c(0,30)
  }
  #Om fix lutning begärts, ge den ett litet intervall för att 
  #kunna användas i den generella metodiken.
  if (!any(is.na(betaint))){
    if (diff(range(betaint))<0.01) betaint<-betaint+c(-1,1)*0.01
  } 
  
  
  #Vi bestämmer ekvivalent antal frihetsgrader för förhandsgissning om standardavvikelse
  kvot<-S_prior_high/S_prior

  if (!is.na(kvot))
  {
    #Vi bestämmer en tabell för frihetsgrader utifrån förhandsgissning om standardavvikelse
    ny<-1:50
    max.over.median<-sqrt(ny/qchisq(0.05,ny))
    
    ny_prior<-floor(approx(max.over.median,ny,kvot)$y) 
    if (kvot>16)
      ny_prior<-0
    if (kvot<1.2)
      ny_prior<-50
  }
  else
  {
    S_prior<-1
    ny_prior<-0
  }
  
  if (DNVplot) int<-"none"
  
  
  predS<-preds_low
  
  #Intervall för figurskalning
  
  if (any(is.na(Srange)))
  {
    srange_low<-min(S)
    srange_high<-max(S)
  }
  else
  {
    srange_low<-Srange[1]
    srange_high<-Srange[2]
  }
  
  
  data<-data.frame(S=S,N=N)
  origN<-length(N)
  
  if (int=="none")
  {
    no_int<-TRUE
    int<-"pred"
  }
  else no_int<-FALSE
  
  ##########################################################
  ##########################################################
  ##Preliminärskatta under antagande om att alla gått sönder
  p<-lm(log(N)~log(S),data)
  # ta fram standardavvikelsen för slumpfelen
  sigma0<-summary(p)$sigma
  
  
  ##########################################################
  ##########################################################
  #Ta med förhandsgissning avseende lutning med osäkerhet:
  #bestäm en vikt så att variansen blir samma som för slumpfelet
  len<-length(data$N)
  if (!any(is.na(betaint))) {
    # Priod knowledge on beta: uniform distrubution
    w <- sigma0/diff(range(betaint))*sqrt(3)*2 # "sigma0" / "std of uniform distribution"
    beta <- mean(betaint)     # Prior estimate = "mean of uniform distribution"
    
    #lägg till en logN-kolumn i data  för att undvika numeriska problem.
    #Vid alltför litet logN blir nämligen exp(logN)=0.
    
    data$logN <- log(data$N)
    #lägg till en rad i ekvationssystemet
    data[len+1,] <- c(0,0,0)
    data$S[len+1] <- exp(w)
    data$logN[len+1] <- -beta*w
    #samt en kolumn för konstanten
    data$dum <- c(rep(1,len),0)
    
    #Gör en ny skattning av linjen
    p<-lm(logN~dum+log(S)-1,data)
  }
  
  # ta fram standardavvikelsen för slumpfelen
  # Pool estimate with prior knowledge on sigma
  nyeq <- length(data$N)-2 #summary(p)$df
  sigma <- sqrt((ny_prior*S_prior^2+nyeq*sigma0^2)/(nyeq+ny_prior))
  
  out<-list("estimate"=c(p$coeff,sigma))

    
  # Vilket sigma skqa man använda vid förkunskap av beta???
  # sigma1 <- summary(p)$sigma
  # sigma1tot <- sqrt((ny_prior*S_prior^2+nyeq*sigma1^2)/(nyeq+ny_prior))
  # 
  # print(sigma0)
  # print(sigma)
  # print(sigma1)
  
  
  #######################################################
  ##kolla om någon är runout
  
  fail<-data$N>0
  nofail<-NA
  
  ##Om någon runout...
  
  #  if (!any(is.na(RO)) & any(RO=="RO"))   # Run-outs?
  if (any(RO=="RO"))   # Run-outs?
  {
    fail<-which(RO!="RO")
    nofail<-which(RO=="RO")
    
    
    ##########################################################
    ##Gör en ML-funktion att minimera
    f<-function(teta)
    {
      teta[3]<-exp(teta[3])#transformera sigma för att undvika negativa värden i minimeringsrutinen.
      a<-0
      b<-0
      a<--sum(log(dnorm(log(data$N[fail]),teta[1]+teta[2]*log(data$S[fail]),teta[3])))
      if (length(data$N)==origN+1) #Om förhandsgissning använts blir väntevärdet ett annat:
      {
        a<-a-log(dnorm(log(data$N[origN+1]),teta[2]*w,teta[3]))
      }
      a<-min(a,1e10)
      
      b<--sum(log(1-pnorm(log(data$N[nofail]),teta[1]+teta[2]*log(data$S[nofail]),teta[3])));
      b<-min(b,1e10) 
      
      a+b
    }#function
    ##########################################################
    
    ##Gör en ny skattning genom minimering
    out<-nlm(f,c(p$coeff[1:2],log(sigma)))
    
    coeff<-out$estimate[1:2]
    out$estimate[3]<-exp(out$estimate[3])
#    sigma<-out$estimate[3]
    sigma<-out$estimate[3]*origN/(origN-2) # Bias correction of std
    
    ##Justera antalet frihetsgrader enligt Lawless 
    z<-(log(data$N[nofail])-coeff[1]-coeff[2]*log(data$S[nofail]))/out$estimate[3]
    hz<-dnorm(z)/(1-pnorm(z))
    lambda<-hz*(hz-z)
    nyeq<-length(fail)-2+sum(lambda)
    ## bestäm en sammanvägd standardavvikelse baserad på förkunskap
    out$estimate[3]<-sqrt((ny_prior*S_prior^2+nyeq*sigma^2)/(nyeq+ny_prior))
    
  }#if     överlevare
  ##########################################################

  #Vi beräknar styrkan vid Nstrength (default: två miljoner cykler) för vår representation av Basquin
  Se=exp((log(Nstrength)-out$est[1])/out$est[2])
  
  
  ##########################################################
  ##Gör en referensvektor för plottning och prediktion.
  
  ref<-list("S"=seq(min(min(data$S[1:origN]),srange_low)/1.2,max(max(data$S[1:origN]),srange_high)*1.2,length.out=300))

  #Om man begärt en extrapolation för skattad utmattningsstyrka så måste skalan ändras
  if (!is.na(Nstrength))
  {
    #skatta styrkan ur modell
    Sstrength<-exp((-out$estimate[1]+log(Nstrength))/out$estimate[2])
    scale1<-exp(-2.5*out$estimate[3]/out$estimate[2])
    #Gör referensvektorn vidare för att få med prediktionsintervallet för extrapolation
    ref$S<-seq(min(Sstrength/scale1,min(ref$S)),max(max(ref$S,Sstrength*scale1)),length.out=300)
  }#if
  ##########################################################
  
  ##########################################################
  ##Beräkna rotuttrycket för konfidens- och prediktionsintervall
  
  rot_man<-0
  denom<-sum((log(data$S[1:len])-mean(log(data$S[1:len])))^2)+w^2
  for (i in 1:length(ref$S)) rot_man[i]<-sqrt(1+1/length(data$S)+(log(ref$S[i])-mean(log(data$S[1:len])))^2/denom)
  
  rot_ci <- sqrt(rot_man^2-1) 
  rot_pi <- rot_man
  
  # Beräkna konfidensintervall
  ci<-matrix(ncol=3,nrow=length(ref$S))
  ci[,1]<-out$estimate[1]+out$estimate[2]*log(ref$S)
  d_ci <- out$estimate[3]*rot_ci*qt(1-(1-conf.level)/2,nyeq+ny_prior)
  ci[,2]<-ci[,1]-d_ci
  ci[,3]<-ci[,1]+d_ci

  # Beräkna prediktionsintervall 
  pi <- ci
  d_pi <- out$estimate[3]*rot_pi*qt(1-(1-conf.level)/2,nyeq+ny_prior)
  pi[,2]<-pi[,1]-d_pi
  pi[,3]<-pi[,1]+d_pi
  
  # pi[,2]<-pi[,1]-out$estimate[3]*rot*qt(1-(1-conf.level)/2,nyeq+ny_prior)
  # pi[,3]<-pi[,1]+out$estimate[3]*rot*qt(1-(1-conf.level)/2,nyeq+ny_prior)
  
  
  ##########################################################
  ##Plotta det hela
  ##Bestäm först konfidens- eller prediktionsintervall för att få rätt skala
  
  # konfidens- eller prediktionsintervall (enligt val i input)
  if (int=="conf") # Confidence interval
    qi <- ci
  else             # Prediction interval
    qi <-pi  

  if (!no_int)  #Om ett intervall skall ritas....
  {
    limx<-c(min(qi[,2]),max(qi[,3]))
    limy<-sort(c(ref$S[which.min(qi[,2])],ref$S[which.max(qi[,3])]))
    
  }#if
  else
  {
    limx<-log(c(min(data$N[1:origN])*0.9,max(data$N[1:origN])/0.9))
    limy<-c(min(data$S[1:origN])*0.9,max(data$S[1:origN])/0.9)
  }#else
  
  #Om användaren angett skala så tar vi den istället
  if (any(!is.na(Nrange))) limx<-log(Nrange)
  if (any(!is.na(Srange))) limy<-Srange 
  

# plot(data$N[fail],data$S[fail],log="xy",pch=16,xlim=exp(limx),ylim=limy,lwd=2,xlab=xlabel,ylab=ylabel,main=title)
#  plot(NA,NA,log="xy",pch=16,xlim=exp(limx),ylim=limy,lwd=2,xlab=xlabel,ylab=ylabel,main=title)
  plot(NA, NA, log="xy", xlim=exp(limx), ylim=limy, xaxs="i", pch=16, lwd=2, xlab=xlabel, ylab=ylabel, main=title)
  
  #Om vi vill rita ut observationerna
  if(plottype == "p" | plottype == "b") {
    #plot(data$N[fail],data$S[fail],log="xy",pch=16,xlim=exp(limx),ylim=limy,lwd=2,xlab=xlabel,ylab=ylabel,main=title)
    points(data$N[fail],data$S[fail],pch=16)
    
    nn<-data$N[nofail]
    ss<-data$S[nofail]
    
    #Överlevare plottas speciellt, först de enstaka..
    points(nn[!duplicated(nn)],ss[!duplicated(nn)],pch=62,cex=1.3,col="blue")
    
    #sedan de som har samma RunOut-värde
    i=0
    while(any(duplicated(nn)))
    {
      i=i+1
      nn1<-nn[duplicated(nn)]
      ss<-ss[duplicated(nn)]
      nn<-nn1
      points(nn[!duplicated(nn)]*(1+i*2/100),ss[!duplicated(nn)],pch=62,cex=1.3)
    }#while
  }
  
  #Om vi vill rita ut linjer
  if(plottype == "l" | plottype == "b") {
    #Om vi inte vill ha konfidens- eler prediktionsintervall
    
    lines(exp(qi[,1]),ref$S,lwd=2,col="red")
    lines(c(Nrange[1],Nstrength,Nstrength),c(Se,Se,0.1*Srange[1]), col="black", lwd=2, lty="dotted")
    
    
    if (no_int)
    {
      #Om vi vill ha nedre två-sigmaintervall
      if(DNVplot)
        lines(exp(qi[,1]-2*out$estimate[3]),ref$S,lwd=2,lty="dashed",col="blue")
    }
    
    #annars plottar vi också dem med den typ och konfidensgrad som angetts
    else
    {
      if (int=="conf" | int=="both") # Confidence interval
      {
        lines(exp(ci[,2]),ref$S,lwd=2,col="green")
        lines(exp(ci[,3]),ref$S,lwd=2,lty="dashed",col="green")
      }
      if (int=="pred" | int=="both") # Prediction interval
        
      {
        lines(exp(pi[,2]),ref$S,lwd=2,col="blue")
        lines(exp(pi[,3]),ref$S,lwd=2,lty="dashed",col="blue")
      }
    }#else
    
    #################################################################
    ##################################################################
    ##Om vi vill ha predikterade livslängder 
    
    #kolla först om den begärda lastnivån är inom rimligt intervaall
    predindex=which(predS<max(ref$S) & predS>min(ref$S))
    
    predN<-matrix(ncol=2,nrow=length(predindex))
    
    #Om så
    if (length(predindex)>0)
    {
      #för varje önskad lastnivå beräknar vi predikterad livslängd med nedre  
      #konfidens- eller prediktionsgräns
      #vi gör det genom att ta inversen av de beräknade intervallen...
      #Vi bestämmer prediktionsintervall med den begärda konfidensgraden
      
      for (i in predindex)
      {
        index<-min(which(ref$S>=predS[i]))
        
        #...och ritar ut det hela
        lines(c(1,exp(pi[index,2])),predS[i]*c(1,1),col="black",lwd=1)
        lines(exp(pi[index,2])*c(1,1),c(1,predS[i]),col="black",lwd=1)
        lines(c(1,exp(pi[index,1])),predS[i]*c(1,1),col="black",lwd=1)
        lines(exp(pi[index,1])*c(1,1),c(1,predS[i]),col="black",lwd=1)
        
      }#for
    }#if
  }#if
  
  ####################################################################
  ####################################################################
  ##Om vi vill ha en styrka för en viss livslängd... Npred
  
  Strength<-c(1,1)	
  if (!is.na(Npred))
  {
    #Vi beräknar såväl medianen som nedre prediktionsgräns med interpolering
    Strength[1]<-exp(approx(qi[,1],log(ref$S),log(Npred))$y)
    Strength[2]<-exp(approx(qi[,2],log(ref$S),log(Npred))$y)
    Strength[3]<-exp(approx(qi[,3],log(ref$S),log(Npred))$y)
    StrengthDNV<-exp(approx(qi[,1]-2*out$estimate[3],log(ref$S),log(Npred))$y)
    #Om vi vill rita ut linjer
    if(plottype == "l" | plottype == "b") {
      #och ritar ut det hela
      lines(c(1,Npred),Strength[1]*c(1,1),col="black",lwd=2)
      if (DNVplot)
      {
        lines(c(1,Npred),StrengthDNV*c(1,1),col="black",lwd=2)
        lines(c(1,1)*Npred,c(par("yaxp")[1]/10,StrengthDNV),col="black",lwd=2)
      }
      else
      {
        lines(c(1,1)*Npred,c(par("yaxp")[1]/10,Strength[2]),col="black",lwd=2)
        lines(c(1,Npred),Strength[2]*c(1,1),col="black",lwd=2)
      }
      lines(c(1,1)*Npred,c(par("yaxp")[1]/10,Strength[1]),col="black",lwd=2)
      
    }#if
  }#if
  else {StrengthDNV<-NA}
  
  
  #Om vi vill ha en formel i figuren...
  if (formel) 
  {
    yfrac<-0.9
    if(Nstrength==2e6){
    text(exp(limx)*0.9,limy*yfrac, substitute(N==2 %.% 10^6 %.% (paste(Delta,S)/Se)^{beta},list(Se=signif(Se,3),beta=signif(out$estimate[2],2))),pos=2)
    }
    else if(Nstrength==1e5){
      text(exp(limx)*0.9,limy*yfrac, substitute(N==1 %.% 10^5 %.% (paste(Delta,S)/Se)^{beta},list(Se=signif(Se,3),beta=signif(out$estimate[2],2))),pos=2)
    }
    else {
      text(exp(limx)*0.9,limy*yfrac, substitute(N==Nstrength %.% (paste(Delta,S)/Se)^{beta},list(Nstrength=Nstrength,Se=signif(Se,3),beta=signif(out$estimate[2],2))),pos=2)
      
    }
  }
  
  grid()
  mtext("RISE Fatigue Design Tool",3,0,font=21,cex=0.7,col="blue",adj=1)
  #bild<-recordPlot()
  
  ##Vi beräknar prediktionsinterval för 95% konfidensgrad
  
  pi95<-matrix(ncol=3,nrow=length(ref$S))
  pi95[,1]<-out$estimate[1]+out$estimate[2]*log(ref$S)
  pi95[,2]<-pi95[,1]-out$estimate[3]*rot_man*qt(0.975,nyeq+ny_prior)
  pi95[,3]<-pi95[,1]+out$estimate[3]*rot_man*qt(0.975,nyeq+ny_prior)
  
  #Vi beräknar prediktionsgränser med interpolering ... för Npred
  Strength95<-0
  Strength95[1]<-exp(approx(pi95[,1],log(ref$S),log(Npred))$y)  # median
  Strength95[2]<-exp(approx(pi95[,2],log(ref$S),log(Npred))$y)  # lower pred.lim.

  
  ##Vi beräknar konfidensinterval för 95% konfidensgrad
  
  ci95<-matrix(ncol=3,nrow=length(ref$S))
  ci95[,1]<-out$estimate[1]+out$estimate[2]*log(ref$S)
  rot<-sqrt(rot_man^2-1)
  ci95[,2]<-ci95[,1]-out$estimate[3]*rot*qt(0.975,nyeq+ny_prior)
  ci95[,3]<-ci95[,1]+out$estimate[3]*rot*qt(0.975,nyeq+ny_prior)
  
  #Vi beräknar 95% konfidensintervall för FAT-value, Se (med interpolering)
  ciFAT<-0
  ciFAT[1]<-exp(approx(ci95[,2],log(ref$S),log(Nstrength))$y)
  ciFAT[2]<-exp(approx(ci95[,3],log(ref$S),log(Nstrength))$y)
  
  #Alla inressanta resultat bestäms och läggs i en vektor
  
  result.Se<-signif(Se,3)
  result.k<-signif(-out$estimate[2],3)
  result.k.pm <- out$estimate[3]/sqrt(denom)*qt(0.975,nyeq+ny_prior)
  result.k.int<-signif(-out$estimate[2]+result.k.pm*c(-1,1),3)
#  result.Strength.int<-signif(rev(Strength),3)
  result.Strength.ci<-signif(ciFAT,3)
  result.Strength.int<-signif(Strength[c(2:3,1)],3)
  result.Strength.s<-signif(-diff(log(Strength95))/1.96,2)
  result.limxtemp<-signif(exp(limx),2)
  result.limytemp<-signif(limy,2)
  result.COV<-signif(out$estimate[3],2)
  

  result<-list("Se"=result.Se, "k"=result.k, "s"=result.COV, "kint"=result.k.int, "result.k.pm"=result.k.pm, 
                 "strength.ci"=result.Strength.ci, "strength.int"=result.Strength.int, "tau"=result.Strength.s,
                 "loglimx"=result.limxtemp,"loglimy"=result.limytemp,
                 "COV"=result.COV,
                 "DNVstrength"=signif(StrengthDNV,3))
  
  #dummy command to avoid rubbish on screen
  result
  
}