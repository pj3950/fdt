library(xlsx)  # Importing Excel-files


read.sn.data <- function(Fname, format='txt')
{
  # Classic Fatigue Design Tool format  
  if(format == 'txt')
  {
    #L?s in SN-data
    dat <- read.table(Fname, sep="", dec=".", header=FALSE, skip=0, as.is=TRUE, fill=TRUE)
    #Kolla om vi har en tredje kolumn, om inte skapa en sådan.
    if(ncol(dat) < 3)
      dat[,3]="F"
    colnames(dat) <- c( "S", "N", "fail")
  }
  
  # Excel format of data
  if(format == 'xlsx')
  {
    #L?s in SN-data
    dat <- read.xlsx(Fname, 1)  # read first sheet
    if(ncol(dat) < 3)
      dat[,3]="F"
    colnames(dat) <- c( "S", "N", "fail")
  }

  dat
}

read.snva.data <- function(Fname, format='xlsx')
{
  # Classic Fatigue Design Tool format  
  if(format == 'txt')
  {
    #L?s in provresultat och spektra
    provresultat<-read.table(Fname,header=FALSE,stringsAsFactors=FALSE,fill=TRUE)
    #Skapa en vektor f?r run out
    if (length(provresultat)>3) { RO<-provresultat$V4 } else   RO<-NA
    provresultat$skala<-as.numeric(provresultat$V2)
    provresultat$Nbrott<-as.numeric(provresultat$V3)
    provresultat$spektrum<-provresultat$V1
    spectra<-list(list())
    N<-numeric()
    for (i in 1:length(provresultat$spektrum))
    {
      spectra[[i]]<-read.table(paste(provresultat$spektrum[i],".txt",sep=""),col.names=c("S","n"),header=FALSE, stringsAsFactors=FALSE)
      spectra[[i]]$S<-spectra[[i]]$S*provresultat$skala[i]
      N[i]<-provresultat$Nbrott[i]
    }
    
    dat <- list(provresultat=provresultat, spectra=spectra, N=N, fail=RO)
  }
  
  # Excel format of data
  if(format == 'xlsx')
  {
    #L?s in provresultat och spektra
    X <- read.xlsx(Fname, 1)  # read first sheet
    if(ncol(X) < 4)
    {
      X[,4]="F"
    }
    colnames(X) <- c("spectrum", "scale", "N", "fail")
    
    Snames <- unique(X[,1])
    SPECT = list()
    for(i in 1:length(Snames))
    {
      SPECT[[i]] <- read.xlsx(Fname, sheetName=Snames[i])  # read first sheet
      colnames(SPECT[[i]]) <- c("S", "n")
    }

    #Skapa en vektor f?r run out
    spectra<-list(list())
    for (i in 1:length(X$spectrum))
    {
      spectra[[i]] <- SPECT[[which(Snames==X$spectrum[i])]]
      spectra[[i]]$S <- spectra[[i]]$S*X$scale[i]
    }
    
    dat <- list(SNVA.data=X, spectra=spectra, N=X$N, fail=X$fail)
  }
  
  
  dat
}

#========================================================
# Implementation by Thomas Svensson

SNVA_TS<-function(dat,...)
  #skattar W?hlerkurvan fr?n spektrumresultat inklusive ?verlevare
  #med hj?lp av en enkel iterativ procedur. N avser antalet cykler till brott f?r hela 
  #spektrat.
  #
  #Kr?ver tillg?ng till funktionen SNw.
  #... betyder att man kan anger valfria inputvariabler f?r funktionen SNw
  #
  #grundfilen ?r ett textdokument med tre kolumner, den f?rsta anger spektrumnamn
  #den andra en skalfaktor
  #den tredje antalet cykler till brott.
  #spektrumnamnet kompletteras i denna funktion med ?ndelsen .txt f?r inl?sning av spektrum.
#resultatet skrivs till fil.

{
  spectra <- dat$spectra
  N <- dat$N
  RO <- dat$fail
  
  if(is.na(RO)) {RO <- rep('F',length(N))}
  
  #Vi g?r nu en iteration av konstantamplitudskattningar d?r den skattade lutningen anv?nds f?r att
  #best?mma ekvivalentlaster i n?sta iteration.
  beta <- 3
  beta1 <- 3.5
  beta1 <- 6
  n.of.iter<-0
  while(abs((beta-beta1)/beta1)>1e-4 & n.of.iter<10)
  {
    beta<-beta1
    Seq<-0
    for (i in 1:length(spectra))
    {
      Seq[i] <- as.numeric((spectra[[i]]$n%*%spectra[[i]]$S^(beta)/sum(spectra[[i]]$n))^(1/(beta)))
    }
#    para<-SNw(S=Seq,N=N,RO=RO,...)
    para<-SNw(S=Seq,N=N,RO=RO)
    
    beta1 <- para$k #-para[[1]][2]
    n.of.iter <- n.of.iter+1
    
  }#while
  
  para$n.iter <- n.of.iter
  

  return(para)
  
}#function



#========================================================
# Estimate SNVA-curve using fix-point interation
# Implementation by Pär Johannesson based on TS implementation in RISE Fatigue Design Tool

SNVA<-function(dat,...)
  #skattar W?hlerkurvan fr?n spektrumresultat inklusive ?verlevare
  #med hj?lp av en enkel iterativ procedur. N avser antalet cykler till brott f?r hela 
  #spektrat.
  #
  #Kr?ver tillg?ng till funktionen SNw.
  #... betyder att man kan anger valfria inputvariabler f?r funktionen SNw
  #
  #grundfilen ?r ett textdokument med tre kolumner, den f?rsta anger spektrumnamn
  #den andra en skalfaktor
  #den tredje antalet cykler till brott.
  #spektrumnamnet kompletteras i denna funktion med ?ndelsen .txt f?r inl?sning av spektrum.
#resultatet skrivs till fil.

{
  spectra <- dat$spectra
  N <- dat$N
  RO <- dat$RO
  
  n <- length(N)
  if(is.na(RO)) {RO <- rep('F',n)}
  
  #Vi g?r nu en iteration av konstantamplitudskattningar d?r den skattade lutningen anv?nds f?r att
  #best?mma ekvivalentlaster i n?sta iteration.
  
  beta0 <- c(0.1, 1:8, 10, 15, 20, 50, 100)
  n.beta0 <- length(beta0)
  Q0 <- NA
  
  for(k in 1:n.beta0)
  {
    beta<-beta0[k]
    Seq<-0
    for (i in 1:n)
    {
      Seq[i] <- as.numeric((spectra[[i]]$n%*%spectra[[i]]$S^(beta)/sum(spectra[[i]]$n))^(1/(beta)))
    }
    Q0[k] <- SNw(S=Seq,N=N,RO=RO)$Q
#    L[k] <- SNw(S=Seq,N=N,RO=RO,...)$Lmin
    #    para<-SNw(S=Seq,N=N,RO=RO,...)
  }
  
  k0 <- which.min(Q0)
  k1 <- k0 + which.min(Q0[c(k0-1,k0+1)])*2-3
  
  b.int <- beta0[c(k0,k1)]
  Q.int <- Q0[c(k0,k1)]
  
  Q<-0
  b <- 0
  n.of.iter<-0
  while(abs(diff(b.int))>1e-3 & n.of.iter<10)
  {
    
    n.of.iter <- n.of.iter+1
    beta<-mean(b.int)
    Seq<-0
    for (i in 1:n)
    {
      Seq[i] <- as.numeric((spectra[[i]]$n%*%spectra[[i]]$S^(beta)/sum(spectra[[i]]$n))^(1/(beta)))
    }
    Q[n.of.iter]<-SNw(S=Seq,N=N,RO=RO)$Q
#    para<-SNw(S=Seq,N=N,RO=RO,...)
    b[n.of.iter] <- beta
    
    kk <- which.min(Q.int)
    Q.int <- c(Q.int[kk], Q[n.of.iter])
    b.int <- c(b.int[kk], beta)
    
  }#while
  
  para<-SNw(S=Seq,N=N,RO=RO)
  
  para$n.iter <- n.of.iter
  
  return(para)
  
}#function