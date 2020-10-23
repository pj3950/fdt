read.snva.data <- function(grundfil)
  
{
  
  #L?s in provresultat och spektra
  provresultat<-read.table(grundfil,header=FALSE,stringsAsFactors=FALSE,fill=TRUE)
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
  
  dat <- list(provresultat=provresultat, spectra=spectra, N=N, RO=RO)
}

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
  N <- dat$N
  RO <- dat$RO
  spectra <- dat$spectra
  
  if(is.na(RO)) {RO <- rep('F',length(N))}
  
  #Vi g?r nu en iteration av konstantamplitudskattningar d?r den skattade lutningen anv?nds f?r att
  #best?mma ekvivalentlaster i n?sta iteration.
  beta <- 3
  beta1 <- 3.5
  n.of.iter<-0
  while(abs((beta-beta1)/beta1)>1e-4 & n.of.iter<10)
  {
    beta<-beta1
    Seq<-0
    for (i in 1:length(spectra))
    {
      Seq[i] <- as.numeric((spectra[[i]]$n%*%spectra[[i]]$S^(beta)/sum(spectra[[i]]$n))^(1/(beta)))
    }
    para<-SNw(S=Seq,N=N,RO=RO,...)
    
    beta1 <- para$k #-para[[1]][2]
    n.of.iter <- n.of.iter+1
    
  }#while
  
  para$n.iter <- n.of.iter
  
  #Resultaten skrivs till en fil
  
  
  # if (is.na(name_R))
  #   utfil<-"resultat/test.csv"
  # else
  #   utfil<-paste("resultat/",strsplit(name_R,".",fixed=TRUE)[[1]][1],".csv",sep="")
  # write.csv(para[[1]],file=utfil,row.names=FALSE)
  # 
  # return(NULL)
  
  return(para)
  
}#function