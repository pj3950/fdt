# ========================================================
# Example: Load Analysis Toolbox
#
# Pär Johannesson, 2018-08-17
#========================================================


#========================================================
# ==== Initiate   ====

rm(list=ls())


#source('ToolboxLIFEMOD.R')
#source('SNwPJ_v2.R')
#source('toolboxFatLim.R')
source('../LoadAnalysisTools.R')

#========================================================
# ==== Read data   ====



Fname<-"load_short.txt"
Fname<-"load.txt"

dat0 <- read.table(Fname, sep="",dec=".",header=FALSE, skip=0, as.is=TRUE)
names(dat0) <- c("t", "L")

dat0

TS <- dat0

#========================================================
# Load Analysis

b=5
h=NA

TP0 <- ts2tp(TS,0)
RFC0 <- tp2rfc(TP0)
D0 <- sum(abs(RFC0[,1]-RFC0[,2])^b)
N0 <- dim(RFC0)[1]
rangeTP <- diff(range(TP0[,2]))

if(is.na(h)) {
  h <- 0.1*rangeTP
}

TP <- ts2tp(TP0,h)
  RFC <- tp2rfc(TP)
  D <- sum(abs(RFC[,1]-RFC[,2])^b)
  if(is.na(D)) {D <- 0 }
  N <- dim(RFC)[1]

plot(TS[,1],TS[,2], type="l", col="blue", main="Rainflow filtered turning points",xlab="Time",ylab="Load")
lines(TP[,1],TP[,2], col="red")
points(TP[,1],TP[,2], col="red", pch=20)


rfc2ls(RFC0) #,title=title,ylab=ylabel)
ranges<-rfc2ls(RFC,new=TRUE,N=N0-N)

rfc2lc(RFC0) #,title=title)
rfc2lc(RFC,new=TRUE)

res <- list(TP = TP)
res$TP0 <- TP0
res$Drel <- D/D0
res$Nacc <- N0/N
res$RFC<-signif(ranges,3)
res$para <- c(h, b, N0, N, res$Nacc, res$Drel)


# save TP
file="TP-RainflowFiltered"
write.table(data.frame(time=RF$TP[,1], value=RF$TP[,2]), file, sep=";", row.names = FALSE)



# --------------------------------------------------------
# Example: analyse one Group

# fatlim.est <- function(data, RO=2e6, conf.level=0.95, int="conf",
#                        plot=TRUE, title="Stair case test result with 95% confidence limits",
#                        xlab="specimen number", ylab="load level", ylim=NULL)
# int		  =  "none" / "conf"
# conf.level    = Confidence level
# plot          = Plot if >0 (1: Stair case plot, 2: Also plot Profile likelohood)
# title, xlab, ylab, ylim = Plot parameters

fatlim.est(dat)

# Test input: int
fatlim.est(dat, int="none")
fatlim.est(dat, int="annat")

# Test input: conf.level
fatlim.est(dat, conf.level=0.9)
fatlim.est(dat, conf.level=0.99,plot=2)
fatlim.est(dat, conf.level=0.999, plot=2)

# Test Plot options
fatlim.est(dat, plot=0)
fatlim.est(dat, plot=1)
fatlim.est(dat,plot=2)

# Test set RO till 2e6
#dat$RO[dat$N>=2e6] <- 'RO'
#dat$N[dat$N>=2e6] <- 2e6

# Test: Set 
#dat$N[dat$RO=='RO'] <- 2e6


# --------------------------------------------------------
# Problem: SNw och survreg gav olika skattning av Wöhlerkurvan.
#
# Orsak: Om man inte angav någon förkunskap om lutningen, 
# så satte SNw intervallet till [0,30] och använde detta som förkunskap
# och la till en extra kolumn i regressionen.
#
# Lösning: Jag ändrade i koden så att förkunskap bara används om man angivit ett intervall, 
# i annat fall görs inet (dvs ingen extra kolumn läggs till).
#
# Detta innebär att nu ger SNw och survreg gav olika skattning av Wöhlerkurvan 
# (möjligen någon skillnad i skattning av std och konf/pred-intervall).
# --------------------------------------------------------

# --------------------------------------------------------
# SP fatigue tool

res1<-SNw(dat$S,dat$N,dat$RO, 
         title=group,ylabel="ΔS, Stress range / MPa",xlabel="N, number of cycles to failure")

res1

# --------------------------------------------------------
# PJ - Re-implementation using survreg

res2<-SN.est(dat, 
         title=group,ylabel="ΔS, Stress range / MPa",xlabel="N, number of cycles to failure")

res2

# --------------------------------------------------------
# Survival regression

dat$fail <- dat$RO != "RO"

M0 <- survreg(Surv(log(N),fail)~log(S), data=dat, dist='gaussian')
M <- survreg(Surv(log(N),RO!='RO')~log(S), data=dat, dist='gaussian')

M
summary(M)
coef(M)
confint(M)
sd(residuals(M))
sum(resid(M)^2)/(length(dat$N)-2)

b <- -coef(M)[2]
a0 <- coef(M)[c(1)]
names(a0) <- unique(dat$group)
da<-a0
da[1]<-0
a<-a0[1]+da

N0 <- 2e6
FAT <- 1/(N0^(1/b))*exp(a/b)
FAT


resSurv = data.frame(b=b, FAT=FAT, s=M$scale)


# Plot SN-curves

SS <- seq(250,800,5)
NN <- N0*(SS/FAT)^(-b)
lines(NN,SS,lwd=2,col="red")

res 
resSurv

# compare with lm
summary(lm(log(N)~log(S), data=dat))
res
summary(M)

#Resultat:
# SNw och survreg ger nu samma Wöhler (när vi inte har med förkunskap om lutningen).
# Skattningen av spridningen, s, skiljer sig åt något.
# Jag tror att 'survreg' skattar alla parametrar med ML, även log(scale), därav skillnaden.

# --------------------------------------------------------
# Testa med förkunskap om lutning

dbeta=10  #Välj t ex 0, 1, 10, 100 och NA
res2<-SNw(dat$S,dat$N,dat$RO, beta_low=6-dbeta, beta_high=6+dbeta,
         title=group,ylabel="ΔS, Stress range / MPa",xlabel="N, number of cycles to failure")

lines(NN,SS,lwd=2,col="red")

res2 
resSurv

#Resultat:
# dbeta=1   ger k=6.2
# dbeta=10  ger k=6.5
# dbeta=1e6 ger k=6.5 (beror ej på mittpunkten av gissningen om lutningen)
# dbeta=NA  ger k=8.6 (NA innebär ingen förkunskap om lutningen)
#
# Problemet verkar bara uppstå när det finns överlevare.






#========================================================
# Survival regression
# - One group
# - All RT

I <- which(data$group==group)
dat <- data[I,]
dat$fail <- dat$RO != "RO"
dat$group <- as.factor(dat$group)
unique(dat$group)

M <- survreg(Surv(log(N),fail)~log(S), data=dat, dist='gaussian')

M
summary(M)
coef(M)
confint(M)
anova(M)
sd(residuals(M))

b <- -coef(M)[2]
a0 <- coef(M)[c(1)]
names(a0) <- unique(dat$group)
da<-a0
da[1]<-0
a<-a0[1]+da

N0 <- 2e6
FAT <- 1/(N0^(1/b))*exp(a/b)
FAT


# --------------------------------------------------------
# Combined SN-curve for all specimen in RT / ET
# Survreg

# Idendity data from group k
k=0  # RT
if(k>0){I <- which(data$temperature == Tgroups[k] & data$batch==batch)}
if(k==0) {I <- which(data$batch==batch)}
dat <- data[I,]
#dat <- dat[dat$orientation=='perpendicular',]
dat$fail <- dat$RO != "RO"
dat$group <- as.factor(dat$group)
unique(dat$group)

#Sdat <- Surv(log(dat$N), dat$RO != "RO")

M <- survreg(Surv(log(N),fail)~log(S)+group, data=dat, dist='gaussian')

summary(M)
coef(M)
confint(M)
anova(M)
sd(residuals(M))

n<-length(coef(M))
b <- -coef(M)[2]
a0 <- coef(M)[c(1,3:n)]
names(a0) <- unique(dat$group)
da<-a0
da[1]<-0
a<-a0[1]+da

N0 <- 2e6
FAT <- 1/(N0^(1/b))*exp(a/b)

# Alternatively, use lognormal distribution

Mlog <- survreg(Surv(N,fail)~log(S)+group, data=dat, dist='lognormal')


# --------------------------------------------------------
# Plot for presentation

# Plot all observations as points
Title <- paste("Batch ",batch,", Temperature: ",Tgroups[k],sep="")
plot(dat$N,dat$S,pch=as.numeric(factor(dat$group)),log="xy",lwd=2, 
     cex=1, cex.lab=1.2, cex.axis=1.2, 
     ylim=c(250,800), xlim=c(1e4,6e6),
     main=Title,ylab="ΔS, Stress range / MPa",xlab="N, Number of cycles to failure")

# Plot runouts
Irunout <- which(dat$RO == "RO")
nn<-dat$N[Irunout]
ss<-dat$S[Irunout]
points(nn,ss,pch=62,cex=1.3,col="blue")

# Plot SN-curves

SS <- seq(250,800,5)
for(kk in 1:length(unique(dat$group))) {
  NN <- N0*(SS/FAT[kk])^(-b)
  lines(NN,SS,lwd=2)
}

legend("bottomleft",legend=unique(dat$group),pch=as.numeric(factor(unique(dat$group))), cex=1.2)







# Weld volume
# Concerning the weld volume, see the attached Excel file with all the geometry measurement data for Batch 1. 
# I marked the average width values (top and bottom) red for the thick and thin specimens, which clearly differ for 
# the thin and thick specimens. You can use these average values, or even better the specimen values from the linescan data 
# (the values in the corresponding columns), to approximate the weld volume (wt+wb)*t/2*W.

# Weld width top
wt <- c(4.87,6.80) # t = 2.54 & 7.1
# Weld width bottom
wb <- c(3.34,4.47) # t = 2.54 & 7.1


names(FAT)

Vol <- FAT
for(kk in names(FAT)) {
  ii <- which(dat$group==kk)[1]
  tt <- as.numeric(dat$thickness[ii])
  W <- as.numeric(dat$width[ii])
  ifelse(tt==2.54,jj<-1,jj<-2)
  Vol[kk] <- (wt[jj]+wb[jj])*tt/2*W
}

plot(Vol,FAT)
plot(Vol,FAT,xlog=TRUE)
plot(Vol,FAT,log="xy")

# --------------------------------------------------------
# Covariance analysis - Combined SN-curve for all specimen in both RT and ET
# Survreg with effects:
# - thickness
# - width
# - temperature
# - orientation
# - weld volume (estiamted using thichness, width and weld size)

# Choose data: RT, ET or all
k=1
I <- which(data$temperature == Tgroups[k] & data$batch==batch)
#I <- which(data$temperature == 'ET' & data$batch==batch)
I <- which(data$batch==batch)

# Prepare data
dat <- data[I,]
#dat <- dat[dat$orientation=='perpendicular',]
dat$fail <- dat$RO != "RO"
dat$group <- as.factor(dat$group)
dat$orientation <- as.factor(dat$orientation)
dat$thickness <- as.factor(dat$thickness)
dat$width <- as.factor(dat$width)
dat$temperature <- as.factor(dat$temperature)


#Sdat <- Surv(log(dat$N), dat$RO != "RO")

#M <- survreg(Surv(log(N),fail)~log(S)+orientation+thickness+width, data=dat, dist='gaussian')
M <- survreg(Surv(log(N),fail)~log(S)+thickness+width+temperature, data=dat, dist='gaussian')

summary(M)
coef(M)
confint(M)
anova(M)
sd(residuals(M))

b <- -coef(M)[2]
a0 <- coef(M)[c(1,3:5)]
#names(a0) <- unique(dat$group)
da<-a0
da[1]<-0
a<-a0[1]+da

N0 <- 2e6
FAT <- 1/(N0^(1/b))*exp(a/b)






