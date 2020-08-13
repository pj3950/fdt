# Load Analysis toolbox
#
# Pär Johannesson, 15-Sep-2011, 22-May-2015, 17-Aug-2018
#
# ts2tp           Time signal to Turning Points (optional rainflow filter)
# tp2arfc4p       Calculates asymmetric rainflow cycles from turning points (4-point)
# tp2rfc          Calculates  rainflow cycles from turning points (incl. residual)

# -------------------------------------------------------------

# =============================================================
# ts2tp           Time signal to Turning Points (optionally rainflow filtered).
#
# CALL:  [y] <- rfcfilter(x,h,def);
#
# Input:
#   x   = Signal.   [nx1] OR [nx2]
#   h   = Threshold for rainflow filter.
#   def = 0: removes cycles with range < h. (default)
#         1: removes cycles with range <= h. (default only if h=0)
# Output:
#   y   = Rainflow filtered signal.
#
# Examples:  # 1. Filtered signal y is the turning points of x.
#      tmp<-read.table(datafil,header=FALSE,stringsAsFactors=FALSE,fill=TRUE)
#      y <- ts2tp(x);
#            # 2. This removes all rainflow cycles with range less than 0.5.
#      y <- ts2tp(x,0.5);
#

ts2tp <- function(x,h=0,def=0)
{
  
  if(h==0) {
    def <- 1
  }
  
  # Initiation
  
  if(is.data.frame(x)) {
    x <- as.matrix(x)                        #  Convert to matrix
  }
  
  if(is.matrix(x)) {
    n <- nrow(x)                        #  Number of turning points
  }
  else if(is.numeric(x)) {
    n <- length(x)                                  #  Number of turning points
    x <- matrix(c(1:n,x),n,2)
  }
  else {
    #ERROR
  }
  
  Ind <- rep(0,n)
  
  j <- 0
  t0 <- 1
  y0 <- x[1,2]
  z0 <- 0
  Ind[1] <- 1
  
  tmin <- 1
  ymin <- y0
  tmax <- 1
  ymax <- y0
  
  # The rainflow filter
  
  for(k in 2:n) {
    fpk <- y0+h
    fmk <- y0-h
    tk <- k
    xk <- x[k,2]
    
    if(z0 == 0) {
      if(xk>ymax) {tmax <- tk; ymax <- xk }
      if(xk<ymin) {tmin <- tk; ymin <- xk }
      if(def == 0) {
        test <- (ymax-ymin>=h)
      }
      else { # def == 1
        test <- (ymax-ymin>h)
      }
      
      if(test) {
        if(tmin<tmax) {
          t0 <- tmin; y0 <- tmin
          z1 <- +1; t1 <- tmax; y1 <- ymax
        }
        else {
          t0 <- tmax; y0 <- ymax
          z1 <- -1; t1 <- tmin; y1 <- ymin
        }
      }
      else {
        z1 <- 0; t1 <- tk; y1 <- xk
      }
    }
    else { # z0 != 0
      # Which definition?
      if(def == 0) {
        test1 <- (((z0==+1) & (xk<=fmk)) | ((z0==-1) & (xk<fpk)))
        test2 <- (((z0==+1) & (xk>fmk))  | ((z0==-1) & (xk>=fpk)))
      }
      else { # def == 1
        test1 <- (((z0==+1) & (xk<fmk))  | ((z0==-1) & (xk<=fpk)))
        test2 <- (((z0==+1) & (xk>=fmk)) | ((z0==-1) & (xk>fpk)))
      }
      
      # Update z1
      if(test1)       { z1 <- -1 }
      else if(test2)  { z1 <- +1 }
      else            {  } # warning(['Something wrong, i=' num2str(i)]);
      
      # Update y1
      if(z1 != z0) {
        t1 <- tk
        y1 <- xk
      }
      else if(z1 == -1) {
        # % y1 = min([y0 xi]);
        if(y0 < xk) {
          t1 <- t0
          y1 <- y0
        }
        else {
          t1 <- tk
          y1 <- xk
        }
      }
      else if(z1 == +1) {
        # % y1 = max([y0 xi]);
        if(y0 > xk) {
          t1 <- t0
          y1 <- y0
        }
        else {
          t1 <- tk
          y1 <- xk
        }
      }
    }
    
    # Update y if y0 is a turning point
    #if(abs(z0-z1) == 2) {
    if(abs(z0-z1) > 0) {
      j <- j+1
      Ind[j] <- t0
    }
    
    # Update t0, y0, z0
    t0 <- t1
    y0 <- y1
    z0 <- z1
  }
  
  # Update y if last y0 is greater than (or equal) threshold
  if(z0 == 0) { # All ranges below threshold
    j <- 1  # return the first value
  }
  else {
    if(def == 0)  { test <- (abs(y0-x[Ind[j],2]) >= h) }
    else          { test <- (abs(y0-x[Ind[j],2]) > h) } # def == 1
    if(test) {
      j <- j+1
      Ind[j] <- t0
    }
  }
  # Truncate Ind
  Ind <- Ind[1:j]
  y <- matrix(x[Ind,],ncol=2)
  
  #res <- list(y=y,Ind=Ind)
  y
}
# END: ts2tp
# =============================================================


# =============================================================
# tp2arfc4p        Calculates asymmetric rainflow cycles from turning points (4-point)
#function [ARFC,res] = tp2arfc4p(x,res0,def_time)
#%TP2ARFC4P Calculates asymmetric rainflow cycles from turning points (4-point).
#%
#% CALL:  [ARFC,res] = tp2arfc4p(tp)
#%        [ARFC,res] = tp2arfc4p(tp,res0,def_time)
#%
#% Output:
#% ARFC  = Asymmetric RFC (without residual).       [N,2]/[N,4]
#% res   = Residual.                               [nres,1]/[nres,2]
#%
#% Input:
#% tp       = Turning points.                         [T,1]/[T,2]
#% res0     = Residual.                               [nres0,1]/[nres0,1]
#% def_time = 0: Don't store time of max and min. (default)
#%            1: Store the time when the maxima and minima occured.
#%
#% Calculates the rainflow cycles for the sequence of turning points,
#% by using the so-called 4-point algorithm.
#%
#% Calculate ARFC for turning points, starting from old residual 'res0'
#%   [ARFC,res] = tp2arfc4p(tp,res0)
#%
#% This routine doesn't use MEX, Fortran or C codes, only matlab code.
#%
#% Example:
#%   x = load('sea.dat'); tp=dat2tp(x);
#%   [ARFC,res]=tp2arfc4p(tp);      % Default (min-to-Max cycles in residual)
#%   ccplot(ARFC), res
#%

tp2arfc4p <- function(tp)
{
  if(is.matrix(tp)) { tp <- tp[,2] }
  
  T <- length(tp)                   #  Number of turrning points
  RFC <- matrix(NaN,floor(T/2),2)   # Rainflow cycles
  N <- 0                            # Number of counted cycles
  
  res <- tp   # Residual
  nres <- 0   # Number of points in residual
  
  # Calculate ARFC and res
  for(k in 1:T) {
    nres <- nres+1
    res[nres] <- tp[k]
    cycleFound <- TRUE
    while(cycleFound & nres >=4) {
      A <- sort(res[(nres-2):(nres-1)])
      B <- sort(res[c((nres-3),nres)])
      if(A[1]>=B[1] & A[2]<=B[2]) {
        N <- N+1
        RFC[N,] <- res[(nres-2):(nres-1)]
        res[nres-2] <- res[nres]
        nres <- nres-2
      }
      else  {
        cycleFound <- FALSE
      }
    }
  }
  
  # Counted rainflow cycles & Residual
  
  #RFC <- RFC[1:N,]
  #if(N==0)    { RFC <- NULL } else  {RFC <- matrix(RFC[1:N,],ncol=2) }
  #if(nres==0) { res <- NULL } else  {res <- res[1:nres]}
  RFC <- matrix(RFC[1:N,],ncol=2)
  res <- res[1:nres]
  
  out <- list(RFC=RFC, res=res)
  
}
# END: tp2rfc4p
#=============================================================


#=============================================================
# tp2rfc          Calculates  rainflow cycles from turning points (incl. residual)
tp2rfc <- function(tp) { # Calculate ARFC0 and res
  
  R <- tp2arfc4p(tp)
  
  res2 <- ts2tp(c(R$res,R$res))  # TP of double residual
  Rres <- tp2arfc4p(res2)
  
  if(is.na(R$RFC[1,1]))    { n <- 0 }    else  { n <- nrow(R$RFC) }
  if(is.na(Rres$RFC[1,1])) { nres <- 0 } else  { nres = nrow(Rres$RFC) }
  if(n+nres==0)       {RFC <- R$RFC}  else { RFC <- matrix(ncol=2, nrow=n+nres) }
  if(n!=0)            { RFC[1:n,] <- R$RFC }
  if(nres!=0)         { RFC[(n+1):(n+nres),] <- Rres$RFC }
  
  RFC
}
# END: tp2rfc
#=============================================================


#=============================================================
# Extract ranges from rainflow cycles
rfc2ls <- function(RFC, new=FALSE, log="x", title="Load spectrum", xlab="Cumulative number of cycles", ylab="Load range",N=1)
{
  
  rfc.rang<-data.frame("S"=sort(abs(RFC[,2]-RFC[,1]),decreasing=TRUE), "n"=rep(1,times=length(RFC[,1])))
  rfc.rang<-rbind(rfc.rang,c(0,N))
  #  rfc.rang<-rbind(rfc.rang,c(N,0))
  if (new)
    lines((cumsum(rfc.rang$n)),sort(rfc.rang$S,decreasing=TRUE),type="S",lwd=2,col="red")
  else
  {
    plot(cumsum(rfc.rang$n),sort(rfc.rang$S,decreasing=TRUE), type="S", log=log, lwd=2, col="blue",
         main=title, xlab=xlab, ylab=ylab)
    grid()
  }
  
  return(rfc.rang)
}
# END: rfc2ls
#=============================================================

#=============================================================
#Find level crossings from rain flow cycles
rfc2lc <- function(RFC, n=200, new=FALSE, log="x", title="Level crossings", xlab="Number of up-crossings", ylab="Load level")
{
  
  rfc<-RFC
  
  for (i in 1:length(RFC[,1]))
    rfc[i,]<-sort(RFC[i,])
  
  levels<-seq(min(rfc[,1]),max(rfc[,2]),len=n)
  
  lc<-numeric()
  for (j in 1:n)
    lc[j]<-sum((rfc[,2]>levels[j] )& (rfc[,1]<=levels[j]))
  if(new)
    #lines(levels,lc,type="S",lwd=2,col="red")
    lines(lc,levels,type="S",lwd=2, col="red")
  else
  {
    #plot(levels,lc,type="S",lwd=2,ylab="up-crossings",main=title,col="blue")
    plot(lc,  levels, type="S", log=log, lwd=2, col="blue", main=title, xlab="Number of up-crossings", ylab="Load level")
    grid()
  }
  
  data.frame("S"=levels, "Nup"=lc)
}
