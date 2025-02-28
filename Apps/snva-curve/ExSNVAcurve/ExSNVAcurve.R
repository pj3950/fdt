# ========================================================
# Example: Estimate SN-curve
# - Estimate parameters of SN-curve
# - Confidence intervals
# - Plot SN-curve with prediction limits
#
# Data:
#
# Pär Johannesson, 2017-12-14
#========================================================


#========================================================
# ==== Initiate   ====

rm(list=ls())

library(xlsx)  # Importing Excel-files

#source('SNwPJ_v2.1.R') # SN-curve
source('../SNwPJ_v2.5.2.R') # RISE Fatigue Tool - SN-curve
#source('../SNwPJ_v2.5.1.R') # RISE Fatigue Tool - SN-curve
#source('../SNVA_v1.0.0.R') # RISE Fatigue Tool - SN-curve
source('../SNVA_v1.1.0.R') # RISE Fatigue Tool - SN-curve
#source('../SNVA-ML_v0.1.0.R') # RISE Fatigue Tool - SN-curve
source('../SNVA-ML_v0.2.0.R') # RISE Fatigue Tool - SN-curve

#source('ToolboxSN.R') # PJ - Re-implementation using survreg

# Function: Print Figure
print.fig <- function(Pname,print=1) 
{
  # Print Figure
  if(print>0){
    # Print EPS
    Fname=paste(Pname,".eps",sep=""); dev.copy2eps(file=Fname, width=8,height=6)
    # Print SVG
    Fname=paste(Pname,".svg",sep=""); dev.print(svg, file=Fname, width=8,height=6)
    # Print PNG  
    Fname=paste(Pname,".png",sep=""); dev.print(png, file=Fname, width=640,height=480)
  }
}



#========================================================
# ==== Read data   ====

# --------------------------------------------------------
# SNVA-data
# - data in format:  S, N, RO/F

# Text format
Fname<-"AgerskovL_SNVA_NARROW.txt"  # Filename of data file
Fname<-"AgerskovL_SNVA_CA_NARROW.txt"  # Filename of data file

dat <- read.snva.data(Fname, format='txt')

# Excel format
Fname<-"../data/AgerskovL_SNVA_CA_NARROW.xlsx"  # Filename of data file
Fname<-"../data/AgerskovL_SNVA_CA_NARROW_run-outs.xlsx"  # Filename of data file
Fname<-"../data/AgerskovL_SNVA_CA.xlsx"  # Filename of data file

dat <- read.snva.data(Fname, format='xlsx')

# --------------------------------------------------------
# SN-data
# - data in format:  S, N, RO/F


Fname2<-"AgerskovL_SN_CA.txt"  # Filename of data file

datSN <- read.sn.data(Fname2, format='txt')


#========================================================
# SNVA-curve


# --------------------------------------------------------
# RISE fatigue tool

res1<-SNVA_TS(dat)

res1



res1<-SNw(dat$S,dat$N,dat$RO, 
          title="SN-curve: LIFEMOD J1",ylabel="ΔS, Stress range / MPa",xlabel="N, number of cycles to failure")

res1

# Test input
resML <- SNVA_TS(dat, int="pred")
resML <- SNVA_TS(dat, int="conf")
resML <- SNVA_TS(dat, int="both")
resML <- SNVA_TS(dat, int="none")


# --------------------------------------------------------
# New ML-implementation

resML <- SNVA.ML(dat)

resML

# Test input

resML <- SNVA.ML(dat, int="pred")
resML <- SNVA.ML(dat, int="conf")
resML <- SNVA.ML(dat, int="both")
resML <- SNVA.ML(dat, int="none")




# Test input: Prior beta
resML <- SNVA.ML(dat, beta_low=NA, beta_high=NA)
resML <- SNVA.ML(dat, beta_low=NA, beta_high=6)
resML <- SNVA.ML(dat, beta_low=4, beta_high=6)
resML <- SNVA.ML(dat, beta_low=6, beta_high=6.1)
resML <- SNVA.ML(dat, beta_low=-10, beta_high=20)

# Test input: Prior sigma
resML <- SNVA.ML(dat, S_prior=NA, S_prior_high=NA)
resML <- SNVA.ML(dat, S_prior=0.1, S_prior_high=.2)
resML <- SNVA.ML(dat, beta_low=NA, beta_high=6)
resML <- SNVA.ML(dat, beta_low=4, beta_high=6)
resML <- SNVA.ML(dat, beta_low=6, beta_high=6.1)
resML <- SNVA.ML(dat, beta_low=-10, beta_high=20)





# --------------------------------------------------------
# Compare; SN-curve vs. SNVA-curve
# RISE fatigue tool

# Test input: int
SNw(datSN$S,datSN$N,datSN$fail, title="SNw")

SNw.ML(datSN$S,datSN$N,datSN$fail, title="SNw.ML")
resML <- SNVA.ML(dat, title="SNVA.ML")


# Test input: Prior beta
SNw(datSN$S,datSN$N,datSN$fail, beta_low=4, beta_high=6, title="SNw")
SNw.ML(datSN$S,datSN$N,datSN$fail, beta_low=4, beta_high=6, title="SNw.ML")
resML <- SNVA.ML(dat, beta_low=4, beta_high=6, title="SNVA.ML")

SNw(datSN$S,datSN$N,datSN$fail, beta_low=6, beta_high=6.1, title="SNw")
SNw.ML(datSN$S,datSN$N,datSN$fail, beta_low=6, beta_high=6.1, title="SNw.ML")
resML <- SNVA.ML(dat, beta_low=6, beta_high=6.1, title="SNVA.ML")

# Slutsats: SNw.ML & SNVA.ML fungerar inte för prior beta


SNw.ML(dat$S,dat$N,dat$fail, beta_low=6, beta_high=6.1)
SNw.ML(dat$S,dat$N,dat$fail, beta_low=-10, beta_high=20)


# --------------------------------------------------------
# RISE fatigue tool
# Prior knowlwdge on slope

# Function call:
# SNw(S, N, RO, title="", ylabel="", xlabel="", formel=TRUE, int="pred", conf.level=0.95,
#               beta_low=NA,beta_high=NA,S_prior=NA,S_prior_high=NA,Nstrength=2e6,
#               preds_low=NA,preds_high=NA,Srange=NA,Nrange=NA,
#               plottype="b")

beta_prior = c(4,7) # uniform distribution between values

res1b<-SNw(dat$S,dat$N,dat$RO, beta_low=beta_prior[1], beta_high=beta_prior[2],
          title="SN-curve: LIFEMOD J1",ylabel="ΔS, Stress range / MPa",xlabel="N, number of cycles to failure")

res1b

# Print figure
print.fig("FigExSNcurve-1b")


# --------------------------------------------------------
# Print figure (with different plot parameters)
op<-par()
par(cex=1.5, cex.main=1.2, cex.lab=1.2, cex.axis=1.2) # Set plot parameters
SNw(dat$S,dat$N,dat$RO, 
    title=Fname, ylabel="ΔS, Stress range / MPa",xlabel="N, number of cycles to failure")

print.fig("FigExSNcurve")

par(op)  # reset to default


# --------------------------------------------------------
# PJ - Re-implementation using survreg

res2<-SN.est(dat, 
         title=group,ylabel="ΔS, Stress range / MPa",xlabel="N, number of cycles to failure")

res2



