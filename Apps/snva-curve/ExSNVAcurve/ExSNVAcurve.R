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

#library(xlsx)  # Importing Excel-files

#source('SNwPJ_v2.1.R') # SN-curve
source('../SNwPJ_v2.5.0.R') # RISE Fatigue Tool - SN-curve
source('../SNVA_v1.0.0.R') # RISE Fatigue Tool - SN-curve

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

# # first row contains variable names
# library(xlsx)
# mydata <- read.xlsx("c:/myexcel.xlsx", 1)
# 
# # read in the worksheet named mysheet
# mydata <- read.xlsx("c:/myexcel.xlsx", sheetName = "mysheet")

# --------------------------------------------------------
# Alternative 1: Data in text-format (RISE fatigue design tool format)
# - data in format:  S, N, RO/F


Fname<-"AgerskovL_SN_NARROW.txt"  # Filename of data file
Fname<-"AgerskovL_SN_CA_NARROW.txt"  # Filename of data file

dat <- read.snva.data(Fname)




#========================================================
# SNVA-curve


# --------------------------------------------------------
# RISE fatigue tool

res1<-SNVA_TS(dat)

res1



          res1<-SNw(dat$S,dat$N,dat$fail, 
                    title="SN-curve: LIFEMOD J1",ylabel="ΔS, Stress range / MPa",xlabel="N, number of cycles to failure")

res1

# Print figure
print.fig("FigExSNcurve-1b")


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



