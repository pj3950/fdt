# ========================================================
# Example: Estimate SN-curve
# - Estimate parameters of SN-curve
# - Confidence intervals
# - Plot SN-curve with prediction limits
#
# Data:
#
# Pär Johannesson, 2023-10-31
#========================================================


#========================================================
# ==== Initiate   ====

rm(list=ls())

library(xlsx)  # Importing Excel-files

#source('SNwPJ_v2.1.R') # SN-curve
#source('SNwPJ_v2.2.R') # RISE Fatigue Tool - SN-curve
#source("../SNwPJ_v2.5.0.R")
source("../SNwPJ_v2.5.2.R")

source('ToolboxSN.R') # PJ - Re-implementation using survreg

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


#Fname<-"LIFEMOD_J1.txt"  # Filename of data file
Fname <- "../data/laser_cbj.txt"

#dat0 <- read.table(Fname,sep="\t",dec=".",header=FALSE, skip=0, as.is=TRUE)
dat0 <- read.table(Fname, sep="", dec=".", header=FALSE, skip=0, as.is=TRUE, fill=TRUE)

names(dat0) <- c("S","N","fail")

dat0

dat <- dat0

# --------------------------------------------------------
# Alternative 2: Data in xlsx-format
# - first row contains variable names; S, N, fail

Fname<-"LIFEMOD_J1.xlsx"      # Filename of data file
dat0 <- read.xlsx(Fname, 1)   # Read data file
dat0                          # Look at data

names(dat0) <- c("S","N","fail") # Set correct field names

dat <- dat0


# --------------------------------------------------------
# Data in text-format (RISE fatigue design tool format)
# - data in format:  S, N, RO/F
Fname<-"provresultat.txt"  # Filename of data file

dat0 <- read.table(Fname,sep="\t",dec=".",header=FALSE, skip=0, as.is=TRUE)
names(dat0) <- c("S","N","fail")

dat0

dat <- dat0

#========================================================
# SN-curve


# --------------------------------------------------------
# RISE fatigue tool

res1<-SNw(dat$S,dat$N,dat$fail, 
         title="SN-curve: LIFEMOD J1",ylabel="ΔS, Stress range / MPa",xlabel="N, number of cycles to failure")

res1

res2<-SNw(dat$S,dat$N,dat$fail, conf.level = 0.9,
          title="SN-curve: LIFEMOD J1",ylabel="ΔS, Stress range / MPa",xlabel="N, number of cycles to failure")

res2

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



