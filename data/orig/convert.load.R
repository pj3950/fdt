# ==== Read data   ====



#Fname<-"load_short"
Fname<-"load"

dat0 <- read.table(paste0(Fname,".txt"), sep="",dec=".",header=FALSE, skip=0, as.is=TRUE)
names(dat0) <- c("time", "load")

dat0

# Example: Plot
ts <- dat0
tp <- ts2tp(ts)   # Extract turning points of time signal.
plot(ts[,1],ts[,2], type="l", col="blue", main="Turning points",xlab="Time [s]",ylab="Sea surface elevation [m]")
lines(tp[,1],tp[,2], type="b", col="black")
# END

load <- dat0

# Save
save(load, file=paste0(Fname,".RData"))
