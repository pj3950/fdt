# ==== Read data   ====



Fname<-"sea"

dat0 <- read.table(paste0(Fname,".dat"), sep="",dec=".",header=FALSE, skip=0, as.is=TRUE)
names(dat0) <- c("time", "Sea.elevation")

dat0

# Example: Plot
ts <- dat0
tp <- ts2tp(ts)   # Extract turning points of time signal.
plot(ts[,1],ts[,2], type="l", col="blue", main="Turning points",xlab="Time [s]",ylab="Sea surface elevation [m]")
lines(tp[,1],tp[,2], type="b", col="black")
# END

sea <- dat0


# Save
save(sea, file=paste0(Fname,".RData"))
