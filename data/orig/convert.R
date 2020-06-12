# ==== Read data   ====



Fname<-"load_short"
#Fname<-"load"

dat0 <- read.table(paste0(Fname,".txt"), sep="",dec=".",header=FALSE, skip=0, as.is=TRUE)
names(dat0) <- c("t", "L")

dat0

TS <- dat0

# Save
save(TS, file=paste0(Fname,".RData"), ascii=TRUE)
