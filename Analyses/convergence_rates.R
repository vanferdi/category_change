df1 <- read.csv("../Data/experiment1_FINAL.csv")

df1c <- subset(df1,condition=="C")
df1i <- subset(df1,condition=="I")

# number of trajectories that haven't converged yet, per iteration
table(df1c$iteration)
table(df1i$iteration)

#############################################################################
# get percentage of trajectories that converged on each iteration

# cultural condition
x <- c(as.vector(table(df1c$iteration)[2:11]),0) # add zero coz we know it converged
c1 <- 1-(x/45) # make inverse into a percentage

# individual condition
# need to look up how many trajectories in the last iteration were and weren't converged
table(subset(df1i,iteration==8)$converged)
# 8 converged and 11 did not
x <- c(as.vector(table(df1i$iteration)[2:8]),11) # so add 11
i1 <- 1-(x/90) # make inverse into a percentage
i1 <- c(i1,"NA","NA","NA")

#############################################################################


plot(c1,type="l",las=1, xaxt="n",xlab="iteration",ylab="converged trajectories")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="#DCDCDC")
abline(v=seq(1,11),h=seq(0,1,by=0.1),col="white")
lines(i1,type="l",las=1)
lines(c1,type="l",las=1)
points(c1,pch=1) # 16
points(i1,pch=2) # 17
# make your own x axis labels
xtick < -seq(1, 11, by=1)
axis(side=1, at=xtick, labels=FALSE)
text(x=xtick, par("usr")[3], labels=xtick, pos=1, offset=1, xpd=TRUE)




plot(c1,type="l",las=1, xaxt="n",xlab="iteration",ylab="converged trajectories")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="#DCDCDC")
abline(v=seq(1,11),h=seq(0,1,by=0.1),col="white")
lines(i1,type="l",las=1,lwd=2,lty="dashed")
lines(c1,type="l",las=1,lwd=2)
# make your own x axis labels
xtick < -seq(1, 11, by=1)
axis(side=1, at=xtick, labels=FALSE)
text(x=xtick, par("usr")[3], labels=xtick, pos=1, offset=1, xpd=TRUE)



