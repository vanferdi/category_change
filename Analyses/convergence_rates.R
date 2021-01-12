df1 <- read.csv("../Data/experiment1_FINAL.csv")
df2 <- read.csv("../Data/experiment2_FINAL.csv")

df1c <- subset(df1,condition=="C")
df1i <- subset(df1,condition=="I")
df2c <- subset(df2,condition=="C")
df2i <- subset(df2,condition=="I")

# number of trajectories that haven't converged yet, per iteration
table(df1c$iteration)
table(df1i$iteration)
table(df2c$iteration)
table(df2i$iteration)

#############################################################################
# get percentage of trajectories that converged on each iteration

# cultural condition

x <- c(as.vector(table(df1c$iteration)[2:11]),0) # add zero coz we know it converged
c1 <- 1-(x/45) # make inverse into a percentage

x <- c(as.vector(table(df2c$iteration)[2:15]),0) 
c2 <- 1-(x/45)

# individual condition

# need to look up how many trajectories in the last iteration were and weren't converged
table(subset(df1i,iteration==8)$converged) # 8 converged and 11 did not
x <- c(as.vector(table(df1i$iteration)[2:8]),11) # so add 11
i1 <- 1-(x/90) # make inverse into a percentage
i1 <- c(i1,"NA","NA","NA")

table(subset(df2i,iteration==8)$converged) # 10 converged and 16 did not
x <- c(as.vector(table(df2i$iteration)[2:8]),16) # so add 16
i2 <- 1-(x/90) 
i2 <- c(i2,"NA","NA","NA","NA","NA","NA","NA")

#############################################################################

# Experiment 1

dev.new(width=440,height=400, unit="px")
plot(c1,type="l",las=1, xaxt="n",xlab="iteration",ylab="converged",main="Experiment 1")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="#DCDCDC")
abline(v=seq(1,11),h=seq(0,1,by=0.1),col="white")
lines(i1,type="l",las=1,lwd=2,lty="twodash")
lines(c1,type="l",las=1,lwd=2)
# make your own x axis labels
xtick <- seq(1, 11, by=1)
axis(side=1, at=xtick, labels=FALSE)
text(x=xtick, par("usr")[3], labels=xtick, pos=1, offset=1, xpd=TRUE)

# Experiment 2

dev.new(width=600,height=400, unit="px")
plot(c2,type="l",las=1, xaxt="n",xlab="iteration",ylab="",main="Experiment 2")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="#DCDCDC")
abline(v=seq(1,15),h=seq(0,1,by=0.1),col="white")
lines(i2,type="l",las=1,lwd=2,lty="twodash")
lines(c2,type="l",las=1,lwd=2)
# make your own x axis labels
xtick <- seq(1, 15, by=1)
axis(side=1, at=xtick, labels=FALSE)
text(x=xtick, par("usr")[3], labels=xtick, pos=1, offset=1, xpd=TRUE)
legend(10, 0.5, legend=c("cultural", "individual"), lty=c("solid","twodash"), cex=1)


#############################################################################
# sanity check something

# print the number of test systems that equalled the train system
# (in iteration 1, the train system is a random initial system)

get_result <- function(d) {
	result <- c()
	for (i in 1:max(d$iteration)) {
		#print(paste("iteration",i))
		sub <- subset(d,iteration==i)
		#print(as.integer(table(sub$converged)["TRUE"]))
		result <- c(result,as.integer(table(sub$converged)["TRUE"]))
	}
	return(result)
}

result_1c <- get_result(df1c)
result_1i <- get_result(df1i)
result_2c <- get_result(df2c)
result_2i <- get_result(df2i)

