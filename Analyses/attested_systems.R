require(ggplot2)
library(tidyverse) # gives you pipe %>%

df1 <- readRDS("../Data/experiment1.rds")
df2 <- readRDS("../Data/experiment2.rds")

# look at all possible category system schemas
# particular label is important (ex: AB and BA are 2 systems): 1024 systems
# label identity isn't import (we're just looking at the patter of where the boundaries are, ex: AB and BA are 1 system): 512 systems

# this whole file works with 512 unique systems

##############################################################
# how many unique systems are there per number of category boundaries? 
# answer: choose(9,k) where k = the number of category boundaries

k <- seq(0,9)
uniques <- choose(9,k)
#sum(count) # 512 - sanity check

# how many of these are hit up in the C and I conditions respectively?

gettem <- function(df,cond) {
	result <- c()
	for (i in k) {
		temp <- subset(df,condition==cond)
		temp <- subset(temp,N_boundaries==i)
		result <- c(result,length(unique(temp$system512)))
	}
	return(result)
}

C1 <- gettem(df1,"C")
I1 <- gettem(df1,"I")
C2 <- gettem(df2,"C")
I2 <- gettem(df2,"I")

# create data frame 
boundaries <- seq(0,9)
d <- data.frame(boundaries,uniques,C1,C2,I1,I2)

pC1 <- d$C1/d$uniques
pC2 <- d$C2/d$uniques
pI1 <- d$I1/d$uniques
pI2 <- d$I2/d$uniques

##############################################################
# create dataframe for ggplot2 plot

boundary <- rep(c("0","1","2","3","4","5","6","7","8","9"),2)
condition <- c(rep("cultural",10),rep("individual",10))
yvals <- c(pC1,pI1)
dgg1 <- data.frame(boundary,condition,yvals)

yvals <- c(pC2,pI2)
dgg2 <- data.frame(boundary,condition,yvals)

dgg2 %>% 
	ggplot(mapping = aes(x=boundary,y=yvals,fill=condition)) +
	geom_bar(position="dodge",stat="identity",colour="black",alpha=0.7) +
	theme_bw() + 
	scale_fill_manual(values=c("black", "grey")) +
	labs(title = "Distinct category systems for each # of boundaries",
		 y = "Proportion of distinct systems",
		 x = "Number of boundaries")

plottr <- function(d, my_title) {
	ggplot(mapping = aes(x=d$boundary,y=d$yvals,fill=d$condition)) +
	geom_bar(position="dodge",stat="identity",colour="black",alpha=0.7) +
	theme_bw() + 
	scale_fill_manual(values=c("black", "grey")) +
	labs(y = "Proportion of distinct systems", x = "Number of boundaries") +
	ggtitle(my_title) +
	theme(plot.title = element_text(hjust = 0.5),
		  legend.title = element_blank(), 
		  text = element_text(size=15))
		  #legend.position=c(.8,.84))  # puts legend inside plot area
}

plottr(dgg1,"Experiment 1")   # export width=800, height=400
plottr(dgg2,"Experiment 2")

##############################################################

