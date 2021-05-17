df1 <- readRDS("../Data/experiment1.rds")
df2 <- readRDS("../Data/experiment2.rds")

# macmini
df1 <- readRDS("/Users/vferdinand/Library/Mobile Documents/com~apple~CloudDocs/Research/PROJECTS/Category Change/GITHUB REPO/Data/experiment1.rds")
df2 <- readRDS("/Users/vferdinand/Library/Mobile Documents/com~apple~CloudDocs/Research/PROJECTS/Category Change/GITHUB REPO/Data/experiment2.rds")

a <- "1111100000"
b <- "1011100000" # 1 change
c <- "0111100001" # 2 changes
d <- "1101011101" # 6 changes

# Levenshtein distance function is adist()
adist(a,b)
adist(a,c)
adist(a,d)

# try it on two strings in the dataframe
a <- df1$system512[1]
b <- df1$system512[6]
adist(a,b)

e <- "0111110001"
f <- "1100000110"
adist(e,f) # answer is 6, so this isn't the raw label flips!

# here's what it's actually doing:
drop(attr(adist(e, f, counts = TRUE), "counts"))

# make another function to compute the raw flips
diffs <- function(string1,string2) {
    a <- strsplit(string1, split = "")[[1]]
    b <- strsplit(string2, split = "")[[1]]
    ans <- length(a==b)-sum(a==b)
    return(ans)
}
# example usage:
diffs(e,f) # 8

# get edit distance for each pair of input-output systems in the data
get_edits_512 <- function(d) {
	edits <- c()
	for (i in 1:nrow(d)) {
		#edit <- adist(d$system512_input[i],d$system512[i]) # comment in to use Levenshtein distance
		edit <- diffs(d$system512_input[i],d$system512[i]) # comment in to use raw # differences
		#print(d$system512_input[i])
		#print(d$system512[i])
		#print(edit)
		edits <- c(edits,edit)
	}
	return(edits)
}

# append new column to each dataframe - contains the number of label flips (edits) between input and output 512 system
edits512 <- get_edits_512(df1)
df1 <- cbind(df1,edits512)
edits512 <- get_edits_512(df2)
df2 <- cbind(df2,edits512)


#####################################
# Does it have to use the 1024 system or are the answers the same anyway using the 512 system?
# Answers are different for 1024.

get_edits_1024 <- function(d) {
	edits <- c()
	for (i in 1:nrow(d)) {
		#edit <- adist(d$system1024_input[i],d$system1024[i])
		edit <- diffs(d$system1024_input[i],d$system1024[i])
		edits <- c(edits,edit)
	}
	return(edits)
}

# SANITY CHECK
table(get_edits_512(df1)==get_edits_1024(df1)) # 600 were the same number of edits, but 42 were different!
table(get_edits_512(df2)==get_edits_1024(df2)) # 609 same, 83 different

edits1024 <- get_edits_1024(df1)
df1 <- cbind(df1,edits1024)
edits1024 <- get_edits_1024(df2)
df2 <- cbind(df2,edits1024)

df1c <- subset(df1,condition=="C")
df1i <- subset(df1,condition=="I")
df2c <- subset(df2,condition=="C")
df2i <- subset(df2,condition=="I")


#####################################
# everything with variable name "edits" below was for the 512 Levenstein edits
#####################################
# Which systems showed zero edits? These should be fairly stable systems.  124 systems
subset(df1,edits==0)$system512

# What were their number of boundaries?   1-5 boundaries, all >5 were unstable
table(subset(df1,edits==0)$N_boundaries)

# show many of each zero-edit event occurred per zero-edit system
table(subset(df1,edits==0)$system512,subset(df1,edits==0)$edits)

# rank them - Experiment 1
"
1111100000 19
1111110000 13
1111111000 12
1111111100 10
1111000000  9
1110000000  8
1100000000  6
1100000001  5
1000000011  4
1111110001  4
1111111110  4
none        3
1000000000  2
1000000001  2
1100000010  2
1100000011  2
1110000001  2
1111100010  2
1111111001  2
several     1
"

# the most stable system is 1111100000 
# had 19 perfect reproductions (19 is the max for zero-edit systems)

# look at the same thing for Experiment 2
table(subset(df2,edits==0)$system512,subset(df2,edits==0)$edits)
"
1111000000 30
1111100000 20
1111110000 14
1111111111 14
1110000000 11
1111111000  5
"


# divide the above counts by the total number of counts per system type
nrow(subset(df1,system512=="1111100000")) # 63


# For all edit distances, show edit distance by number of boundaries 
# left = boundaries (), top = number of edits (0-9)
table(df1$N_boundaries,df1$edits)

# TO DO - plot the table above nicely

##########################################################################
# redo section above for the raw number of edits512 and edits1024

# Which systems showed zero edits? These should be fairly stable systems.  124 systems (same as above)
subset(df1,edits512==0)$system512

table(subset(df1,edits512==0)$system512,subset(df1,edits512==0)$edits512)
# ranking is the same as above

# check 1024 systems and edits also
table(subset(df1,edits1024==0)$system1024,subset(df1,edits1024==0)$edits1024)
# similar - but easier to visualize the top x systems with 512

# check for Experiment 2
table(subset(df2,edits512==0)$system512,subset(df2,edits512==0)$edits512)
table(subset(df2,edits1024==0)$system1024,subset(df2,edits1024==0)$edits1024)

##########################################################################
# stability in C vs I

table(subset(df1i,edits512==0)$system512,subset(df1i,edits512==0)$edits512)
" top 5 in I, Experiment 1
1111100000 9
1111111000 8
1111111100 8
1111000000 6
1111110000 6
"

table(subset(df1c,edits512==0)$system512,subset(df1c,edits512==0)$edits512)
" top 4 in C, Experiment 1
1111100000 10
1110000000  7
1111110000  7
1111111000  4
"

table(subset(df2i,edits512==0)$system512,subset(df2i,edits512==0)$edits512)
" top 4 in I, Experiment 2
1111000000 19
1111100000  9
1111110000  9
1111111111  9
"

table(subset(df2c,edits512==0)$system512,subset(df2c,edits512==0)$edits512)
" top 3 in C, Experiment 2
1111000000 11
1111100000 11
1110000000  9
"

##########################################################################
# what are the TPs among N_boundaries?

table(df1$N_boundaries_input,df1$N_boundaries)
" in is rows, out is on top"
"   1   2   3   4   5   6   7   8
1 177   0  11   2   1   0   0   0
2  12  78   8   8   0   0   0   0
3  46   1  42   4   7   2   1   1
4   3  37   7  28   2   7   1   1
5  20   4  18   4  25   0   3   0
6   8  10   7  16   3   5   0   0
7   8   2   6   3   2   2   1   0
8   0   1   1   4   0   0   2   0"

systype_input <- if(df1$N_boundaries_input==1){"continuous"}

ifelse(df1$N_boundaries_input==1, "continuous", "disjoint")

systype_input <- ifelse(df2$N_boundaries_input==0, "-degenerate", ifelse(df2$N_boundaries_input==1, "continuous", "disjoint"))
cbind(df2$N_boundaries_input,x)

##########################################################################
# what are the TPs among continuous and disjoint systems?

# add to df a column that categorizes input and output systems into 3 types
systype_input <- ifelse(df1$N_boundaries_input==0, "-degenerate", ifelse(df1$N_boundaries_input==1, "continuous", "disjoint"))
systype_output <- ifelse(df1$N_boundaries==0, "-degenerate", ifelse(df1$N_boundaries==1, "continuous", "disjoint"))
df1 <- cbind(df1,systype_input,systype_output)

systype_input <- ifelse(df2$N_boundaries_input==0, "-degenerate", ifelse(df2$N_boundaries_input==1, "continuous", "disjoint"))
systype_output <- ifelse(df2$N_boundaries==0, "-degenerate", ifelse(df2$N_boundaries==1, "continuous", "disjoint"))
df2 <- cbind(df2,systype_input,systype_output)

# have a second version where you group the degenerate ones with continuous ones
systype2_input <- ifelse(df1$N_boundaries_input==0, "continuous", ifelse(df1$N_boundaries_input==1, "continuous", "disjoint"))
systype2_output <- ifelse(df1$N_boundaries==0, "continuous", ifelse(df1$N_boundaries==1, "continuous", "disjoint"))
df1 <- cbind(df1,systype2_input,systype2_output)

systype2_input <- ifelse(df2$N_boundaries_input==0, "continuous", ifelse(df2$N_boundaries_input==1, "continuous", "disjoint"))
systype2_output <- ifelse(df2$N_boundaries==0, "continuous", ifelse(df2$N_boundaries==1, "continuous", "disjoint"))
df2 <- cbind(df2,systype2_input,systype2_output)

df1c <- subset(df1,condition=="C")
df1i <- subset(df1,condition=="I")
df2c <- subset(df2,condition=="C")
df2i <- subset(df2,condition=="I")


a <- table(df1$systype_input,df1$systype_output)  # input on left, output on top
"          continuous disjoint
continuous        177       14
disjoint           97      354
"

b <- table(df2$systype_input,df2$systype_output)
"            degenerate continuous disjoint
degenerate           14          2        3
continuous            4        186       43
disjoint             15        134      291
"

all_ins <- unlist(list(df1$systype_input,df2$systype_input))
all_outs <- unlist(list(df1$systype_output,df2$systype_output))

all <- table(all_ins,all_outs)

# normalize to each row sums to one
pall <- all/rowSums(all)

# get stationary distribution
stat_dist <- function(matrix) { # returns the stationary distribution of a RIGHT stochastic matrix (rows sum to one)
	vec <- eigen(t(matrix))$vectors[,1]
	return(vec/sum(vec))
}
stat_dist(pall) # 0.6264775  0.3299556  0.0435669

a <- a/rowSums(a)
b <- b/rowSums(b)
stat_dist(a)    # 0          0.7458234  0.2541766
stat_dist(b)    # 0.08228323 0.56916726 0.34854951


##########################
# Experiment 1

# whole experiment
x <- table(df1$systype_input,df1$systype_output)  # input on left, output on top
x <- x/rowSums(x)
"             continuous   disjoint
continuous 0.92670157 0.07329843
disjoint   0.21507761 0.78492239"
stat_dist(x)
"0.7458234 0.2541766"

# condition C
a <- table(df1c$systype_input,df1c$systype_output)  # input on left, output on top
"             continuous disjoint
   continuous         70        4     74 continuous in input
   disjoint           43       90    133   disjoint in input 
"   
a <- a/rowSums(a)
"          continuous   disjoint
continuous 0.94594595 0.05405405
disjoint   0.32330827 0.67669173"

stat_dist(a) # stat_dist() needs rows to sum to one
"0.8567582 0.1432418"

# condition I
b <- table(df1i$systype_input,df1i$systype_output)
"             continuous disjoint
   continuous        107       10    117 continuous in input
   disjoint           54      264    318   disjoint in input
"
b <- b/rowSums(b)
"          continuous   disjoint
continuous 0.91452991 0.08547009
disjoint   0.16981132 0.83018868"

stat_dist(b)
"0.6651927 0.3348073"

# and there should be no condition difference between the TPs when calculated for generation = 1 only
# there are gonna be like no continuous systems in input though
table(df1ig1$systype_input) # 1 continuous
table(df1cg1$systype_input) # 0 continous
# so just compare the way disjoint systems transition

df1ig1 <- subset(df1i,iteration==1)
df1cg1 <- subset(df1c,iteration==1)
c <- table(df1cg1$systype_input,df1cg1$systype_output)
c <- c/rowSums(c)
"             continuous  disjoint
continuous                     
disjoint    0.3777778 0.6222222"
i <- table(df1ig1$systype_input,df1ig1$systype_output)
i <- i/rowSums(i)
"          continuous  disjoint
continuous  1.0000000 0.0000000
disjoint    0.2921348 0.7078652"

# k well they're not identical - what the hell stats would you do on this?

##########################
# Experiment 2

# whole experiment - systype2 doesn't include degenerate - groups them as continuous
x <- table(df2$systype2_input,df2$systype2_output)  # input on left, output on top
x <- x/rowSums(x)
stat_dist(x)
"0.6497543 0.3502457" 

# condition C
a <- table(df2c$systype2_input,df2c$systype2_output)
a <- a/rowSums(a)
"          continuous disjoint
continuous   0.877551 0.122449
disjoint     0.440000 0.560000"

stat_dist(a)
"0.7822932 0.2177068"

# condition I
b <- table(df2i$systype2_input,df2i$systype2_output)
b <- b/rowSums(b)
"          continuous  disjoint
continuous  0.7792208 0.2207792
disjoint    0.2984127 0.7015873"

stat_dist(b)
"0.5747638 0.4252362"

# first generation only - should be similar
df2ig1 <- subset(df2i,iteration==1)
df2cg1 <- subset(df2c,iteration==1)
c <- table(df2cg1$systype2_input,df2cg1$systype2_output)
c <- c/rowSums(c)
"          continuous  disjoint
continuous  1.0000000 0.0000000
disjoint    0.2727273 0.7272727"
i <- table(df2ig1$systype2_input,df2ig1$systype2_output)
i <- i/rowSums(i)
"          continuous  disjoint
continuous                     
disjoint    0.3222222 0.6777778"



##########################################################################
# looking at gen 1 only, we'd expect no difference in TPs between C and I
# well the set of systems is more similar at that time across conditions...

# what are the TPs for randomly answering?  it's in file random_label_flipping.R

"         continous disjoint       repeats <- 1000  - took like 15 min to run?
continous  0.201000 0.799000
disjoint   0.008084 0.991916 "

# stationary distribution: 
"0.01001631 0.98998369"


##########################################################################
# how do the 3 types change over time?

d <- df1
# get strategy proportions over time
iter1 <- subset(d,iteration==1)
# initialize with the input to iteration 1
iter1 <- subset(d,iteration==1) #table(iter1$systype_input)
total <- c(length(iter1$participant)) # total number of systems (use for normalizing each p)
p_degenerate <- c(length(subset(iter1,systype_input=="degenerate")$participant))
p_continuous <- c(length(subset(iter1,systype_input=="continuous")$participant))
p_disjoint <- c(length(subset(iter1,systype_input=="disjoint")$participant))

# now use the output systems
for (i in 1:max(d$iteration)) {
	iter <- subset(d,iteration==i)
	total <- c(total,length(iter$participant))
	p_degenerate <- c(p_degenerate,length(subset(iter,systype_input=="degenerate")$participant))
	p_continuous <- c(p_continuous,length(subset(iter,systype_input=="continuous")$participant))
	p_disjoint <- c(p_disjoint,length(subset(iter,systype_input=="disjoint")$participant))
}
	
# sanity check that these all equal the total
unique((p_degenerate+p_continuous+p_disjoint)==total)

p_degenerate <- p_degenerate/total
p_continuous <- p_continuous/total
p_disjoint <- p_disjoint/total

p_degenerate
p_continuous
p_disjoint

# plot the type of each catsys over time
time <- seq(1,length(total))
plot(time,p_degenerate,type="l",las=1,ylim=c(0,1),lty="dotted")
lines(time,p_continuous)
lines(time,p_disjoint,lty="twodash")

# TO DO - make the last system in each category persist in each later generation
# for a better curved plot that communicates variants taking over the population




##########################################################################
# stability analysis
##########################################################################

# get stability of each system type, per experiment and per condition

stability_dataframe <- function(df) {
   
}

df <- df1

# for each system type, get % input-output pairs that were stable

all_system_types <- unique(df$system512)



##########################################################################
# size of evolutionary steps
##########################################################################

# Experiment 1
mean(df1c$edits1024)  # 1.97 in C
mean(df1i$edits1024)  # 2.13 in I

# Experiment 2
mean(df2c$edits1024)  # 2.26 in C
mean(df2i$edits1024)  # 2.35 in I

hist(df1c$edits1024, col="lightblue", las=1) # both are basically identical
hist(df1i$edits1024, col="lightblue", las=1)

hist(df2c$edits1024, col="lightblue", las=1)
hist(df2i$edits1024, col="lightblue", las=1)

######################
require(lme4)

# Experiment 1
full1 <- lmer(edits1024 ~ condition * iteration + (1|lineage), data=df1)
r1 <- lmer(edits1024 ~ condition + iteration + (1|lineage), data=df1)
anova(full1,r1) # full is almost better, but not  p = 0.06859

r2 <- lmer(edits1024 ~ iteration + (1|lineage), data=df1)
anova(r1,r2) # lose condition

r3 <- lmer(edits1024 ~ condition + (1|lineage), data=df1)
anova(r1,r3) # keep iteration


# Experiment 2
full2 <- lmer(edits1024 ~ condition * iteration + (1|lineage), data=df2)
r1 <- lmer(edits1024 ~ condition + iteration + (1|lineage), data=df2)
anova(full2,r1) # full is not better  p = 0.8492

r2 <- lmer(edits1024 ~ iteration + (1|lineage), data=df2)
anova(r1,r2) # lose condition

r3 <- lmer(edits1024 ~ condition + (1|lineage), data=df2)
anova(r1,r3) # keep iteration

# compare both of the full models
summary(full1)
"                     Estimate Std. Error t value
(Intercept)           2.96085    0.22342  13.252
conditionI            0.51358    0.27708   1.854
iteration            -0.33833    0.04958  -6.824
conditionI:iteration -0.11438    0.06285  -1.820
"
summary(full2)
"                     Estimate Std. Error t value
(Intercept)           3.56616    0.22744  15.679
conditionI            0.02246    0.28322   0.079
iteration            -0.41264    0.04686  -8.805
conditionI:iteration  0.01118    0.05941   0.188
"
# they yield different interpretations coz interaction direction is different

######################


require(ggplot2)

p <- ggplot(df1, aes(x=iteration, y=edits1024, group=condition, color=condition)) +
	geom_point()
p

# compute standard error
ste <- function(data) {
	sd(data)/sqrt(length(data))
}

# get mean edit distance of a dataframe over time
gettem <- function(df) {
	means <- c()
	stes <- c()
	for (i in 1:8) {
		means <- c(means,mean(subset(df,iteration==i)$edits1024))
		stes <- c(stes,ste(subset(df,iteration==i)$edits1024))
	}
	return(list(means,stes))
}

result <- gettem(df1c)
C1_means <- result[[1]]
C1_stes <- result[[2]]
result <- gettem(df1i)
I1_means <- result[[1]]
I1_stes <- result[[2]]

# make df for ggplot








