# read in the data from the two experiments
df1 <- read.csv("../../Data/experiment1_FINAL.csv", colClasses=c(system1024="character"))
df2 <- read.csv("../../Data/experiment2_FINAL.csv", colClasses=c(system1024="character"))

# subset by condition	
df1i <- subset(df1,condition=="I")
df1c <- subset(df1,condition=="C")
df2i <- subset(df2,condition=="I")
df2c <- subset(df2,condition=="C")

jf1 <- subset(df1,iteration < 8)
jf2 <- subset(df2,iteration < 8)
jf1i <- subset(jf1,condition=="I")
jf1c <- subset(jf1,condition=="C")
jf2i <- subset(jf2,condition=="I")
jf2c <- subset(jf2,condition=="C")

##########################################################################
# Discovery Rate
##########################################################################

# get the unique set of systems discovered in each condition
# and divide by the total number of tries (i.e. iterations) in that condition
# does one condition find more unique systems in fewer tries, or not?

# do not include the generation zero initial systems, 
# because these aren't systems that the participants produced or discovered themselves.  
# all systems in column "system512" and "system1024" is the output of each try.

# Experiment 1
length(unique(df1$system512))/nrow(df1)    # 188 / 642 = 0.293 - the probability that each try results in discovery of a new system
length(unique(df1i$system512))/nrow(df1i)  # 159 / 435 = 0.366
length(unique(df1c$system512))/nrow(df1c)  #  75 / 207 = 0.362

# Experiment 2
length(unique(df2$system512))/nrow(df2)    # 183 / 692 = 0.264 - interesting that the total % is lower
length(unique(df2i$system512))/nrow(df2i)  # 151 / 469 = 0.322 - means they had diff exploratory niches?
length(unique(df2c$system512))/nrow(df2c)  #  74 / 223 = 0.332

# there was no difference in the size of the space explored
# (but remember there was a difference in the diversity of N_bound types explored - in that violin plot)

# see way below for calculations of local and global discovery rates over time

####################################################
# Subset the systems discovered by only C, only I, and both C and I
####################################################

# Experiment 1

# what systems are in both C and I?
intersect(df1i$system512,df1c$system512)
length(unique(intersect(df1i$system512,df1c$system512)))  # 46 unique systems
# all 9 of the 1-boundary systems are in here

# what systems were only discovered in C (and not I)?  
# setdiff(a,b) returns what is in a and not in b
unique(setdiff(df1i$system512,df1c$system512))    # 113 systems in I that are not in C
unique(setdiff(df1c$system512,df1i$system512))    #  29 systems in C that are not in I
# visually, they look like a smattering of the same thing
# but do they differ by their number of boundaries?

get_n_boundaries <- function(systems) {
    require(stringr)
    boundaries01 <- str_count(systems, pattern = "01")
    boundaries10 <- str_count(systems, pattern = "10")
    return(boundaries01+boundaries10)
}

get_n_boundaries("0000011111")

both <- intersect(df1i$system512,df1c$system512)
I_only <- setdiff(df1i$system512,df1c$system512)
C_only <- setdiff(df1c$system512,df1i$system512)

# baseline number of 512 systems in each n boundaries bin: choose(n,k) where n = 9 (max bounds)
choose(9,seq(0,9))
" 0  1  2  3  4   5   6  7  8  9 = k
  1  9  36 84 126 126 84 36 9  1 = number of systems of each  k
"
# table of N boundaries in both C and I (the intersection)
table(get_n_boundaries(unique(both)))
" 1  2  3  4  5  6  7 
  9 17 11  5  1  1  2"  # all 9 of the 1-bound systems are in the intersection

# table of N boundaries for uniques in I only
table(get_n_boundaries(unique(I_only)))
" 2  3  4  5  6  7  8 
  6 29 36 28 10  3  1"

# table of N boundaries for uniques in C only
table(get_n_boundaries(unique(C_only)))
" 2  3  4  5  6  7  8 
  3 10  6  5  3  1  1 "

table(get_n_boundaries(unique(df1$system512)))
"  1  2  3  4  5  6  7  8 
   9 26 50 47 34 14  6  2 "

# the C condition is exploring proportionally more systems with N_bounds 2 & 3 than the I condition is.
# convert to proportion
table(get_n_boundaries(unique(C_only)))/29
"     2      3      4      5      6      7      8
  0.103  0.344  0.206  0.172  0.103  0.034  0.034 "

# and the I condition is exploring proportionally more systems with N_bounds 4 & 5
table(get_n_boundaries(unique(I_only)))/113
"     2      3      4      5      6      7      8
  0.053  0.256  0.318  0.247  0.088  0.026  0.008"

# this makes me think the cultural chains must be more homogenous to one another
# because they are exploring spaces that contain fewer unique category systems
# yet C and I still find the same number of category systems on average!


##########################
# rethink: should I be inlcuding the random initial systems in this or not?

# number of unique systems including the init set
length(unique(c(df1$system512,df1$system512_input)))   # 239
length(unique(c(df1i$system512,df1i$system512_input))) # 206
length(unique(c(df1c$system512,df1c$system512_input))) # 106

# each row is a try - the init systems weren't the product of a try
# I think don't include the initial sets as discoveries
# also this initial set doesn't differ between conditions - it's a random sample from the same set


##########################################################################
# Discovery rate over time
##########################################################################
# maybe ignore this section - the local and global calculations below are better

# gets the set of unique categories per iteration
itersets <- function(df_name,system_type) {
	sets <- c()  # the set of unique systems per iteration
	setsize <- 0 # the number of unique sysetms per iteration
	for (i in 1:max(df_name$iteration)) {
		if (system_type == 512) { u <- unique(subset(df_name,iteration==i)$system512) }
		if (system_type == 1024) { u <- unique(subset(df_name,iteration==i)$system1024) }
		sets <- c(sets,list(u))
	}
	setsize <- lengths(sets) # the number of unique sysetms per iteration
	return(sets)
}

# gets the total number of categories systems per iteration
itertries <- function(df_name,system_type) {
	tries <- c() # total number of tries per iteration
	for (i in 1:max(df_name$iteration)) {
		tries <- c(tries,nrow(subset(df_name,iteration==i)))
	}
	return(tries)
}

# example usage
sets <- itersets(df1i,512)
tries <- itertries(df1i,512)
setsize <- lengths(sets)
sets[[1]] # the set of category systems in iteration 1

# returns the set of systems discovered for the first time per iteration
cumulative_novel_counts <- function(itersets_result) {
	new_discoveries <- list(unique(itersets_result[[1]])) # call all of the sets in the first iteration new discoveries
	for (i in 2:length(itersets_result)) { # for each iteration, 2 to max
		all_previous <- unlist(itersets_result[1:(i-1)])
		current_set <- unlist(itersets_result[i])
		n <- setdiff(current_set,all_previous) # returns what is in current_set and not in all_previous
		new_discoveries <- c(new_discoveries,list(n))
	}
	return(new_discoveries)
}

# example usage
cumulative_novel_counts(sets)

# the percentage of new systems discovered, out of the total discovery attempts in that iteration
cumulative_discovery_rate <- function(df_name,system_type) {
	new_discoveries <- cumulative_novel_counts(itersets(df_name,system_type))
	tries <- itertries(df_name,system_type)
	return(lengths(new_discoveries)/tries)
}

# plot percentage of tries that yielded a novel discovery per iteration
rate_plot <- function(e1i,e1c,e2i,e2c,title,ymax) {
	time <- seq(1,15)
	plot(time,e2c,las=1,ylim=c(0,ymax),type="l",lty="dotted",col="red",ylab="proportion discovered",xlab="iteration",main=title)
	lines(e1i,type="l")
	lines(e1c,type="l",lty="dotted")
	lines(e2i,type="l",col="red")
	legend(10, ymax, cex=0.8, legend=c("Exp1 individual", "Exp1 cultural","Exp2 individual", "Exp2 cultural"), 
		   col=c("black", "black","red", "red"), 
		   lty=c("solid","dotted","solid","dotted"))
}

################################################
# Discovery rate per iteration - 512 system
################################################

# Experiment 1
e1i_512 <- cumulative_discovery_rate(df1i,512)
e1c_512 <- cumulative_discovery_rate(df1c,512)

# Experiment 2
e2i_512 <- cumulative_discovery_rate(df2i,512)
e2c_512 <- cumulative_discovery_rate(df2c,512)

rate_plot(e1i_512,e1c_512,e2i_512,e2c_512,"discovery rate - 512 systems",1)

################################################
# Discovery rate per iteration - 1024 system
################################################

# Experiment 1
e1i_1024 <- cumulative_discovery_rate(df1i,1024)
e1c_1024 <- cumulative_discovery_rate(df1c,1024)

# Experiment 2
e2i_1024 <- cumulative_discovery_rate(df2i,1024)
e2c_1024 <- cumulative_discovery_rate(df2c,1024)

rate_plot(e1i_1024,e1c_1024,e2i_1024,e2c_1024,"discovery rate - 1024 systems",1)

################################################
# Take home message from this section:
# Discovery rate starts off high and slows down each iteration as more space is explored.
# After seeing these plots I realized they aren't very communicative
# instead let's plot the proportion of space explored cumulatively over time


##########################################################################
# Cumulative proportion of the space discovered over time
##########################################################################

# Out of the 512 total systems, how many have been discovered by iteration x?
# This is cumulative, ex: iteration 5 contains all uniques discovered in iteration 1-5

sets <- itersets(df1i,512)     # uniques within each iteration
cumulative_novel_counts(sets)  # uniques

lengths(cumulative_novel_counts(itersets(df1i,512)))

space_discovery <- function(df_name,system_type) {
	x <- lengths(cumulative_novel_counts(itersets(df_name,system_type)))
	discovered <- c()  # cumulative discoveries by each iteration
	for (i in 1:length(x)) {
		discovered <- c(discovered,sum(x[1:i]))
	}
	return(discovered/system_type)
}

# example usage
space_discovery(df1i,512)

################################################
# Expansion rate - 512 system
################################################

a <- space_discovery(df1i,512)
b <- space_discovery(df1c,512)
c <- space_discovery(df2i,512)
d <- space_discovery(df2c,512)

rate_plot(a,b,c,d,"expansion rate - 512 systems",0.5)

################################################
# Expansion rate - 1024 system
################################################

a <- space_discovery(df1i,1024)
b <- space_discovery(df1c,1024)
c <- space_discovery(df2i,1024)
d <- space_discovery(df2c,1024)

rate_plot(a,b,c,d,"expansion rate - 1024 systems",0.5)


################################################
# Expansion rate - 512 system (45 chains from I)
################################################

# of course the I condition discovers more systems because we give it 2x more tries.
# now randomly sample 45 chains from the I condition and re-do the plot above

sampleI45 <- function(df) { # feed it df1 or df2
	I_trajectories <- unique(subset(df,condition=="I")$trajectory)  # get all 90 chains in I
	I_trajectories_sample <- sample(I_trajectories,45) # randomly choose 45 of them
	dfI45 <- df[df$trajectory %in% I_trajectories_sample,]  # 214 or so entries
	return(dfI45)
}

a <- space_discovery(sampleI45(df1),512)
b <- space_discovery(df1c,512)
c <- space_discovery(sampleI45(df2),512)
d <- space_discovery(df2c,512)

rate_plot(a,b,c,d,"expansion rate - 512 systems (45 from I)",0.5)

# important note:
# each chain has a variable number of tries (iterations) in it
# so matching chain number doesn't actually match the number of tries
# compare:
nrow(sampleI45(df1))
nrow(df1c)

# k but I is still hitting a higher proportion of novel ones,
# even when it's total tries is lower than the tries in C (always 207 above)
# (run the above code over and over to see this)

# I need to get confidence intervals for these lines.
# It kinda looks like I is exploring a larger portion of the space over time
# But again - C and I both find 1 system per 3 tries in the aggregate.
# What's up with that?

################################################
# Expansion rate - 1024 system (45 chains from I)
################################################

a <- space_discovery(sampleI45(df1),1024)
b <- space_discovery(df1c,1024)
c <- space_discovery(sampleI45(df2),1024)
d <- space_discovery(df2c,1024)

rate_plot(a,b,c,d,"expansion rate - 1024 systems (45 from I)",0.5)


##########################################################################
# Sanity check for the 1024 systems
##########################################################################

# I've been looking at the 512 systems (i.e. saying that 0000000000 and 1111111111 are the same system type)
# But let's make sure we get the same discovery rates for the 1024 systems.
# (the difference between 512 things to discover in 642 tries vs 1024 things to discover in 642 tries)

head(df1$system1024) # double check these read in correctly as strings

# Experiment 1
length(unique(df1$system1024))/nrow(df1)    # 237 / 642 = 0.369
                                            # 188 / 642 = 0.293 <- for system512
length(unique(df1i$system1024))/nrow(df1i)  # 197 / 435 = 0.453
                                            # 159 / 435 = 0.366 <- for system512
length(unique(df1c$system1024))/nrow(df1c)  #  91 / 207 = 0.440
                                            #  75 / 207 = 0.362 <- for system512

# I thought these rates should be the same because we're just doubling the discoverable systems
# but the rates are higher for the 1024 systems - because...?
# It'd hafta be something like if this process discovered 0000000000 it's more likely to discover 1111111111
# so there's some kind of contingency between the mirror image sets,
# which is probably due to the more thorough exploration of low N_bounds
# or means they really are exploring types and the labels don't matter.


##########################################################################
# Double check for the just 8
##########################################################################

df1_just8 <- subset(df1,iteration < 9)
df2_just8 <- subset(df2,iteration < 9)

df1i_just8 <- subset(df1_just8,condition=="I")
df1c_just8 <- subset(df1_just8,condition=="C")
df2i_just8 <- subset(df2_just8,condition=="I")
df2c_just8 <- subset(df2_just8,condition=="C")

# 1024 system
length(unique(df1_just8$system1024))/nrow(df1_just8)    # 237 / 633 = 0.374
length(unique(df1i_just8$system1024))/nrow(df1i_just8)  # 197 / 435 = 0.453   same
length(unique(df1c_just8$system1024))/nrow(df1c_just8)  #  90 / 198 = 0.455

# numbers in C and I got more similar to one another when you restructed analysis to the first 8 iterations

# 512 system
length(unique(df1_just8$system512))/nrow(df1_just8)     # 188 / 633 = 0.297
length(unique(df1i_just8$system512))/nrow(df1i_just8)   # 159 / 435 = 0.366   same
length(unique(df1c_just8$system512))/nrow(df1c_just8)   #  75 / 198 = 0.379

# compare above to 512 system original
length(unique(df1$system512))/nrow(df1)                 # 188 / 642 = 0.293
length(unique(df1i$system512))/nrow(df1i)               # 159 / 435 = 0.366
length(unique(df1c$system512))/nrow(df1c)               #  75 / 207 = 0.362

# numbers in C and I got less similar - C shot up higher


##########################################################################
# Local and Global Discoveries
##########################################################################

# Ok this is a better treatment of the discovery process
# Go through each system and ask: 
# is this the first discovery of this system locally in the chain?
# is this the first discovery of this system globally among all chains?

local_global_maker <- function(df,system_type) {  # system_type = 512 or 1024
	local_discovery <- c()  # is this system new to the chain? (not in previous iterations in this chain)
	global_discovery <- c() # is this system new to the world? (not in previous iterations in all chains)
	
	# loop through each system (row) in the dataframe
	for (i in 1:nrow(df)) {
	#for (i in 1:10) {
		current_system <- df[i,system_type]     # grab the current system
		current_trajectory <- df[i,]$trajectory # look up the trajectory ID of the current system (chain is for C only)
		current_iteration <- df[i,]$iteration   # look up what iteration it is
		
		# get all systems from the previous iterations in that chain
		temp <- subset(df,trajectory==current_trajectory)
		previous_local <- subset(temp,iteration<current_iteration)[[system_type]] # can be empty
		
		# check if current system already exists in the previous systems (setdiff(a,b) returns what is in a and not in b)
		n <- length(setdiff(current_system,previous_local))  # returns 0 if current_system is not new, 1 if it is
		local_discovery <- c(local_discovery,n) # add boolean value to the new column we're creating
		
		# check if the current system already exists in previous systems of ALL the chains in the respective condition
		current_condition <- df[i,]$condition
		temp <- subset(df,condition==current_condition)
		previous_global <- subset(temp,iteration<current_iteration)[[system_type]]
		n <- length(setdiff(current_system,previous_global))
		global_discovery <- c(global_discovery,n)
		
	}
	return(list(local_discovery,global_discovery))
}

# btw, here's how to access a dataframe column using a variable in place of the column name (por fin!):
system_type <- "system1024"
df1[[system_type]]

# add columns for both systems to df1
local_discovery_512 <- local_global_maker(df1,"system512")[[1]]
global_discovery_512 <- local_global_maker(df1,"system512")[[2]]
local_discovery_1024 <- local_global_maker(df1,"system1024")[[1]]
global_discovery_1024 <- local_global_maker(df1,"system1024")[[2]]
df1 <- cbind(df1,local_discovery_512,global_discovery_512,local_discovery_1024,global_discovery_1024)

head(df1)

################################################
# sanity checks

# all iteration = 1 should have discovery = 1
subset(df1,iteration==1)$local_discovery_512
subset(df1,iteration==1)$global_discovery_512
subset(df1,iteration==1)$local_discovery_1024
subset(df1,iteration==1)$global_discovery_1024

################################################
# for starters, just look at the data

df1i <- subset(df1,condition=="I")  # 435 rows
df1c <- subset(df1,condition=="C")  # 207 rows

df1i$local_discovery_1024 
df1c$local_discovery_1024

################################################

il <- df1i$local_discovery_1024 
cl <- df1c$local_discovery_1024
ig <- df1i$global_discovery_1024 
cg <- df1c$global_discovery_1024

# Is the proportion of local discoveries similar between conditions? Yes
sum(il)/length(il)  # 0.737931   local discoveries per try, I
sum(cl)/length(cl)  # 0.7536232                             C    

# Is the proportion of global discoveries similar between conditions? Yes
sum(ig)/length(ig)  # 0.5103448 global discoveries per try, I
sum(cg)/length(cg)  # 0.5120773                             C

# and it's easier to make a local discovery than a global one - which makes sense.

# the probability that any try will be a local discovery is ~ 0.75 and a global discovery is ~ 0.5

# double check that 512 is approx identical
il <- df1i$local_discovery_512 
cl <- df1c$local_discovery_512
ig <- df1i$global_discovery_512 
cg <- df1c$global_discovery_512

sum(il)/length(il)  # 0.737931   local discoveries per try, I - identical to 1024
sum(cl)/length(cl)  # 0.7536232                             C - identical to 1024   
sum(ig)/length(ig)  # 0.4482759 global discoveries per try, I
sum(cg)/length(cg)  # 0.4541063

# this makes sense:
# the local scores don't change because people don't reverse the label mapping in one chain
table(df1i$local_discovery_1024,df1i$local_discovery_512) # see, no mismatch in local scores at all
# but the global scores are lower because they do discover sister systems (ex: 0000000000 and 1111111111 are sister systems)
table(df1i$global_discovery_1024,df1i$global_discovery_512) # 27 of the 512 "olds" are new according to 1024
sum(df1i$global_discovery_1024) # 222 new 1024 systems = 195 + 27



##########################################################################
# do lmer - stick to the 1024

# is there a significant effect of condition on discovery rate or not?

#############
### LOCAL ###
#############
full <- glmer(local_discovery_1024 ~ condition * iteration + (1|trajectory), data=df1, family=binomial)
m1 <- glmer(local_discovery_1024 ~ iteration + (1|trajectory), data=df1, family=binomial)
m2 <- glmer(local_discovery_1024 ~ condition + (1|trajectory), data=df1, family=binomial)
anova(full,m1)  # full is not better, so lose condition
anova(full,m2)  # full is sig better, so keep iteration
best <- m1

# what I think the relationship is: as iteration goes up, fewer systems are discovered
summary(m1)
"            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  3.17780    0.37819   8.403  < 2e-16 ***
iteration   -0.59201    0.09882  -5.991 2.09e-09 ***"
# with each iteration, global discovery rate goes down

# check one thing...
m0 <- glmer(local_discovery_1024 ~ condition + iteration + (1|trajectory), data=df1, family=binomial)
anova(full,m0) # well crap, marginally significant interaction... but condition doesn't matter on its own

# k well what's the effect of condition then
summary(m0)
"            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  3.21300    0.41980   7.654 1.95e-14 ***
conditionI  -0.06149    0.30722  -0.200    0.841    
iteration   -0.59002    0.09919  -5.948 2.71e-09 ***"
# condition I yields fewer local discoveries - this means they must re-discover systems from earlier iterations

# prob just go with this:
summary(full)
"                     Estimate Std. Error z value Pr(>|z|)    
(Intercept)            2.7145     0.4617   5.879 4.12e-09 ***
conditionI             0.7780     0.5273   1.476   0.1400    
iteration             -0.4587     0.1111  -4.129 3.64e-05 ***
conditionI:iteration  -0.2226     0.1138  -1.955   0.0506 ."

##############
### GLOBAL ###
##############
full <- glmer(global_discovery_1024 ~ condition * iteration + (1|trajectory), data=df1, family=binomial)

summary(full)  # only iteration is significant
"                      Estimate Std. Error z value Pr(>|z|)    
(Intercept)           2.590798   0.477630   5.424 5.82e-08 ***
conditionI           -0.006004   0.546239  -0.011    0.991    
iteration            -0.845478   0.131147  -6.447 1.14e-10 ***
conditionI:iteration  0.007596   0.138264   0.055    0.956 "
# with each iteration, global discovery rate goes down

##########################################################################
# do stats on the just8

# LOCAL
full <- glmer(local_discovery_1024 ~ condition * iteration + (1|trajectory), data=jf1, family=binomial)
m1 <- glmer(local_discovery_1024 ~ iteration + (1|trajectory), data=jf1, family=binomial)
m2 <- glmer(local_discovery_1024 ~ iteration + (1|trajectory), data=jf1, family=binomial)
m3 <- glmer(local_discovery_1024 ~ condition + (1|trajectory), data=jf1, family=binomial)
anova(full,m1) # full is not better
anova(full,m2) # lose condition
anova(full,m3) # keep iteration

# GLOBAL
full <- glmer(global_discovery_1024 ~ condition * iteration + (1|trajectory), data=jf1, family=binomial)
m1 <- glmer(global_discovery_1024 ~ iteration + (1|trajectory), data=jf1, family=binomial)
m2 <- glmer(global_discovery_1024 ~ iteration + (1|trajectory), data=jf1, family=binomial)
m3 <- glmer(global_discovery_1024 ~ condition + (1|trajectory), data=jf1, family=binomial)
anova(full,m1) # full is not better
anova(full,m2) # lose condition
anova(full,m3) # keep iteration


##########################################################################
# make new dataframe for plotting - stick to the 1024 systems

# plot the number of discoveries that each chain makes by the length of the chain

df <- df1

# columns:
trajectory <- c()
tries <- c()  # this is the number of iterations in the trajectory
local_discoveries <- c()
global_discoveries <- c()
condition <- c()

set_of_trajectory_IDs <- unique(df$trajectory)
length(set_of_trajectory_IDs) # 135 trajectories (45+90=135)

for (i in set_of_trajectory_IDs) {   # for each trajectory in the data
	sub <- subset(df,trajectory==i)
	trajectory <- c(trajectory,i)
	condition <- c(condition,as.character(sub[1,]$condition))
	tries <- c(tries,max(sub$iteration))
	local_discoveries <- c(local_discoveries,sum(sub$local_discovery_1024))
	global_discoveries <- c(global_discoveries,sum(sub$global_discovery_1024))
}

df_discovery <- data.frame(trajectory,condition,tries,local_discoveries,global_discoveries)

############################
# look at result

table(df_discovery$local_discoveries,df_discovery$tries)  # discoveries on left, tries on top
"   2  3  4  5  6  7  8  9 10 11
1  25  0  0  0  0  0  0  0  0  0     all chains of length 2 made 1 discovery (25 chains)
2   0 23  4  1  1  0  0  0  0  0
3   0  0 22  2  0  1  0  0  0  0
4   0  0  0 12  4  2  1  0  0  0
5   0  0  0  0  8  1  1  0  0  0
6   0  0  0  0  0  3  5  1  0  0
7   0  0  0  0  0  0  7  0  0  0
8   0  0  0  0  0  0  7  1  2  0
10  0  0  0  0  0  0  0  0  0  1"





############################
# PLOT

require("tidyverse")

# scatter plot of LOCAL discoveries
df_discovery %>% 
	ggplot(mapping = aes(x=tries,y=local_discoveries)) +
	geom_point(aes(color = factor(condition)), alpha=0.5, position = position_jitter(width=0.2, height=0.2)) +
	scale_color_manual(values = c("red", "blue")) +
	scale_x_continuous(breaks = round(seq(2, 11, by = 1),1)) +
	scale_y_continuous(breaks = round(seq(1, 10, by = 1),1)) +
	labs(title = "Number of systems discovered by chains of different lengths",
		 y = "total local discoveries",
		 x = "length of chain")

# C condition - each try yields a new local discovery
# I condition - some tries yield previous systems

# scatter plot of GLOBAL discoveries
df_discovery %>% 
	ggplot(mapping = aes(x=tries,y=global_discoveries)) +
	geom_point(aes(color = factor(condition)), alpha=0.5, position = position_jitter(width=0.2, height=0.2)) +
	scale_color_manual(values = c("red", "blue")) +
	scale_x_continuous(breaks = round(seq(2, 11, by = 1),1)) +
	scale_y_continuous(breaks = round(seq(1, 10, by = 1),1)) +
	labs(title = "Number of systems discovered by chains of different lengths",
		 y = "total global discoveries",
		 x = "length of chain")

# This result is more interesting and looks nonlinear
# I condition makes a lot of global discoveries on iteration 8
# I chains seem more likely to make a global discovery on every try than C chains are.
# Reminder: globalness is computed relative to all other systems in the SAME condition only.

# K so yeah I think the I condition is making more global discoveries because they are conducting a more diverse search.

# These plots aren't that great either - 
# I want to visualize diversity within and between chains, in each codition.
# Are the C chains more homogenous than the I chains are?
# It's hard to entertain these types of differences while knowing that C and I have the same aggregate discovery rates.

# Other note:
# It's strange there's such a crisp diagonal line (i.e. upper bound) that's consistently one less discovery that the number of tries.
# Oh yeah that's because of our convergence criteria - all chains end on the duplication and therefore not a new discovery
# sanity check those 25 chains with tries=2 - these are the chains that converged on the 2nd iteration.
subset(df_discovery,tries==2)
# one example:
subset(df1,trajectory==38)
# Also that's why chains of iteration 8 are the only chains that show 8 discoveries
# because some of those chains ended without converging due to our automatic cutoff at iteration 8.




# ex of how to include a regression line w/ggplot2 (currently one line for both conditions)
df_discovery %>% 
	ggplot(mapping = aes(x=tries,y=global_discoveries)) +
	geom_point(aes(color = factor(condition)), alpha=0.5, position = position_jitter(width=0.2, height=0.2)) +
	scale_color_manual(values = c("red", "blue")) +
	scale_x_continuous(breaks = round(seq(2, 11, by = 1),1)) +
	scale_y_continuous(breaks = round(seq(1, 10, by = 1),1)) +
	labs(title = "Number of systems discovered by chains of different lengths",
		 y = "total discoveries",
		 x = "length of chain") +
	stat_summary(fun.data=mean_cl_normal) + 
	geom_smooth(method='lm', formula= y~x)

summary(lm(global_discoveries ~ tries * condition)) # there's no effect of condition, only tries



##########################################################################
# Number of unique category systems from 45 sampled chains in I
##########################################################################
# replicate Andy's code from "data_playing.R"
# and get a 95% confidence interval around the observed discovery rates
# do this for the 1024 system types
 
N <- 100000
ncats <- rep(NA,N)   # total unique category systems found by that sample of 45 chains
ntries <- rep(NA,N)  # put the total number of generations in all sampled chains here (= total tries)
nt <- unique(df1i$trajectory)
for (i in 1:N) {
	samples <- sample(nt,length(unique(df1c$trajectory)),replace=FALSE)  # sample 45 chains from condition I
	ncats[i] <- length(unique(df1i$system1024[df1i$trajectory %in% samples]))
	ntries[i] <- length(df1i$system1024[df1i$trajectory %in% samples])
}

# compare mean(ncats) to the unique systems discovered by the 45 chains in condition C
mean(ncats) # about 113-114
sd(ncats)   # about 9-11
length(unique(df1c$system1024)) # 91 uniques

# compare mean(ntries) to the total number of tries in C
mean(ntries)  # about 216-218
nrow(df1c)    # 207 tries

# do the extra tries in I account for the higher mean number of unique systems found?
# 10 more tries get you 4.5 more systems (using the I rate of 0.4528736)
# but I is showing 113-91 = 22 more systems discovered, so 10 tries falls short.

# coz otherwise the discovery rates are pretty similar:
length(unique(df1$system1024))/nrow(df1)    # 237 / 642 = 0.3691589
length(unique(df1i$system1024))/nrow(df1i)  # 197 / 435 = 0.4528736   I discovery rate for system1024
length(unique(df1c$system1024))/nrow(df1c)  #  91 / 207 = 0.4396135   C discovery rate for system1024

# 217 tries * 0.4528736 discovery rate = 98 discoveries expected - strange that the samples are higher at 113-114 discoveries

# t-test
ncatT <- t.test(ncats, mu=length(unique(dfc$system1024)))
# lower bound on confidence interval
round(ncatT$conf.int[[1]],2)

############################
# 95% confidence intervals by sorting MCMC results (run for N = 10,000)
min(ncats)  # 70
max(ncats)  # 154

# return the value at the lower or upper bound of the 95% confidence interval
bound95 <- function(my_list,type) {
	sorted <- sort(my_list)
	if (type == "lower") { result <- sorted[ceiling(length(my_list)*.025)+1] }
	if (type == "upper") { result <- sorted[floor(length(my_list)*.925)] }
	return(result)
}

bound95(ncats,"lower")  # 94  - these stay the same with N = 100,000 runs
bound95(ncats,"upper")  # 129

# C found 91 of the 1024 systems and that's out of the confidence intervals
length(unique(df1c$system1024))

# get 95% CI on the rates also
rates <- ncats/ntries
bound95(rates,"lower")  # 0.4527363
bound95(rates,"upper")  # 0.5765766

# ok finally this is the answer I need to convince myself that the rates in C and I are not "the same"
# the observed I rate is not in the middle of the 45 subsamples - it's on the low end!
# that's why I keep getting weirded out by I finding more systems when subsampling the 45
# but this is also weird because I've never run MCMC confidence intervals where the observed mean wasn't smack in the middle of the distribution of samples!

hist(rates)
abline(v=0.4528736,col="red")

############################
# same deal for 512 systems: condition I seems to be finding more of them
N <- 10000
ncats <- rep(NA,N)   # total unique category systems found by that sample of 45 chains
ntries <- rep(NA,N)  # put the total number of generations in all sampled chains here (= total tries)
nt <- unique(df1i$trajectory)
for (i in 1:N) {
	samples <- sample(nt,length(unique(df1c$trajectory)),replace=FALSE)  # sample 45 chains from condition I
	ncats[i] <- length(unique(df1i$system512[df1i$trajectory %in% samples]))
	ntries[i] <- length(df1i$system512[df1i$trajectory %in% samples])
}

# I still finding ~20 more systems than C is
mean(ncats) # about 95-96
sd(ncats)   # about 9-11
length(unique(df1c$system512)) # 75 uniques

bound95(ncats,"lower")  # 77
bound95(ncats,"upper")  # 109

# C found 75 of the 512 systems and that's out of the confidence intervals
length(unique(df1c$system512))

############################
# alright so based on this, now I'm thinking that the I condition actually is exploring more unique systems...
