
df1 <- read.csv("/Users/vanferdi/Library/Mobile Documents/com~apple~CloudDocs/Research/PROJECTS/Category Change/GITHUB REPO/Data/experiment1_FINAL.csv", colClasses=c(system1024="character"))
df2 <- read.csv("/Users/vanferdi/Library/Mobile Documents/com~apple~CloudDocs/Research/PROJECTS/Category Change/GITHUB REPO/Data/experiment2_FINAL.csv", colClasses=c(system1024="character"))
	
df1i <- subset(df1,condition=="I")
df1c <- subset(df1,condition=="C")
df2i <- subset(df2,condition=="I")
df2c <- subset(df2,condition=="C")

##########################################################################
# Discovery Rate
##########################################################################

# attested systems in each condition and experiment
# these do not include the generation zero initial systems.  system512 is the output of each participant.

# Experiment 1
length(unique(df1$system512))/nrow(df1)    # 188 / 642 = 0.293 - the probability that each try results in discovery of a new system
length(unique(df1i$system512))/nrow(df1i)  # 159 / 435 = 0.366
length(unique(df1c$system512))/nrow(df1c)  #  75 / 207 = 0.362

# Experiment 2
length(unique(df2$system512))/nrow(df2)    # 183 / 692 = 0.264 - interesting that the total % is lower
length(unique(df2i$system512))/nrow(df2i)  # 151 / 469 = 0.322 - means they had diff exploratory niches?
length(unique(df2c$system512))/nrow(df2c)  #  74 / 223 = 0.332

# there was no difference in the size of the space explored
# (but remember there was a difference in the diversity of nbound types explored - violin plot)

##########################
# Experiment 1
# what systems are in both C and I
intersect(df1i$system512,df1c$system512)  # 46
length(unique(intersect(df1i$system512,df1c$system512)))  # 46 unique systems

# what systems were only discovered in C (and not I)?  
# setdiff(a,b) returns what is in a and not in b
unique(setdiff(df1i$system512,df1c$system512))    # 113 systems in I that are not in C
unique(setdiff(df1c$system512,df1i$system512))    #  29 systems in C that are not in I

# visually, they look like a smattering of the same thing

# do they differ by nbound?

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

# baseline number of 512 systems in each n boundaries bin: choose(n,k) where n = 9 (max bounds)
choose(9,seq(0,9))
" 0  1  2  3  4   5   6  7  8  9 = k
  1  9  36 84 126 126 84 36 9  1
"

table(get_n_boundaries(unique(df1$system512)))
"  1  2  3  4  5  6  7  8 
   9 26 50 47 34 14  6  2 "

##########################
# should I be inlcuding the init systems in this or not?

# number of unique systems including the init set
length(unique(c(df1$system512,df1$system512_input)))   # 239
length(unique(c(df1i$system512,df1i$system512_input))) # 206
length(unique(c(df1c$system512,df1c$system512_input))) # 106

# each row is a try - the init systems weren't the product of a try
# I think don't include the initial sets as discoveries

##########################################################################
# Discovery rate over time
##########################################################################

# get the cumulatively unique systems per iteration - ignore the init systems coz they weren't "discovered"

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
rate_plot <- function(e1i,e1c,e2i,e2c,title) {
	time <- seq(1,15)
	plot(time,e2c,las=1,ylim=c(0,1),type="l",lty="dotted",col="red",ylab="proportion novel",xlab="iteration",main=title)
	lines(e1i,type="l")
	lines(e1c,type="l",lty="dotted")
	lines(e2i,type="l",col="red")
	legend(10, 1, cex=0.8, legend=c("Exp1 individual", "Exp1 cultural","Exp2 individual", "Exp2 cultural"), 
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

rate_plot(e1i_512,e1c_512,e2i_512,e2c_512,"discovery rate - 512 systems")

################################################
# Discovery rate per iteration - 1024 system
################################################

# Experiment 1
e1i_1024 <- cumulative_discovery_rate(df1i,1024)
e1c_1024 <- cumulative_discovery_rate(df1c,1024)

# Experiment 2
e2i_1024 <- cumulative_discovery_rate(df2i,1024)
e2c_1024 <- cumulative_discovery_rate(df2c,1024)

rate_plot(e1i_1024,e1c_1024,e2i_1024,e2c_1024,"discovery rate - 1024 systems")


##########################################################################
# Proportion of the space discovered over time
##########################################################################

# Out of the 512 total systems, how many have been discovered by iteration x?
# This is cumulative so iteration 5 contains all uniques discovered in iteration 1-5

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

rate_plot(a,b,c,d,"expansion rate - 512 systems")

################################################
# Expansion rate - 1024 system
################################################

a <- space_discovery(df1i,1024)
b <- space_discovery(df1c,1024)
c <- space_discovery(df2i,1024)
d <- space_discovery(df2c,1024)

rate_plot(a,b,c,d,"expansion rate - 1024 systems")


################################################
# Expansion rate - 512 system (45 chains from I)
################################################

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

rate_plot(a,b,c,d,"expansion rate - 512 systems (45 from I)")

# important note:
# each chain has a variable number of tries (iterations) in it
# so matching chain number doesn't actually match the number of tries
# compare:
nrow(sampleI45(df1))
nrow(df1c)

# k but I is still hitting a higher proportion of novel ones,
# even when it's total tries is lower than the tries in C (always 207 above)
# (run the above code over and over to see this)


################################################
# Expansion rate - 1024 system (45 chains from I)
################################################

a <- space_discovery(sampleI45(df1),1024)
b <- space_discovery(df1c,1024)
c <- space_discovery(sampleI45(df2),1024)
d <- space_discovery(df2c,1024)

rate_plot(a,b,c,d,"expansion rate - 1024 systems (45 from I)")

##########################################################################
# Questions
##########################################################################

# how on earth are these discovery rates similar?
# coz the I condition ones are searching more complex systems PLUS all the less complex ones




##########################################################################
# Sanity check for the 1024 systems
##########################################################################

# I want to stick to the 512 systems for all analyses
# But let's make sure we get the same discovery rates for the 1024 systems.

head(df1$system1024) # double check these read in correctly as strings

# Experiment 1
length(unique(df1$system1024))/nrow(df1)    # 237 / 642 = 0.369
                                            # 188 / 642 = 0.293 <- for system512
length(unique(df1i$system1024))/nrow(df1i)  # 197 / 435 = 0.453
                                            # 159 / 435 = 0.366 <- for system512
length(unique(df1c$system1024))/nrow(df1c)  #  91 / 207 = 0.440
                                            #  75 / 207 = 0.362 <- for system512

# 512 things to discover in 642 tries vs 1024 things to discover in 642 tries.

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

sum(il)/length(il)  # 0.737931   local discoveries per try, I
sum(cl)/length(cl)  # 0.7536232                             C    
sum(ig)/length(ig)  # 0.5103448 global discoveries per try, I
sum(cg)/length(cg)  # 0.5120773                             C

# the probability that any try will be a local discovery is ~ 0.75 and a global discovery is ~ 0.5

# double check that 512 is approx identical
il <- df1i$local_discovery_512 
cl <- df1c$local_discovery_512
ig <- df1i$global_discovery_512 
cg <- df1c$global_discovery_512

sum(il)/length(il)  # 0.737931   local discoveries per try, I
sum(cl)/length(cl)  # 0.7536232                             C    
sum(ig)/length(ig)  # 0.4482759 global discoveries per try, I
sum(cg)/length(cg)  # 0.4541063

# this makes sense:
# the local scores don't change because people don't reverse the label mapping in one chain
table(df1i$local_discovery_1024,df1i$local_discovery_512) # see, no mismatch in local scores at all
# but the global scores are lower because they do discover sister systems
table(df1i$global_discovery_1024,df1i$global_discovery_512) # 27 512 "olds" are new according to 1024
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
# condition I means fewer local discoveries?

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
# make new dataframe for plotting - stick to the 1024 systems

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
1  25  0  0  0  0  0  0  0  0  0     25 chains of length 2 made 1 discovery
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





# scatter plot of GLOBAL discoveries - include a regression line
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






