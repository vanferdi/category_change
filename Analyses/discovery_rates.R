
df1 <- read.csv("/Users/vanferdi/Library/Mobile Documents/com~apple~CloudDocs/Research/PROJECTS/Category Change/GITHUB REPO/Data/experiment1_FINAL.csv", colClasses=c(system1024="character"))
df2 <- read.csv("/Users/vanferdi/Library/Mobile Documents/com~apple~CloudDocs/Research/PROJECTS/Category Change/GITHUB REPO/Data/experiment2_FINAL.csv", colClasses=c(system1024="character"))
	
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










