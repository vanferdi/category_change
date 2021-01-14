# create a random label flipping process
# keep track to the transition probabilities between system types
# use as random baseline for the observed transition probabilites
# will tell you how the human adjecent possible space differs from a standard theoretical a.p. space

require(gtools) # for permutations()
require(stringr)

##########################################################################
# get and initial set to mutate
##########################################################################
# can be the 1024 or the random initial ones only

get_1024 <- function() {
	# generate one of each of the 1024 systems
	x <- permutations(2, 10, c(0,1), repeats.allowed=TRUE)
	# collapse each row into a string
	the_set <- apply(x,1,paste,collapse="")
	return(the_set)
}

# example usage:
set1024 <- get_1024()

get_inits <- function() {
	inits <- c()
	set1024 <- get_1024()
	for (i in set1024) {
		if (str_count(i,"0") == 5) {
			inits <- c(inits,i)
		}
	}
	return(inits)
}

# example usage:
inits <- get_inits()

##########################################################################
# mutation process
##########################################################################

# for each string, randomly flip one of its elements to the other binary value

rando_flipper <- function(set) { # input = a list of strings
	new_set <- c()
	for (i in set) {
		rand <- sample(seq(1,10),1)
		chosen <- substr(i,rand,rand)
		if (chosen == "0") { new_val = "1" } else { new_val = "0" }
		substr(i,rand,rand) <- new_val  # changes string in place
		new_set <- c(new_set,i)
	}
	return(new_set)
}

# example usage
rando_flipper(set1024)

##########################################################################
# get a whole bunch of input sets
##########################################################################

repeats <- 10
# repeat the set several times and concatenate them all into a column called inputs
inputs <- rep(get_1024(),repeats)

# then mutate them all into a column called outputs
outputs <- rando_flipper(inputs)

d <- data.frame(inputs,outputs)


##########################################################################
# add columns to the dataframe
##########################################################################

# compute number of boundaries per system
get_N_boundaries <- function(d) {
	boundaries01 <- str_count(d, pattern = "01")
	boundaries10 <- str_count(d, pattern = "10")
	return(boundaries01+boundaries10)
}

# classify type of system
get_systype <- function(d) {
	return(ifelse(d==0, "-degenerate", ifelse(d==1, "continous", "disjoint")))
}

add_cols <- function() {
	input_N <- get_N_boundaries(d$inputs)
	output_N <- get_N_boundaries(d$outputs)
	d <- cbind(d,input_N,output_N)
	input_type <- get_systype(d$input_N)
	output_type <- get_systype(d$output_N)
	d <- cbind(d,input_type,output_type)
	return(d)
}

##########################################################################
# classification of the 1024
##########################################################################

table(get_systype(get_N_boundaries(get_1024())))
"degenerate   continous    disjoint 
 2            18           1004             "

# as probability distribution
table(get_systype(get_N_boundaries(get_1024())))/sum(table(get_systype(get_N_boundaries(get_1024()))))
"0.001953125 0.017578125 0.980468750"

##########################################################################
# ANALYZE - 1024 inputs
##########################################################################

# theoretically this needs to be the same as the distribution in the 1024 set
# sanity check:

# create dataframe
repeats <- 10
inputs <- rep(get_1024(),repeats)
outputs <- rando_flipper(inputs)
d <- data.frame(inputs,outputs)
d <- add_cols()

# get transition probabilties - among types
counts <- table(d$input_type,d$output_type)
p <- counts/rowSums(counts)

"            degenerate   continous    disjoint
degenerate  0.000000000 0.250000000 0.750000000
continous   0.033333333 0.216666667 0.750000000
disjoint    0.001195219 0.013247012 0.985557769  "

# get stationary distribution (but this is just based off of one iteration from an initial set)
stat_dist(p)   
"0.001744247 0.017148261 0.981107492"
# should be the same as the probabilty distribution in the section above

table(d$output_type)

# one repeat
# differences are the sampling error on the 1024 set
"degenerate   continous    disjoint 
 4            20           1000         "

# ten repeats
# divide by ten - yep this is the same distribution
"degenerate   continous    disjoint 
 19           181          10040         "

##########################################################################
# classification of the random initial inputs
##########################################################################

table(get_systype(get_N_boundaries(get_inits())))
"continous  disjoint 
         2       250  "

# as probability distribution
table(get_systype(get_N_boundaries(get_inits())))/sum(table(get_systype(get_N_boundaries(get_inits()))))
"  continous    disjoint 
 0.007936508 0.992063492"

##########################################################################
# ANALYZE - random initial inputs
##########################################################################

# create dataframe
repeats <- 1000
inputs <- rep(get_inits(),repeats)
outputs <- rando_flipper(inputs)
d <- data.frame(inputs,outputs)
d <- add_cols()

# get transition probabilties - among types
counts <- table(d$input_type,d$output_type)
p <- counts/rowSums(counts)
p

"         continous disjoint       repeats <- 100
continous   0.22000  0.78000
disjoint    0.00832  0.99168 "

"         continous disjoint       repeats <- 1000  - took like 15 min to run?
continous  0.201000 0.799000
disjoint   0.008084 0.991916 "

# get stationary distribution (but this is just based off of one iteration from an initial set)
stat_dist(p)
"0.01001631 0.98998369"  # based on the run with repeats <- 1000

##########################################################################
# Iterate it
##########################################################################

# iterate the one with the random initial systems
# and see how to best approximate that stationary distribution
# is the method above (presumably with the effect of initial conditions in there) sufficient?
# (this is totally some other theoretical question about estimating stationary distributions from observed lineages)







##########################################################################
# Summary
##########################################################################

# types in the 1024 set
"2  18  1004"

# types in one mutation cycle on the 1024 set: theoretically identical to the above

# types in one mutation cycle on the random initial systems set



# Experiment 1
table(df1$systype_output)
" 0  274  368"

"            degenerate continuous disjoint
degenerate            0          0        0
continuous            0        177       14
disjoint              0         97      354
"

# Experiment 2
table(df2$systype_output)
"33  322  337"

"            degenerate continuous disjoint
degenerate           14          2        3
continuous            4        186       43
disjoint             15        134      291
"


#############################
# stationary distributions:

"degenerate  continuous  disjoint"
"0.001953125 0.017578125 0.980468750"    # the 1024 set as pdf
"0.001744247 0.017148261 0.981107492"    # the 1024 single flip simulation stat dist (should be same)
"0           0.007936508 0.992063492"    # the random init set as pdf
"0           0.01001631  0.98998369 "    # random init single flip simulation stat dist (didn't hafta be the same)

# k these two baselines are really similar

"0.0435669   0.6264775   0.3299556"      # Experiment 1 and 2 pooled together stat dist
"0           0.7458234   0.2541766"      # Experiment 1 stat dist
"0.08228323  0.56916726  0.34854951"     # Experiment 2 stat dist

# k these are really different that the baselines above

##########################################################################
# END
##########################################################################
