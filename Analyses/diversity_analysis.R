# Research question:
# do participants in the "cultural" (C) vs "individual" (I) condition explore the space of all possible categories differently?
# and are they more selective (i.e. less exploratory) than a random baseline?

# what data to use:
# take all systems produced by each participant and look at the number of category boundaries in that system
# but only use iterations 1 through 8, because some C condition chains ran longer than 8. All I chains were capped at 8.
# why? data with iterations > 8 had more time to evolve than data capped at 8 iterations, so that would be an unfair comparison.

# operationalized research question:
# what is the distribution over the number of category boundaries (N_boundaries) produced in each condition (C or I)?
# find the Shannon entropy of the distribution of N boundaries (DNB).
# compare them to the Shannon entropy of the baseline DNB (baseline defined below).

# operationalize "more" or "less exploratory":
# more exploratory (than x) = higher Shannon entropy of DNB (than the Shannon entropy of x's DNB)
# less exploratory (than x) = lower Shannon entropy of DNB (than the Shannon entropy of x's DNB)
# shorthand: H(DNB) means the "Shannon entropy of the distribution over N boundaries", in a given set of category systems.

# operationalize the words "statistically significant"
# do 100,000 MCMC resamples of the DNB in each condition (C and I)
# and calculate the new, minimax corrected H(DNB) for each resample
# pull the 95% confidence interval from those samples, meaning rank the 100,000 samples low to high:
# lower bound of the 95% confidence interval = sample #2500 in the ranking
# upper bound of the 95% confidence interval = sample #97500 in the ranking
# see function "MCMC" for full definition of the above.
# if the confidence intervals do no overlap, say the two conditions are significantly different
# if the confidence intervals overlap, say we have no evidence that they are different (they still could be)
# see here for the above interpretation:
# Schenker, N., & Gentleman, J. F. (2001). On judging the significance of differences by examining the overlap between confidence intervals. The American Statistician, 55(3), 182-186.

# report the first MCMC run!
# but then do 10 more and report the average of those, to let people decide for themselves whether the first one was "representative" or not.

# random baseline
# there are lots of ways that something can be "random", my definition here will be:
# what is the DNB that would be produced if participants were randomly clicking on answers in all the test trials?
# that DNB will be called the baseline distribution
boundaries_landscape <- 2*choose(9,seq(0,9))
boundaries_landscape_pmf <- boundaries_landscape/sum(boundaries_landscape)

# Two things to look for:

# 1) see if the confidence intervals for C and I overlap
# if yes, say there is no evidence for a difference between exploratory behavior in the two conditions
# if no, say there is a significant difference between exploratory behavior in the two conditions and report which one is more exploratory (i.e. has the higher Shannon entropy)

# 2) see if the C and/or I confidence intervals include the baseline entropy
# if one includes the baseline entropy, say it's level of exploration didn't differ from random clicking during testing
# if one does include the baseline entropy:
# if the entropy of C or I was lower, say it was less exploratory than the baseline
# if the entropy of C or I was higher, say it was more exploratory than the baseline

#########################################################
# STEP 1
#########################################################

# here is the dataframe from the last experiment
# all examples below will be calculated using this data
demo_df <- read.csv("./experiment1_data_just8.csv")

#########################
# this block added after pre-registration:
# here's the data to run this on
#demo_df <- subset(readRDS("../Data/experiment1.rds"),iteration < 9)
#demo_df <- subset(readRDS("../Data/experiment2.rds"),iteration < 9)
#########################

# load support libraries and functions
library(entropy) # for entropy estimators

H <- function(X,base) {
    X <- X[X != 0 ] # remove all zeros
    return(-sum(X*log(X,base)))
}

mytable <- function(array,range_start,range_stop) {
    return(table(factor(array, levels = range_start:range_stop)))
}

#########################################################
# STEP 2
#########################################################

# calculate the 3 Shannon entropy values, baseline, C, and I.
# calculate raw and corrected Shannon entropy values for all 3
# report the corrected values for C and I (coz they are empirical data - finite samples have lower entropy than their source)
# report the raw value one for the baseline (coz this is the theoretically known probability distribution of the source)
# Use minimax correction. I need to choose one correction type. I chose minimax because it sounds cool.

# get baseline DNB
boundaries_landscape <- 2*choose(9,seq(0,9)) # the null space
boundaries_landscape_pmf <- boundaries_landscape/sum(boundaries_landscape)
baseline <- H(boundaries_landscape_pmf,2)                              # 2.629928 bits

# get DNB for condition C
dc <- subset(demo_df,condition=="C") # get just the C condition data
C_landscape <- mytable(dc$N_boundaries,0,9)
C_landscape_pmf <- C_landscape/sum(C_landscape)
C_ent <- entropy(C_landscape, unit="log2")                             # 1.969054
C_ent_estimate <- entropy(C_landscape, unit="log2", method="minimax")  # 2.174893

# get DNB for condition I
di <- subset(demo_df,condition=="I") # get just the I condition data
I_landscape <- mytable(di$N_boundaries,0,9)
I_landscape_pmf <- I_landscape/sum(I_landscape)
I_ent <- entropy(I_landscape, unit="log2")                             # 2.347838
I_ent_estimate <- entropy(I_landscape, unit="log2", method="minimax")  # 2.471043

#############################################
# some important notes:

# sanity check the library's entropy function
H(C_landscape_pmf,2)                  # 1.969054 bits
entropy(C_landscape_pmf, unit="log2") # 1.969054
entropy(C_landscape, unit="log2")     # 1.969054

# entropy library manual says the input needs to be counts in table format (not probabilities) for the estimator methods
entropy(C_landscape_pmf, unit="log2", method="minimax") # 2.981651  WRONG
entropy(C_landscape, unit="log2", method="minimax")     # 2.174893  CORRECT

#########################################################
# STEP 3
#########################################################
# get MCMC confidence intervals

MCMC <- function(data,N_resamples,type) { # data is gonna be the N_boundaries column from a dataframe
    sample_ents <- c()
    for (i in 1:N_resamples) {
        
        # sample from data with replacement
        sam <- sample(data,replace=TRUE)
        
        # get dist of n boundaries
        dist <- mytable(sam,0,9)
        
        # calculate entropy of that sample
        if (type == "plugin") { ent <- entropy.empirical(dist, unit="log2") }
        if (type == "estimate") { ent <- entropy(dist, unit="log2", method="minimax") }
        sample_ents <- c(sample_ents,ent)
        
    }
    ranked <- sort(sample_ents)
    upper_bound <- ranked[N_resamples*.975]
    lower_bound <- ranked[N_resamples*.025]
    
    return(c(upper_bound,lower_bound))
}

# use the two data subsets for iterations <= 8
# I condition = di
# C condition = dc
# and use the N_boundaries column from the dataframe so MCMC resamples that data

MCMC(di$N_boundaries,100000,type="plugin")
MCMC(dc$N_boundaries,100000,type="plugin")

# first run:
# I condition: 2.428651 2.239489
# C condition: 2.134517 1.738572

# RESULT
MCMC(di$N_boundaries,100000,type="estimate")
MCMC(dc$N_boundaries,100000,type="estimate")

# first run:
# I condition: 2.543619 2.375812
# C condition: 2.321119 1.983724

# Next 10 for the I condition
# 2.543750 2.375807
# 2.543922 2.375531
# 2.543374 2.376475
# 2.543177 2.376046
# 2.543388 2.375855
# 2.544047 2.375334
# 2.543816 2.376145
# 2.543586 2.375508
# 2.543967 2.376144
# 2.543485 2.375473

# Next 10 for the C condition
# 2.319723 1.983885
# 2.320807 1.982811
# 2.320223 1.983160
# 2.319669 1.981735
# 2.320692 1.983401
# 2.319889 1.983597
# 2.322059 1.982475
# 2.320318 1.983582
# 2.320403 1.983227
# 2.320057 1.983860

#########################################################
# END
#########################################################
