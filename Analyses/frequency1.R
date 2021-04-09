########################################################################################
# CONTENTS
########################################################################################

# 1) read in data
# 2) process data in dataframe for stats
# 3) the stats

########################################################################################
# 1) READ IN DATA
########################################################################################

df <- read.csv("../Data/experiment1_FINAL.csv")

# subset to just look at the first 8 iterations
# there were just a couple iterations in the cultural condition that went over 8
# we toss those out because no one in the individual condition was allowed to go beyond 8 iterations
df <- subset(df,iteration<9)
table(df$iteration) # check

########################################################################################
# 2) PROCESS DATA
########################################################################################

# create a new dataframe (called d) from df - each row is a stimulus

################
# define what the columns in d are gonna be

iteration <- c()        # generation (cultural condition) or round (individual condition) - this is the "age" of the category system
participant <- c()      # unique participant ID
lineage <- c()          # unique ID of the chain 
skew <- c()             # S = one of the skewed frequency distributions, U = the uniform one
condition <- c()        # C = cultural, I = individual
trial <- c()            # test trial number (ranges 1 through 10)

RT <-c()                # time spent on the current test trial stim in milliseconds
stim_color <- c()       # greyscale code of the image in the current test trial
# df$testset values map to this array of greyscale colors:
greys <- c(25,50,75,100,125,150,175,200,225,250)
# ex: testset 0 = greyscale 25, testset 4 = greyscale 125
# 25 is almost black, 250 is almost white
stim_color_rescale <- c()  # this one rescales stim color to a 0-1 range

stim_frequency <- c()   # frequency of the stim in the current test trial during training (normalized 0 to 1)
L_counts <- c(10,5,4,3,2,2,1,1,1,1)  # frequency per stim in the left skew condition
L_counts <- L_counts/sum(L_counts)
R_counts <- rev(L_counts)            # frequency per stim in the right skew condition

# is the current stimulus on the edge of a category boundary? According to the category system people produced on that testing round.
# uses the function defined below system_to_bounded()
bounded <- c()          # 0 = no. Both neighbors to the stim have same category label as the stim.
# 1 = yes. At least one of the neighbors to the stim had a difference category label.

################
# define extra functions

# input: a category system in string format (see df$system512)
# ex: "1100000001" where 0 = category A label, like "lem", 1 = category B label, like "vit"
# the order of the 10 labels map on to the greyscale color of each stim, defined in variable "greys" above.

# output: a different binary string format where
# 0 = stim has no boundary adjacent to it, 1 = stim has boundary adjacent
# example output for the input above: "0110000011"

system_to_bounded <- function(system_string) { # system = a string, ex: "1100000001"
    result <- c() # result is an indexable array of binary digits
    s <- as.numeric(strsplit(system_string, split="")[[1]])
    
    m <- length(s)
    # if the two labels are the same, assign 0. If the two lables are different, assign 1.
    if ( s[1] == s[2] ) { result[1] <- 0 } else { result[1] <- 1 } # first one
    if ( s[m] == s[m-1] ) { result[m] <- 0 } else { result[m] <- 1 } # last one
    
    # if the three labels are the same, assign 0.  Otherwise, assign 1.
    for (j in 2:(m-1)) { # the ones in the middle
        if ( s[j] == s[j-1] && s[j] == s[j+1] ) { result[j] <- 0 } else { result[j] <- 1 }
    }
    return(result)
}

# example usage:
# system_to_bounded("1100000001")

################
# fill columns

for (r in 1:length(df$X)) { # for each row in the original data frame (= the result of one round)
    
    dis <- df[r,]$distribution
    
    # get the elements to break up per test trial in the loop below
    rts <- df[r,]$test_RTs
    sti <- df[r,]$testset
    
    # then break them up into an indexable array
    rts <- as.numeric(strsplit(toString(rts),",")[[1]])
    sti <- as.numeric(strsplit(toString(sti),",")[[1]])
    
    bounded_codes <- system_to_bounded(toString(df[r,]$system512))
    
    for (s in 1:10) { # for each stimulus in the testing set
        
        # use as.character() to keep these as factors - otherwise they convert to numerics
        lineage <- c(lineage,as.character(df[r,]$trajectory)) 
        participant <- c(participant, as.character(df[r,]$participant))
        condition <- c(condition, as.character(df[r,]$condition))
        iteration <- c(iteration, df[r,]$iteration)
        trial <- c(trial,s)
        
        if (dis == "U") { skew <- c(skew, "U") } else { skew <- c(skew, "S") }
        
        RT <- c(RT, rts[s])
        stim_color <- c(stim_color, greys[sti[s]+1]) # +1 coz testset starts at zero
        
        # work out what the stim frequency was
        if (dis == "L") { fre <- L_counts[sti[s]+1] }
        if (dis == "R") { fre <- R_counts[sti[s]+1] }
        if (dis == "U") { fre <- 3/30 }
        stim_frequency <- c(stim_frequency, fre)
        
        # pull out the bounded code
        bounded <- c(bounded, bounded_codes[sti[s]+1])
    }
}

# re-scale color from range 25-250 to 0-1
stim_color_rescale <- (stim_color-25)/225
# summary(stim_color_rescale)

d <- data.frame(lineage,iteration,participant,trial,stim_color,stim_color_rescale,stim_frequency,RT,bounded,skew,condition)

# make sure everything you want to be a factor is a factor
#d$participant <- as.factor(d$participant)

d$bounded <- as.factor(d$bounded)

################
# some quality control checks

# original data frame
length(unique(df$participant)) # 288 unique participants

length(unique(d$participant)) # 288 unique participants
length(unique(d$lineage)) # 135 unique lineages


########################################################################################
# 3) STATS
########################################################################################

require(lme4)

# this is the model structure I want:
# bounded ~ stim_frequency * stim_color * skew * condition * iteration  + (1|participant) + (1|lineage)

# bounded: is the current test trial stim have a category boundary next to it or not?  binary variable.
# stim_frequency: what was the frequency of this stim in the training phase?  ranges 0 to 1
# skew: the frequency condition, coded "S" for skew and "U" for uniform
# condition: the cultural or individual transmission condition
# iteration: the generation / round / age of the category system
# (1|participant): random effect for participant, because test trials are grouped into participants
# (1|lineage): random effect for lineage, because test trials are grouped into lineages (1 lineage = 1 iterated learning chain)

# Questions:
# 1) Can I leave out skew? That info is basically all in the stim_frequency variable - or is it?
# 2) Should I only run this model on the data from the two skewed conditions? (and also leave out skew because of that)

# need to run a logit regression because the dependent variable is binary.
# do that with glmer() and specifying family = "binomial"

full <- glmer(bounded ~ stim_frequency * stim_color * skew * condition * iteration  + (1|participant) + (1|lineage), data=d, family = "binomial")

# Problem: model didn't converge!
# but you can still look at the outcome:
summary(full)

"Fixed effects:
Estimate Std. Error z value Pr(>|z|)  
(Intercept)                                   -0.6245549  0.8936370  -0.699   0.4846  
stim_frequency                                 3.0157247  5.3495232   0.564   0.5729  
stim_color                                    -0.0015618  0.0053494  -0.292   0.7703  
skewU                                          1.7636507  1.0571039   1.668   0.0952 .
condition                                     -0.0293868  0.5087619  -0.058   0.9539  
iteration                                     -0.2937655  0.2396105  -1.226   0.2202  
stim_frequency:stim_color                      0.0073400  0.0323565   0.227   0.8205  
stim_color:skewU                              -0.0095965  0.0059212  -1.621   0.1051  
stim_frequency:condition                      -1.0740860  3.0622923  -0.351   0.7258  
stim_color:condition                           0.0028483  0.0030524   0.933   0.3508  
skewU:condition                               -0.5663632  0.6077377  -0.932   0.3514  
stim_frequency:iteration                      -0.7625273  1.6251765  -0.469   0.6389  
stim_color:iteration                           0.0011168  0.0014579   0.766   0.4437  
skewU:iteration                               -0.3482901  0.2593043  -1.343   0.1792  
condition:iteration                            0.1846463  0.1332035   1.386   0.1657  
stim_frequency:stim_color:condition           -0.0077485  0.0183283  -0.423   0.6725  
stim_color:skewU:condition                     0.0036028  0.0034005   1.060   0.2894  
stim_frequency:stim_color:iteration           -0.0108300  0.0101689  -1.065   0.2869  
stim_color:skewU:iteration                     0.0037075  0.0015923   2.328   0.0199 *
stim_frequency:condition:iteration            -0.3931367  0.9127886  -0.431   0.6667  
stim_color:condition:iteration                -0.0009712  0.0008165  -1.190   0.2343  
skewU:condition:iteration                      0.0952511  0.1475952   0.645   0.5187  
stim_frequency:stim_color:condition:iteration  0.0088872  0.0055617   1.598   0.1101  
stim_color:skewU:condition:iteration          -0.0017167  0.0009090  -1.889   0.0589 ."

# NOTE: the above output took 2 minutes to run on my laptop
# then I realized that it was treating participant and lineage as continuous numeric variables
# so I fixed it so they are factors now
# but now the model just runs on my laptop without stopping, fyi - in case it's doing that for you too right now.


################
# now run it on the data from the skew conditions only

# because I really just want to see if there's an effect of stim frequency on category boundary location
# (the frequent stim should attract more category boundaries).
# All of the U condition data stim have equal stim_frequency (3/10 = 0.3)
# So is it weird to inlcude that data in the analysis anyway?

# save skewed data only in new data frame called "ds"
ds <- subset(d,skew=="S")

# sanity check that all the U data is dropped
table(d$skew) 
table(ds$skew) 

# new model
m2 <- glmer(bounded ~ stim_frequency * stim_color * condition * iteration + (1|participant) + (1|lineage), data=ds, family = "binomial")

summary(m2)

# model also failed to converge.

"Fixed effects:
Estimate Std. Error z value Pr(>|z|)
(Intercept)                                   -0.6210102  0.8865394  -0.700    0.484
stim_frequency                                 2.9489231  5.3330180   0.553    0.580
stim_color                                    -0.0015730  0.0053345  -0.295    0.768
condition                                     -0.0319201  0.5045854  -0.063    0.950
iteration                                     -0.2941240  0.2392897  -1.229    0.219
stim_frequency:stim_color                      0.0077603  0.0322427   0.241    0.810
stim_frequency:condition                      -1.0353655  3.0528485  -0.339    0.734
stim_color:condition                           0.0028459  0.0030436   0.935    0.350
stim_frequency:iteration                      -0.7367687  1.6214499  -0.454    0.650
stim_color:iteration                           0.0011372  0.0014558   0.781    0.435
condition:iteration                            0.1859119  0.1330055   1.398    0.162
stim_frequency:stim_color:condition           -0.0079708  0.0182621  -0.436    0.662
stim_frequency:stim_color:iteration           -0.0110023  0.0101500  -1.084    0.278
stim_frequency:condition:iteration            -0.4048206  0.9107453  -0.444    0.657
stim_color:condition:iteration                -0.0009802  0.0008152  -1.202    0.229
stim_frequency:stim_color:condition:iteration  0.0089676  0.0055509   1.615    0.106"

# NOTE: this output is also from participant and lineage as continuous niumeric variables.

# and nothing was significant in this model.

################
# These aren't converging because there are so many parameters with these freakin interactions.
# Let's just run a too-simple model and see if it converges.
# Does the too-simple model tell a coherent story at all?

# at the most basic level, I just want to know if/how stim_frequency and stim_color affect bounded

m3 <- glmer(bounded ~ stim_frequency * stim_color_rescale + (1|participant) + (1|lineage), data=ds, family="binomial")
summary(m3)

"                          Estimate Std. Error z value Pr(>|z|)    
(Intercept)                -0.5713     0.1250  -4.572 4.83e-06 ***
stim_frequency             -2.9908     0.7008  -4.268 1.97e-05 ***
stim_color                  0.3438     0.1761   1.953   0.0509 .  
stim_frequency:stim_color   1.8865     1.0771   1.751   0.0799 ."

# the story here:

# intercept: 
# in the absence of variables' effects, a stim is more likely to be 0 (not be on a category boundary)
table(ds$bounded) 
"
0     1 
2479  1671
"
# k there are just more 0 stim

# stim_frequency on it's own:
# when that training frequency of a stim is higher, it is more likely to be 0 (NOT have a category boundary)
table(ds$stim_frequency)  
"
0.0333  0.0666  0.1  0.1333  0.1666  0.3333
1660    830     415  415     415     415
"
# k there are way more low-freq stims 

table(ds$bounded,ds$stim_frequency)
"
0.0333  0.0666  0.1000  0.1333  0.1666  0.3333
0       999     424     254      274    229     299
1       661     406     161      141    186     116
"
# and here's what that interaction between stim frequency and bounded looks like
# 0 is more likely over all, but even more likely for the low frequency stim. Looks linear.

# stim_color on it's own:
# as stim gets brighter (moves up toward 250), it KINDA makes a category boundary more likely.
# these units are really small because the range of stim_color is much larger than the range of bounded.
# We can re-scale this variable for better units - it doesn't change the stats (see line 132). 

table(ds$bounded,ds$stim_color)
"     
25  50  75  100 125 150 175 200 225 250  <- stim color
0   329 257 261 254 217 207 222 232 213 287  <- stim not on category boundary
1   86  158 154 161 198 208 193 183 202 128  <- stim is on category boundary (looks non-linear with dip back down to 128 on brightest edge)
"

# stim_frequency:stim_color interaction:
# higher frequency and brighter color KINDA makes a category boundary more likely
# alright - this interaction is what I saw when I plotted the data for the cogci poster. But it might not be significant.

# remember,
# the highest-order interation tells the story
# and the effect of the variables on their own is what's left to explain after the interaction does it's work.

################
# a note on the non-convergence thang

# I normally do model selection starting with the full model and removing each variable and interaction one by one.
# But the problem here is the full model isn't converged.
# Can you do model selection using un-converged full models?
# Or is it reasonable practice to do the reverse? Start with a simple model and rule in extra complexities one by one?

# Obviously I want to know how this boundaries evolve over time, so I want to know the effect of iteration.
m <- glmer(bounded ~ stim_frequency * stim_color * iteration + (1|participant) + (1|lineage), data=ds, family="binomial")

# And I also want to know the effect of condition in case the boundaries evolve differently in the two transmission regimes.
m <- glmer(bounded ~ stim_frequency * stim_color * condition + (1|participant) + (1|lineage), data=ds, family="binomial")

# But as I said, those models aren't converging....

########################################################################################
# END
########################################################################################