require(lme4)

d1 <- readRDS("./experiment1.rds")
d2 <- readRDS("./experiment2.rds")

# restrict analyses to the first 8 iterations
d1 <- subset(d1,iteration < 9)
d2 <- subset(d2,iteration < 9)

#######################################################################
# the optimality score
#######################################################################

# optimality score is based on Shannon entropy, H()
H <- function(X,base) { # X is a discrete probability distribution
    X <- X[X != 0 ] # remove all zeros
    return(-sum(X*log(X,base)))
}

# the probability distribution over stimuli in each of the three frequency conditions
# stimuli colors are a greyscale gradient ranging from darkest, i=1, to lightest, i=10, on the vectors below
Lmass <- c(10,5,4,3,2,2,1,1,1,1)/30    # the "dark/purple skew" condition  Lmass[1] is the darkest stimulus
Rmass <- rev(Lmass)                    # the "light/blue skew" condition 
Umass <- rep(3,10)/30                  # the "uniform" condition

# define optimality score
# get vector of the mass under each category label, 0 and 1
# let the category system rack up points under any distribution of mass
scorer <- function(system,mass) {
    sys <- as.numeric(strsplit(as.character(system),"")[[1]]) # system in df is one integer, turn to array of digits
    mass_on_1 <- sum(sys*mass) # let each 1 rack up its corresponding mass
    mass_on_0 <- 1-mass_on_1   # requires that all values in variable "mass" sums to one
    return(H(c(mass_on_0,mass_on_1),2))
}

# example usage:
s <- d1$system512[1]  # example system is 1111110000
scorer(s,Lmass)       # H(0.1333333 0.8666667) = 0.5665094

# range of score
scorer("0000000000",Umass) # 0
scorer("1111100000",Umass) # 1
scorer("0000000000",Lmass) # 0
scorer("1100000000",Lmass) # 1 <- best possible optimality in L condition

# note: "0000000000" has lowest optimality possible under all encodings
# could be important in Experiment 2, where participants did produce "0000000000" systems

#######################################################################
# add optimality score columns to dataframe
#######################################################################

add_optimality_cols <- function(df) {
    HUmass <- c() # optimality score of each system under Umass - what the system's optimality would be if the distribution over stimuli were uniform
    Hmass <- c()  # actual optimality score - what the optimality of the system was given the actual distribution over stimuli
    HLmass <- c() # entropy of each system under Lmass
    HRmass <- c() # entropy of each system under Rmass
    
    for (i in 1:nrow(df)) {
        HLmass <- c(HLmass,scorer(df$system512[i],Lmass))
        HRmass <- c(HRmass,scorer(df$system512[i],Rmass))
        HUmass <- c(HUmass,scorer(df$system512[i],Umass))
        if (df$distribution[i]=="L") { Hmass <- c(Hmass,HLmass[i]) }
        if (df$distribution[i]=="R") { Hmass <- c(Hmass,HRmass[i]) }
        if (df$distribution[i]=="U") { Hmass <- c(Hmass,HUmass[i]) }
    }
    
    Hmass_diff <- Hmass-HUmass
    # positive values mean optimality under the actual distribution is better than the uniform one
    # negative values mean optimality is better under the uniform one
    
    df <- cbind(df,Hmass,HUmass,Hmass_diff)
    return(df)
}

d1 <- add_optimality_cols(d1)
d2 <- add_optimality_cols(d2)

#######################################################################
# full model
#######################################################################

# run full model on all data in iterations 1-8

# set reference variables
d1$distribution <- relevel(d1$distribution, ref="U") # make the uniform frequency condition the reference point
d1$condition <- relevel(d1$condition, ref="I") # make the individual transmission condition the reference point

full <- lmer(HUmass ~ distribution * condition * iteration + (1|lineage), data=d1, REML=FALSE)

reduce1 <- lmer(HUmass ~ distribution * condition + iteration + (1|lineage), data=d1, REML=FALSE)
reduce2 <- lmer(HUmass ~ distribution + condition * iteration + (1|lineage), data=d1, REML=FALSE)
reduce3 <- lmer(HUmass ~ distribution + condition + iteration + (1|lineage), data=d1, REML=FALSE)
anova(full,reduce1,reduce2,reduce3) # nothing is best, so no interactions are justified

r1 <- lmer(HUmass ~ distribution + condition + (1|lineage), data=d1, REML=FALSE)
anova(reduce3,r1) # keep iteration
r2 <- lmer(HUmass ~ distribution + iteration + (1|lineage), data=d1, REML=FALSE)
anova(reduce3,r2) # lose condition
r3 <- lmer(HUmass ~ condition + iteration + (1|lineage), data=d1, REML=FALSE)
anova(reduce3,r3) # lose distribution (was close: p = 0.08953)
best <- lmer(HUmass ~ iteration + (1|lineage), data=d1, REML=FALSE)

anova(full,best) # yep best is best coz full is not significantly better

summary(best) # result: iteration lowers optimality
"             Estimate Std. Error t value
(Intercept)  0.948664   0.009139  103.80
iteration   -0.012251   0.001888   -6.49"

# L = dark skew
# R = light skew


#######################################################################
# check if this result replicates in Experiment 2 - NOPE

d2$distribution <- relevel(d2$distribution, ref="U") # make the uniform frequency condition the reference point
d2$condition <- relevel(d2$condition, ref="I") # make the individual transmission condition the reference point

full <- lmer(HUmass ~ distribution * condition * iteration + (1|lineage), data=d2, REML=FALSE)
reduce1 <- lmer(HUmass ~ distribution * condition + iteration + (1|lineage), data=d2, REML=FALSE)
reduce2 <- lmer(HUmass ~ distribution + condition * iteration + (1|lineage), data=d2, REML=FALSE)
reduce3 <- lmer(HUmass ~ distribution + condition + iteration + (1|lineage), data=d2, REML=FALSE)
anova(full,reduce1,reduce2,reduce3) # full is significantly better

# k that means we'll need to keep all the variables, but check each one anyway:
r1 <- lmer(HUmass ~ distribution + condition + (1|lineage), data=d2, REML=FALSE)
anova(full,r1) # keep iteration
r2 <- lmer(HUmass ~ distribution + iteration + (1|lineage), data=d2, REML=FALSE)
anova(full,r2) # keep condition
r3 <- lmer(HUmass ~ condition + iteration + (1|lineage), data=d2, REML=FALSE)
anova(full,r3) # keep distribution

summary(full)
"                                    Estimate Std. Error t value
(Intercept)                         0.940045   0.038689  24.298
distributionL                       0.068645   0.055265   1.242
distributionR                      -0.069291   0.055167  -1.256
conditionC                          0.034914   0.069342   0.504
iteration                          -0.016529   0.006465  -2.557 <- optimality goes down over time
distributionL:conditionC           -0.132659   0.097279  -1.364
distributionR:conditionC            0.084858   0.098784   0.859
distributionL:iteration            -0.026837   0.009769  -2.747
distributionR:iteration             0.001912   0.009751   0.196
conditionC:iteration               -0.011991   0.013670  -0.877
distributionL:conditionC:iteration  0.051853   0.018330   2.829
distributionR:conditionC:iteration -0.031394   0.020383  -1.540
"

# L = purple skew
# R = blue skew

# I think this is because degenerate systems (i.e. "0000000000") occurred in Experiment 2
# and these have an optimality = 0, the lowest possible optimality score

# look at how these degenerate systems were distributed across the conditions
s <- subset(d2,system512=="1111111111") # 33 degenerate systems
unique(s$lineage) # across 17 lineages
table(s$condition,s$distribution)
"  U  L  R
I  8  4 11
C  4  2  4
"

s[order(s$iteration)] # sort dataframe by iteration

# look at each lineage
for (i in unique(s$lineage)) {
    print(subset(s,lineage==i)$system1024)
}

# what's up withthe 1-gen chains?

#######################################################################
# create Figure X in paper
#######################################################################

require(ggplot2)

p1 <- ggplot(d1, aes(x=distribution, y=HUmass)) +
    geom_violin() +
    stat_summary(fun.y=median, geom="point", size=2) +
    ylim(0,1) +
    xlab("frequency condition") +
    ylab("optimality \n under a uniform distribution") +
    scale_x_discrete(labels=c("uniform","dark skew","light skew"))

p2 <- ggplot(d1, aes(x=distribution, y=Hmass)) +
    geom_violin() +
    stat_summary(fun.y=median, geom="point", size=2) +
    ylim(0,1) +
    xlab("frequency condition") +
    ylab("optimality \n under the correct distribution") +
    scale_x_discrete(labels=c("uniform","dark skew","light skew"))

ggsave(filename = "optimality_uniform.png", p1, width = 4, height = 3, dpi = 300, units = "in", device='png')
ggsave(filename = "optimality_actual.png", p2, width = 4, height = 3, dpi = 300, units = "in", device='png')

#######################################################################
# check plot for Experiment 2

# colors are different in Experiment 2, its a blue-purple gradient
# i=1 is the purplest and i=10 is the bluest

p1 <- ggplot(d2, aes(x=distribution, y=HUmass)) +
    geom_violin() +
    stat_summary(fun.y=median, geom="point", size=2) +
    ylim(0,1) +
    xlab("frequency condition") +
    ylab("optimality \n under a uniform distribution") +
    scale_x_discrete(labels=c("uniform","purple skew","blue skew"))

p2 <- ggplot(d2, aes(x=distribution, y=Hmass)) +
    geom_violin() +
    stat_summary(fun.y=median, geom="point", size=2) +
    ylim(0,1) +
    xlab("frequency condition") +
    ylab("optimality \n under the correct distribution") +
    scale_x_discrete(labels=c("uniform","purple skew","blue skew"))

ggsave(filename = "optimality_uniform_Exp2.png", p1, width = 4, height = 3, dpi = 300, units = "in", device='png')
ggsave(filename = "optimality_actual_Exp2.png", p2, width = 4, height = 3, dpi = 300, units = "in", device='png')


#######################################################################
# the optimality score for continuous systems only
#######################################################################

d11 <- subset(d1,N_boundaries==1) # 266 systems with 1 boundary

d11$distribution <- relevel(d11$distribution, ref="U") # make the uniform frequency condition the reference point
d11$condition <- relevel(d11$condition, ref="C")

full <- lmer(HUmass ~ distribution * condition * iteration + (1|trajectory), data=d11, REML=FALSE)
reduce1 <- lmer(HUmass ~ distribution * condition + iteration + (1|trajectory), data=d11, REML=FALSE)
reduce2 <- lmer(HUmass ~ distribution + condition * iteration + (1|trajectory), data=d11, REML=FALSE)
reduce3 <- lmer(HUmass ~ distribution + condition + iteration + (1|trajectory), data=d11, REML=FALSE)
anova(full,reduce1) # full wins
anova(full,reduce2) # full wins
anova(full,reduce3) # full wins

r1 <- lmer(HUmass ~ distribution * condition + (1|trajectory), data=d11, REML=FALSE)
anova(full,r1) # full wins
r2 <- lmer(HUmass ~ distribution * iteration + (1|trajectory), data=d11, REML=FALSE)
anova(full,r2) # full wins 
r3 <- lmer(HUmass ~ condition * iteration + (1|trajectory), data=d11, REML=FALSE)
anova(full,r3) # full wins 

summary(full)
"                                   Estimate Std. Error t value
(Intercept)                         1.08698    0.05210  20.865
distributionL                      -0.12561    0.06902  -1.820
distributionR                      -0.19146    0.07315  -2.618
conditionI                         -0.18812    0.06228  -3.021
iteration                          -0.04751    0.01043  -4.556
distributionL:conditionI            0.23221    0.08871   2.618
distributionR:conditionI            0.22208    0.08950   2.481
distributionL:iteration             0.03259    0.01382   2.358
distributionR:iteration             0.04681    0.01523   3.074
conditionI:iteration                0.04588    0.01188   3.861
distributionL:conditionI:iteration -0.05685    0.01710  -3.324
distributionR:conditionI:iteration -0.06933    0.01813  -3.824"


# boundary locations
require(stringr)
bound_location <- c()
for (i in 1:nrow(d11)) {
    loc <- str_count(d11$system512[i],"1")
    bound_location <- c(bound_location,loc)
}

d11 <- cbind(d11,bound_location)


# distribution L
s <- subset(d11,distribution=="L")
mean(s$Hmass) # 0.6360488
table(s$bound_location)/sum(table(s$bound_location))
" 1  2  3  4  5  6  7  8  9 
  0  4  4 16 16 12 20  8  2 "

# distribution U
s <- subset(d11,distribution=="U")
mean(s$Hmass) # 0.9033566
table(s$bound_location)/sum(table(s$bound_location))
" 1  2  3  4  5  6  7  8  9 
  2  3 10 11 29 18 12 11  2"

# distribution R
s <- subset(d11,distribution=="R")
mean(s$Hmass) # 0.7209378
table(s$bound_location)/sum(table(s$bound_location))
" 1  2  3  4  5  6  7  8  9 
  3  6 10  8 17 20 11  6  5 "

as.vector(table(s$bound_location)/sum(table(s$bound_location)))











