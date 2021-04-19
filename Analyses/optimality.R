require(lme4)
require(ggplot2)

d1 <- readRDS("../Data/experiment1.rds")
d2 <- readRDS("../Data/experiment2.rds")

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

# more degenerate systems in I and R

s[order(s$iteration)] # sort dataframe by iteration

# look at each lineage
for (i in unique(s$lineage)) {
    print(subset(s,lineage==i)$system1024)
}

# what's up with the 1-gen chains?

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

# Experiment 2

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

full <- lmer(HUmass ~ distribution * condition * iteration + (1|lineage), data=d11, REML=FALSE)
reduce1 <- lmer(HUmass ~ distribution * condition + iteration + (1|lineage), data=d11, REML=FALSE)
reduce2 <- lmer(HUmass ~ distribution + condition * iteration + (1|lineage), data=d11, REML=FALSE)
reduce3 <- lmer(HUmass ~ distribution + condition + iteration + (1|lineage), data=d11, REML=FALSE)
anova(full,reduce1) # full wins
anova(full,reduce2) # full wins
anova(full,reduce3) # full wins

r1 <- lmer(HUmass ~ distribution * condition + (1|lineage), data=d11, REML=FALSE)
anova(full,r1) # full wins
r2 <- lmer(HUmass ~ distribution * iteration + (1|lineage), data=d11, REML=FALSE)
anova(full,r2) # full wins 
r3 <- lmer(HUmass ~ condition * iteration + (1|lineage), data=d11, REML=FALSE)
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

# freakin everythang matters

# Experiment 2 replication
d21 <- subset(d2,N_boundaries==1) # 312 systems with 1 boundary

d21$distribution <- relevel(d21$distribution, ref="U") # make the uniform frequency condition the reference point
d21$condition <- relevel(d21$condition, ref="C")

full <- lmer(HUmass ~ distribution * condition * iteration + (1|lineage), data=d21, REML=FALSE)
reduce1 <- lmer(HUmass ~ distribution * condition + iteration + (1|lineage), data=d21, REML=FALSE)
reduce2 <- lmer(HUmass ~ distribution + condition * iteration + (1|lineage), data=d21, REML=FALSE)
reduce3 <- lmer(HUmass ~ distribution + condition + iteration + (1|lineage), data=d21, REML=FALSE)
anova(full,reduce1) # full wins
anova(full,reduce2) # full wins
anova(full,reduce3) # full wins

summary(full)
"Estimate Std. Error t value
(Intercept)                         0.9614095  0.0392289  24.508
distributionL                      -0.0697083  0.0513997  -1.356
distributionR                      -0.0422546  0.0624259  -0.677
conditionI                         -0.0381885  0.0521384  -0.732
iteration                          -0.0053979  0.0080969  -0.667
distributionL:conditionI            0.1311589  0.0673098   1.949
distributionR:conditionI            0.0441562  0.0771043   0.573
distributionL:iteration             0.0148055  0.0100920   1.467
distributionR:iteration            -0.0007485  0.0142884  -0.052
conditionI:iteration                0.0032928  0.0105839   0.311
distributionL:conditionI:iteration -0.0336615  0.0134620  -2.500
distributionR:conditionI:iteration  0.0021496  0.0169592   0.127"


# now do under the correct distribution
full <- lmer(Hmass ~ distribution * condition * iteration + (1|lineage), data=d11, REML=FALSE)
reduce1 <- lmer(Hmass ~ distribution * condition + iteration + (1|lineage), data=d11, REML=FALSE)
reduce2 <- lmer(Hmass ~ distribution + condition * iteration + (1|lineage), data=d11, REML=FALSE)
reduce3 <- lmer(Hmass ~ distribution + condition + iteration + (1|lineage), data=d11, REML=FALSE)
anova(full,reduce1) # full wins
anova(full,reduce2) # full wins
anova(full,reduce3) # full wins

summary(full)
"                                   Estimate Std. Error t value
(Intercept)                         1.09312    0.06609  16.539
distributionL                      -0.27560    0.08775  -3.141
distributionR                      -0.30967    0.09264  -3.343
conditionI                         -0.19585    0.07944  -2.466
iteration                          -0.04918    0.01253  -3.926
distributionL:conditionI            0.22271    0.11314   1.968
distributionR:conditionI            0.15514    0.11393   1.362
distributionL:iteration             0.01736    0.01668   1.041
distributionR:iteration             0.02777    0.01835   1.514
conditionI:iteration                0.04799    0.01420   3.379
distributionL:conditionI:iteration -0.07250    0.02053  -3.531
distributionR:conditionI:iteration -0.02193    0.02172  -1.010"

full <- lmer(Hmass ~ distribution * condition * iteration + (1|lineage), data=d21, REML=FALSE)
reduce1 <- lmer(Hmass ~ distribution * condition + iteration + (1|lineage), data=d21, REML=FALSE)
reduce2 <- lmer(Hmass ~ distribution + condition * iteration + (1|lineage), data=d21, REML=FALSE)
reduce3 <- lmer(Hmass ~ distribution + condition + iteration + (1|lineage), data=d21, REML=FALSE)
anova(full,reduce1) # full wins
anova(full,reduce2) # lose interaction at p = 0.06392
anova(full,reduce3) # full wins

reduce4 <- lmer(Hmass ~ condition + iteration + (1|lineage), data=d21, REML=FALSE)
reduce5 <- lmer(Hmass ~ distribution + iteration + (1|lineage), data=d21, REML=FALSE)
reduce6 <- lmer(Hmass ~ distribution + condition + (1|lineage), data=d21, REML=FALSE)
anova(full,reduce4) # keep distribution
anova(full,reduce5) # lose condition at p = 0.0559 .
anova(full,reduce6) # keep iteration

inter5 <- lmer(Hmass ~ distribution * iteration + (1|lineage), data=d21, REML=FALSE)
anova(inter5,reduce5)

best <-reduce5
summary(best)
"               Estimate Std. Error t value
(Intercept)    0.987646   0.031793  31.064
distributionL -0.233159   0.038184  -6.106
distributionR -0.263500   0.039915  -6.602
iteration     -0.016567   0.003533  -4.690"

summary(full)
"                                    Estimate Std. Error t value
(Intercept)                         0.963616   0.054127  17.803
distributionL                      -0.207774   0.071784  -2.894
distributionR                      -0.269336   0.088254  -3.052
conditionI                         -0.040578   0.072901  -0.557
iteration                          -0.006016   0.009976  -0.603
distributionL:conditionI            0.080703   0.094614   0.853
distributionR:conditionI            0.111271   0.109114   1.020
distributionL:iteration            -0.002224   0.012423  -0.179
distributionR:iteration            -0.012667   0.019010  -0.666
conditionI:iteration                0.004379   0.013020   0.336
distributionL:conditionI:iteration -0.031486   0.016584  -1.899
distributionR:conditionI:iteration -0.008176   0.022115  -0.370"


#######################################################################
# create Figure X in paper

p1 <- ggplot(d11, aes(x=distribution, y=HUmass)) +
    geom_violin() +
    stat_summary(fun.y=median, geom="point", size=2) +
    ylim(0,1) +
    xlab("frequency condition") +
    ylab("optimality \n under a uniform distribution") +
    scale_x_discrete(labels=c("uniform","dark skew","light skew"))

p2 <- ggplot(d11, aes(x=distribution, y=Hmass)) +
    geom_violin() +
    stat_summary(fun.y=median, geom="point", size=2) +
    ylim(0,1) +
    xlab("frequency condition") +
    ylab("optimality \n under the correct distribution") +
    scale_x_discrete(labels=c("uniform","dark skew","light skew"))

ggsave(filename = "optimality_uniform_singles.png", p1, width = 4, height = 3, dpi = 300, units = "in", device='png')
ggsave(filename = "optimality_actual_singles.png", p2, width = 4, height = 3, dpi = 300, units = "in", device='png')

# Experiment 2

# colors are different in Experiment 2, its a blue-purple gradient
# i=1 is the purplest and i=10 is the bluest

p1 <- ggplot(d21, aes(x=distribution, y=HUmass)) +
    geom_violin() +
    stat_summary(fun.y=median, geom="point", size=2) +
    ylim(0,1) +
    xlab("frequency condition") +
    ylab("optimality \n under a uniform distribution") +
    scale_x_discrete(labels=c("uniform","purple skew","blue skew"))

p2 <- ggplot(d21, aes(x=distribution, y=Hmass)) +
    geom_violin() +
    stat_summary(fun.y=median, geom="point", size=2) +
    ylim(0,1) +
    xlab("frequency condition") +
    ylab("optimality \n under the correct distribution") +
    scale_x_discrete(labels=c("uniform","purple skew","blue skew"))

ggsave(filename = "optimality_uniform_singles_Exp2.png", p1, width = 4, height = 3, dpi = 300, units = "in", device='png')
ggsave(filename = "optimality_actua_singlesl_Exp2.png", p2, width = 4, height = 3, dpi = 300, units = "in", device='png')
######################################################################################












# Check if R and L optimality is going up?

# is optimality for the respective distribution going up over time?
# U should be at ceiling, but the other two could be going up if they're optimizing
u1 <- subset(d11,distribution=="U")
plot(jitter(u1$iteration),jitter(u1$Hmass),las=1)
abline(lm(u1$Hmass~u1$iteration)) # goes down over time

r1 <- subset(d11,distribution=="R")
plot(jitter(r1$iteration),jitter(r1$Hmass),las=1)
abline(lm(r1$Hmass~r1$iteration)) # basically flat

l1 <- subset(d11,distribution=="L")
plot(jitter(l1$iteration),jitter(l1$Hmass),las=1)
abline(lm(l1$Hmass~l1$iteration)) # goes way down

# re-plot with lmer intercept and slope
# U
d11$distribution <- relevel(d11$distribution, ref="U")
summary(lmer(HUmass ~ distribution * condition * iteration + (1|lineage), data=d11, REML=FALSE))
plot(jitter(u1$iteration),jitter(u1$Hmass),las=1)
abline(1.08698,-0.04751)

# R
d11$distribution <- relevel(d11$distribution, ref="R")
summary(lmer(HUmass ~ distribution * condition * iteration + (1|lineage), data=d11, REML=FALSE))
plot(jitter(r1$iteration),jitter(r1$Hmass),las=1)
abline(0.8955231,-0.0006964)

# L
d11$distribution <- relevel(d11$distribution, ref="L")
summary(lmer(HUmass ~ distribution * condition * iteration + (1|lineage), data=d11, REML=FALSE))
plot(jitter(l1$iteration),jitter(l1$Hmass),las=1)
abline(0.961370,-0.014921)

# Experiment 2
u2 <- subset(d21,distribution=="U")
plot(jitter(u2$iteration),jitter(u2$Hmass),las=1)
abline(lm(u2$Hmass~u2$iteration)) 

r2 <- subset(d21,distribution=="R")
plot(jitter(r2$iteration),jitter(r2$Hmass),las=1)
abline(lm(r2$Hmass~r2$iteration)) 

l2 <- subset(d21,distribution=="L")
plot(jitter(l2$iteration),jitter(l2$Hmass),las=1)
abline(lm(l2$Hmass~l2$iteration)) 

#######################################################################
# plot boundary locations for continuous systems
#######################################################################

# compute boundary locations
require(stringr)

d11 <- subset(d1,N_boundaries==1) 
d21 <- subset(d2,N_boundaries==1) 

compute_bound_locations <- function(df) {
    bound_location <- c()
    for (i in 1:nrow(df)) {
        loc <- str_count(df$system512[i],"1") # in system512, # of 1's = boundary location
        bound_location <- c(bound_location,loc)
    }
    df <- cbind(df,bound_location)
    return(df)
}

d11 <- compute_bound_locations(d11)
d21 <- compute_bound_locations(d21)

# make dataframe
mytable <- function(array,range_start,range_stop) {
    return(table(factor(array, levels = range_start:range_stop)))
}

bars <- function(df,U_name,L_name,R_name) {
    s <- subset(df,distribution=="U" & condition=="C")
    UC1 <-data.frame(mytable(s$bound_location,1,9))
    s <- subset(df,distribution=="U" & condition=="I")
    UI1 <-data.frame(mytable(s$bound_location,1,9))
    s <- subset(df,distribution=="L" & condition=="C")
    LC1 <-data.frame(mytable(s$bound_location,1,9))
    s <- subset(df,distribution=="L" & condition=="I")
    LI1 <-data.frame(mytable(s$bound_location,1,9))
    s <- subset(df,distribution=="R" & condition=="C")
    RC1 <-data.frame(mytable(s$bound_location,1,9))
    s <- subset(df,distribution=="R" & condition=="I")
    RI1 <-data.frame(mytable(s$bound_location,1,9))
    
    Dist <- c(rep(U_name,nrow(UC1)*2),rep(L_name,nrow(UC1)*2),rep(R_name,nrow(UC1)*2))
    Cond <- rep(c(rep("cultural",nrow(UC1)),rep("individual",nrow(UC1))),3)
    
    d <- data.frame(Cond,Dist,rbind(UC1,UI1,LC1,LI1,RC1,RI1))
    return(d)
}

d11bar <- bars(d11,"uniform","dark skew","light skew")
d21bar <- bars(d21,"uniform","purple skew","blue skew")


# plot all continous systems
p1 <- ggplot(data=d11bar, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity") +
    labs(x="\n\n\nlocation of category boundary", y="number of systems") +
    ylim(0,80) +
    ggtitle("Experiment 1") + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.margin=unit(c(0,0.5,0,0),"cm")) # top,right,bottom,left
p1

p2 <- ggplot(data=d21bar, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity") +
    labs(x="\n\n\nlocation of category boundary", y="") +
    ggtitle("Experiment 2") + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.margin=unit(c(0,0.5,0,0),"cm")) 
p2

ggsave(filename = "barsall_Exp1.png", p1, width = 4, height = 4, dpi = 300, units = "in", device='png')
ggsave(filename = "barsall_Exp2.png", p2, width = 4, height = 4, dpi = 300, units = "in", device='png')


# 6-panel plot broken down by distribution and condition
p3 <- ggplot(data=d11bar, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity") +
    facet_grid(Cond ~ Dist) +
    labs(x="location of category boundary", y="number of systems") +
    ggtitle("Experiment 1") + 
    theme(plot.title = element_text(hjust = 0.5)) # center the title
p3

# change order of columns in the grid
d21bar$Dist = factor(d21bar$Dist, levels=c("purple skew","blue skew","uniform"))

p4 <- ggplot(data=d21bar, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity") +
    facet_grid(Cond ~ Dist) +
    labs(x="location of category boundary", y="") +
    ggtitle("Experiment 2") + 
    theme(plot.title = element_text(hjust = 0.5))
p4

ggsave(filename = "bars_Exp1.png", p3, width = 4, height = 4, dpi = 300, units = "in", device='png')
ggsave(filename = "bars_Exp2.png", p4, width = 4, height = 4, dpi = 300, units = "in", device='png')

######################################################################################
# calulate skew and kurtosis
require("moments")
skewness(d11bar$Freq) # 0.9236969  moderate skew
kurtosis(d11bar$Freq) # 3.658913

skewness(d21bar$Freq) # 1.092105
kurtosis(d21bar$Freq) # 3.258279

# skewness between -0.5 and 0.5, data is fairly symmetrical
# up to -1 or 1 is moderate and larger than -1 or 1 is strong skewness

# kurtosis of a normal distribution = 3
# kurtosis > 3 means a distribution has heavier tails than the normal distribution

uc1 <- subset(d11bar,Dist=="uniform" & Cond=="cultural")
ui1 <- subset(d11bar,Dist=="uniform" & Cond=="individual")
lc1 <- subset(d11bar,Dist=="dark skew" & Cond=="cultural")
li1 <- subset(d11bar,Dist=="dark skew" & Cond=="individual")
rc1 <- subset(d11bar,Dist=="light skew" & Cond=="cultural")
ri1 <- subset(d11bar,Dist=="light skew" & Cond=="individual")

uc2 <- subset(d21bar,Dist=="uniform" & Cond=="cultural")
ui2 <- subset(d21bar,Dist=="uniform" & Cond=="individual")
lc2 <- subset(d21bar,Dist=="purple skew" & Cond=="cultural")
li2 <- subset(d21bar,Dist=="purple skew" & Cond=="individual")
rc2 <- subset(d21bar,Dist=="blue skew" & Cond=="cultural")
ri2 <- subset(d21bar,Dist=="blue skew" & Cond=="individual")

skewness(uc1$Freq)  # 1.279401   light skew
skewness(ui1$Freq)  # 0.4364344  no skew
skewness(lc1$Freq)  # -0.0082    no skew
skewness(li1$Freq)  # 0.3173207  no skew
skewness(rc1$Freq)  # 0.5506595  light skew
skewness(ri1$Freq)  # 0.5191305  light skew

var(uc1$Freq) # culture has lower variance than individual
var(ui1$Freq)

skewness(uc2$Freq)  # 1.578724  blue skew
skewness(ui2$Freq)  # 0.884026  blue skew
skewness(lc2$Freq)  # 0.348843  no skew
skewness(li2$Freq)  # 0.706134  blue skew
skewness(rc2$Freq)  # 1.098968  blue skew
skewness(ri2$Freq)  # 0.597544  blue skew

var(uc2$Freq) 
var(ui2$Freq)

######################################################################################
# look at mean boundary location in each combo of conditions

uc1 <- subset(d11,distribution=="U" & condition=="C")
ui1 <- subset(d11,distribution=="U" & condition=="I")
lc1 <- subset(d11,distribution=="L" & condition=="C")
li1 <- subset(d11,distribution=="L" & condition=="I")
rc1 <- subset(d11,distribution=="R" & condition=="C")
ri1 <- subset(d11,distribution=="R" & condition=="I")

uc2 <- subset(d21,distribution=="U" & condition=="C")
ui2 <- subset(d21,distribution=="U" & condition=="I")
lc2 <- subset(d21,distribution=="L" & condition=="C")
li2 <- subset(d21,distribution=="L" & condition=="I")
rc2 <- subset(d21,distribution=="R" & condition=="C")
ri2 <- subset(d21,distribution=="R" & condition=="I")

mean(rc1$bound_location)  # 5         # light
mean(uc1$bound_location)  # 5.366667
mean(lc1$bound_location)  # 5.324324  # dark

mean(ri1$bound_location)  # 5.458333  # light
mean(ui1$bound_location)  # 5.352941
mean(li1$bound_location)  # 5.8       # dark

mean(lc2$bound_location)  # 4.45283   # purple
mean(uc2$bound_location)  # 4.25641
mean(rc2$bound_location)  # 4.37931   # blue

mean(li2$bound_location)  # 5.405063  # purple
mean(ui2$bound_location)  # 5.369565
mean(ri2$bound_location)  # 4.878788  # blue

# these means aren't in the right direction anyway
# if there's any signal in here, all we can see is noise with this sample size.





















#######################################################################
# color analysis - creates a different data frame
#######################################################################

# create a new dataframe (called c1) from d1 - each row is a stimulus


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

create_colors_df <- function(df) {
    # define what the columns are gonna be
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
    # greys above won't matter once we rescale color, so no need to separately define "blues" for Exp 2
    
    stim_frequency <- c()   # frequency of the stim in the current test trial during training (normalized 0 to 1)
    L_counts <- c(10,5,4,3,2,2,1,1,1,1)  # frequency per stim in the left skew condition
    L_counts <- L_counts/sum(L_counts)
    R_counts <- rev(L_counts)            # frequency per stim in the right skew condition
    
    # is the current stimulus on the edge of a category boundary? According to the category system people produced on that testing round.
    # uses the function defined below system_to_bounded()
    bounded <- c()          # 0 = no. Both neighbors to the stim have same category label as the stim.
    # 1 = yes. At least one of the neighbors to the stim had a difference category label.
    
    
    for (r in 1:length(df$participant)) { # for each row in the original data frame (= the result of one round)
        
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
            lineage <- c(lineage,as.character(df[r,]$lineage)) 
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
    
    cf <- data.frame(lineage,iteration,participant,trial,stim_color,stim_color_rescale,stim_frequency,RT,bounded,skew,condition)
    
    # make sure everything you want to be a factor is a factor
    #d$participant <- as.factor(d$participant)
    
    d$bounded <- as.factor(d$bounded)
    
    return(cf)
}

c1 <- create_colors_df(d1)  # nrow(c1) 6330 rows (i.e. stimuli)
c2 <- create_colors_df(d2)  # nrow(c2) 6810 rows


#######################################################################
# color stats
#######################################################################

# at the most basic level, I just want to know if/how stim_frequency and stim_color affect bounded

m0 <- glmer(bounded ~ condition * stim_frequency * stim_color_rescale + (1|participant) + (1|lineage), data=c1, family="binomial")
summary(m0)

m1 <- glmer(bounded ~ stim_frequency * stim_color_rescale + (1|participant) + (1|lineage), data=c1, family="binomial")
summary(m1)
"                                  Estimate Std. Error z value Pr(>|z|)
(Intercept)                        -0.4826     0.1094  -4.412 1.02e-05
stim_frequency                     -3.0907     0.6958  -4.442 8.92e-06
stim_color_rescale                  0.2473     0.1538   1.608   0.1079
stim_frequency:stim_color_rescale   2.1041     1.0667   1.972   0.0486
"

# higher frequency and brighter color make a category boundary more likely
# also a brighter color on its own makes a boundary more likely
# but frequency in its own repels boundaries!

m2 <- glmer(bounded ~ stim_frequency + stim_color_rescale + (1|participant) + (1|lineage), data=c1, family="binomial")
anova(m1,m2) # k full model is sig best
"m1  6 8045.4 8086.0 -4016.7   8033.4 3.9124      1    0.04793 *"

m1 <- glmer(bounded ~ stim_frequency * stim_color_rescale + (1|participant) + (1|lineage), data=c2, family="binomial")
summary(m1)
"                                  Estimate Std. Error z value Pr(>|z|)
(Intercept)                       -0.08874    0.10471  -0.847   0.3967
stim_frequency                    -6.58866    0.80713  -8.163 3.27e-16
stim_color_rescale                -0.26688    0.15751  -1.694   0.0902
stim_frequency:stim_color_rescale  1.69694    1.24433   1.364   0.1727"

m2 <- glmer(bounded ~ stim_frequency + stim_color_rescale + (1|participant) + (1|lineage), data=c2, family="binomial")
anova(m1,m2) # lose the interaction
m3 <- glmer(bounded ~ stim_color_rescale + (1|participant) + (1|lineage), data=c2, family="binomial")
anova(m3,m2) # keep frequency
m4 <- glmer(bounded ~ stim_frequency + (1|participant) + (1|lineage), data=c2, family="binomial")
anova(m4,m2) # lose color
m5 <- glmer(bounded ~ 1 + (1|participant) + (1|lineage), data=c2, family="binomial")
anova(m5,m4) # keep frequency

best <- m4
"               Estimate Std. Error z value Pr(>|z|)    
(Intercept)    -0.22582    0.06583   -3.43 0.000603 ***
stim_frequency -5.71485    0.46770  -12.22  < 2e-16 ***"

# proportion of bounded to not, by frequency
tc1 <- table(c1$stim_frequency,c1$bounded)
cbind(tc1,tc1[,2]/(tc1[,1]+tc1[,2]))

tc2 <- table(c2$stim_frequency,c2$bounded)
cbind(tc2,table(c2$stim_frequency,c2$bounded)[,2]/table(c2$stim_frequency,c2$bounded)[,1])

# is the lower freq = more likely to have a boundary due just because there are more low freq items?
# I thought lmers took care of different numbers of observations, for sure.


summary(glmer(bounded ~ condition * stim_frequency * stim_color_rescale + (1|participant) + (1|lineage), data=c1, family="binomial"))
summary(glmer(bounded ~ condition * stim_frequency * stim_color_rescale + (1|participant) + (1|lineage), data=c2, family="binomial"))




