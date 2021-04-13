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
d21 <- subset(d2,N_boundaries==1) # 266 systems with 1 boundary

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



