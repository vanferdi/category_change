# this file contains basic info about the data and descriptives

df1 <- readRDS("../Data/experiment1.rds")
df2 <- readRDS("../Data/experiment2.rds")

# number of participants
length(unique(df1$participant))     # 297 in experiment 1
length(unique(df2$participant))     # 313 in experiment 2

# participants per condition
df1c <- subset(df1,condition=="C")
df1i <- subset(df1,condition=="I")
length(unique(df1c$participant))    # 207 in cultural
length(unique(df1i$participant))    # 90 in individual

df2c <- subset(df2,condition=="C")
df2i <- subset(df2,condition=="I")
length(unique(df2c$participant))    # 223 in cultural
length(unique(df2i$participant))    # 90 in individual

# number of category systems
length(df1$system512)               # 642 in experiment 1
length(df2$system512)               # 692 in experiment 2

# number of trajectories
length(unique(df1c$trajectory))     # 45 in Exp 1 C
length(unique(df1i$trajectory))     # 90 in Exp 1 I
length(unique(df2c$trajectory))     # 45 in Exp 2 C
length(unique(df2i$trajectory))     # 90 in Exp 2 I
sum(45,45,90,90)                    # 270 total

#########################################################################
# duration of the experiment in minutes

# average
mean(df1c$duration_experiment)/60000 # 6.820537   6 min 49 sec
mean(df1i$duration_experiment)/60000 # 17.2528   17 min 15 sec
mean(df2c$duration_experiment)/60000 # 6.194271   6 min 12 sec
mean(df2i$duration_experiment)/60000 # 17.68063  17 min 41 sec

0.820537*60
0.2528*60
0.194271*60
0.68063*60

# max
max(df1c$duration_experiment)/60000 # 40.58247
max(df1i$duration_experiment)/60000 # 51.39727
max(df2c$duration_experiment)/60000 # 25.12885
max(df2i$duration_experiment)/60000 # 72.14815 (outlier)

# min
min(df1c$duration_experiment)/60000 # 4.01455
min(df1i$duration_experiment)/60000 # 3.922617
min(df2c$duration_experiment)/60000 # 3.911183
min(df2i$duration_experiment)/60000 # 4.045917

table(df1c$duration_experiment/60000)
table(df1i$duration_experiment/60000)
table(df2c$duration_experiment/60000)
table(df2i$duration_experiment/60000)

# btw I couldn't use df2$duration_experiment_minutes coz there was an error in line 692
# here's the fix (floored minutes from 372715/60000)
df2[692,]$duration_experiment_minutes <- 6

# 8 rounds of the I condition experiment was designed to take less than 1 hours
# so participants who took over an hour must have taken a break.

#########################################################################