df1 <- read.csv("/Users/vanferdi/Library/Mobile Documents/com~apple~CloudDocs/Research/PROJECTS/Category Change/GITHUB REPO/Data/experiment1_FINAL.csv")
df2 <- read.csv("/Users/vanferdi/Library/Mobile Documents/com~apple~CloudDocs/Research/PROJECTS/Category Change/GITHUB REPO/Data/experiment2_FINAL.csv")

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

##########################
# Discovery rate over time

# get the cumulatively unique systems per iteration - ignore the init systems coz they weren't "discovered"

# Experiment 1, condition I
df <- df1i
total_tries <- table(df$iteration)  # number of tries per iteration, 
" 1  2  3  4  5  6  7  8 
90 90 78 63 41 32 22 19"
u1 <- unique(subset(df,iteration==1)$system512)
u2 <- unique(subset(df,iteration==2)$system512)
u3 <- unique(subset(df,iteration==3)$system512)
u4 <- unique(subset(df,iteration==4)$system512)
u5 <- unique(subset(df,iteration==5)$system512)
u6 <- unique(subset(df,iteration==6)$system512)
u7 <- unique(subset(df,iteration==7)$system512)
u8 <- unique(subset(df,iteration==8)$system512)

dr1 <- length(u1)/total_tries[1]
dr2 <- length(setdiff(u2,u1))/total_tries[2] # setdiff(a,b) returns what is in a and not in b
dr3 <- length(setdiff(u3,c(u2,u1)))/total_tries[3]
dr4 <- length(setdiff(u4,c(u3,u2,u1)))/total_tries[4]
dr5 <- length(setdiff(u5,c(u4,u3,u2,u1)))/total_tries[5]
dr6 <- length(setdiff(u6,c(u5,u4,u3,u2,u1)))/total_tries[6]
dr7 <- length(setdiff(u7,c(u6,u5,u4,u3,u2,u1)))/total_tries[7]
dr8 <- length(setdiff(u8,c(u7,u6,u5,u4,u3,u2,u1)))/total_tries[8]

e1i_rates <- c(dr1,dr2,dr3,dr4,dr5,dr6,dr7,dr8)

# Experiment 1, condition C
df <- df1c
total_tries <- table(df$iteration)  # number of tries per iteration, 
" 1  2  3  4  5  6  7  8  9 10 11 
45 45 32 24 20 14 11  7  5  3  1 "
u1 <- unique(subset(df,iteration==1)$system512)
u2 <- unique(subset(df,iteration==2)$system512)
u3 <- unique(subset(df,iteration==3)$system512)
u4 <- unique(subset(df,iteration==4)$system512)
u5 <- unique(subset(df,iteration==5)$system512)
u6 <- unique(subset(df,iteration==6)$system512)
u7 <- unique(subset(df,iteration==7)$system512)
u8 <- unique(subset(df,iteration==8)$system512)
u9 <- unique(subset(df,iteration==9)$system512)
u10 <- unique(subset(df,iteration==10)$system512)
u11 <- unique(subset(df,iteration==11)$system512)

dr1 <- length(u1)/total_tries[1]
dr2 <- length(setdiff(u2,u1))/total_tries[2] 
dr3 <- length(setdiff(u3,c(u2,u1)))/total_tries[3]
dr4 <- length(setdiff(u4,c(u3,u2,u1)))/total_tries[4]
dr5 <- length(setdiff(u5,c(u4,u3,u2,u1)))/total_tries[5]
dr6 <- length(setdiff(u6,c(u5,u4,u3,u2,u1)))/total_tries[6]
dr7 <- length(setdiff(u7,c(u6,u5,u4,u3,u2,u1)))/total_tries[7]
dr8 <- length(setdiff(u8,c(u7,u6,u5,u4,u3,u2,u1)))/total_tries[8]
dr9 <- length(setdiff(u9,c(u8,u7,u6,u5,u4,u3,u2,u1)))/total_tries[9]
dr10 <- length(setdiff(u10,c(u9,u8,u7,u6,u5,u4,u3,u2,u1)))/total_tries[10]
dr11 <- length(setdiff(u11,c(u10,u9,u8,u7,u6,u5,u4,u3,u2,u1)))/total_tries[11]

e1c_rates <- c(dr1,dr2,dr3,dr4,dr5,dr6,dr7,dr8,dr9,dr10,dr11)


# Experiment 2, condition I
df <- df2i
total_tries <- table(df$iteration)  # number of tries per iteration, 
u1 <- unique(subset(df,iteration==1)$system512)
u2 <- unique(subset(df,iteration==2)$system512)
u3 <- unique(subset(df,iteration==3)$system512)
u4 <- unique(subset(df,iteration==4)$system512)
u5 <- unique(subset(df,iteration==5)$system512)
u6 <- unique(subset(df,iteration==6)$system512)
u7 <- unique(subset(df,iteration==7)$system512)
u8 <- unique(subset(df,iteration==8)$system512)

dr1 <- length(u1)/total_tries[1]
dr2 <- length(setdiff(u2,u1))/total_tries[2] # setdiff(a,b) returns what is in a and not in b
dr3 <- length(setdiff(u3,c(u2,u1)))/total_tries[3]
dr4 <- length(setdiff(u4,c(u3,u2,u1)))/total_tries[4]
dr5 <- length(setdiff(u5,c(u4,u3,u2,u1)))/total_tries[5]
dr6 <- length(setdiff(u6,c(u5,u4,u3,u2,u1)))/total_tries[6]
dr7 <- length(setdiff(u7,c(u6,u5,u4,u3,u2,u1)))/total_tries[7]
dr8 <- length(setdiff(u8,c(u7,u6,u5,u4,u3,u2,u1)))/total_tries[8]

e2i_rates <- c(dr1,dr2,dr3,dr4,dr5,dr6,dr7,dr8)


# Experiment 2, condition C
df <- df2c
total_tries <- table(df$iteration)  # number of tries per iteration, 
"  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 
45 44 38 30 23 16 11  5  3  2  2  1  1  1  1"
u1 <- unique(subset(df,iteration==1)$system512)
u2 <- unique(subset(df,iteration==2)$system512)
u3 <- unique(subset(df,iteration==3)$system512)
u4 <- unique(subset(df,iteration==4)$system512)
u5 <- unique(subset(df,iteration==5)$system512)
u6 <- unique(subset(df,iteration==6)$system512)
u7 <- unique(subset(df,iteration==7)$system512)
u8 <- unique(subset(df,iteration==8)$system512)
u9 <- unique(subset(df,iteration==9)$system512)
u10 <- unique(subset(df,iteration==10)$system512)
u11 <- unique(subset(df,iteration==11)$system512)
u12 <- unique(subset(df,iteration==12)$system512)
u13 <- unique(subset(df,iteration==13)$system512)
u14 <- unique(subset(df,iteration==14)$system512)
u15 <- unique(subset(df,iteration==15)$system512)

dr1 <- length(u1)/total_tries[1]
dr2 <- length(setdiff(u2,u1))/total_tries[2] 
dr3 <- length(setdiff(u3,c(u2,u1)))/total_tries[3]
dr4 <- length(setdiff(u4,c(u3,u2,u1)))/total_tries[4]
dr5 <- length(setdiff(u5,c(u4,u3,u2,u1)))/total_tries[5]
dr6 <- length(setdiff(u6,c(u5,u4,u3,u2,u1)))/total_tries[6]
dr7 <- length(setdiff(u7,c(u6,u5,u4,u3,u2,u1)))/total_tries[7]
dr8 <- length(setdiff(u8,c(u7,u6,u5,u4,u3,u2,u1)))/total_tries[8]
dr9 <- length(setdiff(u9,c(u8,u7,u6,u5,u4,u3,u2,u1)))/total_tries[9]
dr10 <- length(setdiff(u10,c(u9,u8,u7,u6,u5,u4,u3,u2,u1)))/total_tries[10]
dr11 <- length(setdiff(u11,c(u10,u9,u8,u7,u6,u5,u4,u3,u2,u1)))/total_tries[11]
dr12 <- length(setdiff(u12,c(u11,u10,u9,u8,u7,u6,u5,u4,u3,u2,u1)))/total_tries[12]
dr13 <- length(setdiff(u13,c(u12,u11,u10,u9,u8,u7,u6,u5,u4,u3,u2,u1)))/total_tries[13]
dr14 <- length(setdiff(u14,c(u13,u12,u11,u10,u9,u8,u7,u6,u5,u4,u3,u2,u1)))/total_tries[14]
dr15 <- length(setdiff(u15,c(u14,u13,u12,u11,u10,u9,u8,u7,u6,u5,u4,u3,u2,u1)))/total_tries[15]

e2c_rates <- c(dr1,dr2,dr3,dr4,dr5,dr6,dr7,dr8,dr9,dr10,dr11,dr12,dr13,dr14,dr15)

### PLOT ###
# solid: individual   dashed: cultural
# black: Exp1         blue:   Exp2
time <- seq(1,15)
plot(time,e2c_rates,las=1,ylim=c(0,1),type="l",lty="dotted",col="blue",main="discovery rate")
lines(e1i_rates,type="l")
lines(e1c_rates,type="l",lty="dotted")
lines(e2i_rates,type="l",col="blue")


##########################################
# Questions

# how on earth are these discovery rates similar?
# coz the I condition ones are searching more complex systems PLUS all the less complex ones












