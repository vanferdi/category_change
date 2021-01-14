df1 <- read.csv("/Users/vanferdi/Library/Mobile Documents/com~apple~CloudDocs/Research/PROJECTS/Category Change/GITHUB REPO/Data/experiment1_FINAL.csv")
df2 <- read.csv("/Users/vanferdi/Library/Mobile Documents/com~apple~CloudDocs/Research/PROJECTS/Category Change/GITHUB REPO/Data/experiment2_FINAL.csv")

df1c <- subset(df1,condition=="C")
df1i <- subset(df1,condition=="I")
df2c <- subset(df2,condition=="C")
df2i <- subset(df2,condition=="I")



a <- "1111100000"
b <- "1011100000" # 1 change
c <- "0111100001" # 2 changes
d <- "1101011101" # 6 changes

# Levenshtein distance function is adist()
# will just give the number of labels that flipped
adist(a,b)
adist(a,c)
adist(a,d)

# try it on two strings in the dataframe
a <- df1$system512[1]
b <- df1$system512[6]
adist(a,b)


get_edits_512 <- function(d) {
	edits <- c()
	for (i in 1:nrow(d)) {
		edit <- adist(d$system512_input[i],d$system512[i])
		#print(d$system512_input[i])
		#print(d$system512[i])
		#print(edit)
		edits <- c(edits,edit)
	}
	return(edits)
}

# append new column to each dataframe - contains the number of label flips (edits) between input and output 512 system
edits <- get_edits_512(df1)
df1 <- cbind(df1,edits)
edits <- get_edits_512(df2)
df2 <- cbind(df2,edits)

#####################################
# Does it have to use the 1024 system or are the answers the same anyway using the 512 system?
get_edits_1024 <- function(d) {
	edits <- c()
	for (i in 1:nrow(d)) {
		edit <- adist(d$system1024_input[i],d$system1024[i])
		edits <- c(edits,edit)
	}
	return(edits)
}
e1 <- get_edits_512(df1)
e2 <- get_edits_1024(df1)
# ah crap that style of answer doesn't work because the 1024 systems initial zeros are removed!
cbind(e1,e2,df1$system1024)


#####################################
# Which systems showed zero edits?   124 systems
subset(df1,edits==0)$system512

# What were their number of boundaries?   1-5 boundaries, all >5 were unstable
table(subset(df1,edits==0)$N_boundaries)

# show many of each zero-edit event occurred per zero-edit system
table(subset(df1,edits==0)$system512,subset(df1,edits==0)$edits)

# the most stable system is 1111100000 
# had 19 perfect reproductions (19 is the max for zero-edit systems)

# For all edit distances, show edit distance by number of boundaries (left = boundaries, right = number of edits)
table(df1$N_boundaries,df1$edits)

# TO DO - plot the table above nicely


##########################################################################
# what are the TPs among N_boundaries?

table(df1$N_boundaries_input,df1$N_boundaries)
" in is rows, out is on top"
"   1   2   3   4   5   6   7   8
1 177   0  11   2   1   0   0   0
2  12  78   8   8   0   0   0   0
3  46   1  42   4   7   2   1   1
4   3  37   7  28   2   7   1   1
5  20   4  18   4  25   0   3   0
6   8  10   7  16   3   5   0   0
7   8   2   6   3   2   2   1   0
8   0   1   1   4   0   0   2   0"

systype_input <- if(df1$N_boundaries_input==1){"continuous"}

ifelse(df1$N_boundaries_input==1, "continous", "disjoint")

systype_input <- ifelse(df2$N_boundaries_input==0, "degenerate", ifelse(df2$N_boundaries_input==1, "continous", "disjoint"))
cbind(df2$N_boundaries_input,x)

##########################################################################
# what are the TPs among continuous and disjoint systems?

# add to df a column that categorizes input and output systems into 3 types
systype_input <- ifelse(df1$N_boundaries_input==0, "degenerate", ifelse(df1$N_boundaries_input==1, "continous", "disjoint"))
systype_output <- ifelse(df1$N_boundaries==0, "degenerate", ifelse(df1$N_boundaries==1, "continous", "disjoint"))
df1 <- cbind(df1,systype_input,systype_output)

systype_input <- ifelse(df2$N_boundaries_input==0, "degenerate", ifelse(df2$N_boundaries_input==1, "continous", "disjoint"))
systype_output <- ifelse(df2$N_boundaries==0, "degenerate", ifelse(df2$N_boundaries==1, "continous", "disjoint"))
df2 <- cbind(df2,systype_input,systype_output)

a <- table(df1$systype_input,df1$systype_output)
"         continous disjoint
continous       177       14
disjoint         97      354
"

b <- table(df2$systype_input,df2$systype_output)
"          continous degenerate disjoint
continous        186          4       43
degenerate         2         14        3
disjoint         134         15      291
"

all_ins <- unlist(list(df1$systype_input,df2$systype_input))
all_outs <- unlist(list(df1$systype_output,df2$systype_output))

all <- table(all_ins,all_outs)

# normalize to each row sums to one
pall <- all/rowSums(all)

# get stationary distribution
stat_dist <- function(matrix) { # returns the stationary distribution of a RIGHT stochastic matrix (rows sum to one)
	vec <- eigen(t(matrix))$vectors[,1]
	return(vec/sum(vec))
}
stat_dist(pall) # 0.6264775 0.3299556 0.0435669



