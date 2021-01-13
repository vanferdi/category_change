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