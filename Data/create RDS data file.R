require(tidyverse)

df1 <- read.csv("../Data/experiment1_FINAL.csv",colClasses=c(system1024="character",system512="character",system1024_input="character",system512_input="character"))
df2 <- read.csv("../Data/experiment2_FINAL.csv",colClasses=c(system1024="character",system512="character",system1024_input="character",system512_input="character"))

# remove some columns
remove_columns <- function(df) {
    df <- select(df,-c(
        X,
        jointime_user,
        jointime_user_UTC,
        bonused,
        duration_callback,
        n_label1,
        n_label1_input,
        n_label1_change,
        p_input,
        p_output,
        p_change,
        chain,
        generation
        )
    )
    return(df)
}

df1 <- remove_columns(df1)
df2 <- remove_columns(df2)

df2 <- select(df2,-c(X.1)) # there was one extra column in df2 only called "X.1"

# rename some columns
rename_columns <- function(df) {
    df <- df %>% rename(
        lineage = trajectory, 
        duration_experiment_minutes = whole_experiment,
        data_entry_created = created,
        duration_final_round = duration_round
    )
    return(df)
}

df1 <- rename_columns(df1)
df2 <- rename_columns(df2)

saveRDS(df1, file = "experiment1.rds")
saveRDS(df2, file = "experiment2.rds")

# example: read one back in
remove(df1)  # first remove the existing copy of df1 from the workspace
df1 <- readRDS("/Users/vanferdi/Library/Mobile Documents/com~apple~CloudDocs/Research/PROJECTS/Category Change/Analyses Archive/experiment1.rds")


##########################################################################################
# notes

# C trajectories are numbered 1-45, I trajectories are numbered with the session_code



