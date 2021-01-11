# Data

Explanation of the columns in the file `experiment1_FINAL.csv` and `experiment2_FINAL.csv`

(Right now it just contains the info relevant for understanding `frequency1.R`)

`participant` unique ID per participant

`trajectory` unique ID per iterated learning chain.  All data from the same IL chain shares the same trajectory ID.

`condition` C = cultural, I = individual

`iteration` Numeric ranging from 1 to 11, showing the age of the category system. In the cultural condition, this corresponds to the generation number.  In the individual condition, this corresponds to the round number. Refers to the category system that participants produced in the testing phase of the generation/round (not the one they were trained on).

`distribution`  U = uniform, R = right skew (bright shell most common), L = left skew (dark shell most common)

left and right here correspond to an indexable array containing the greyscale values of each stimulus: <br>
`[25,50,75,100,125,150,175,200,225,250]` <br>
and an indexable array containing the number of training trials per color: <br>
`[10,5,4,3,2,2,1,1,1,1]` being the left skew <br>
`[1,1,1,1,1,2,2,4,5,15]` being the righft skew <br>
`[3,3,3,3,3,3,3,3,3,3]` being the uniform distribution <br>
FYI: `25` is almost black and `250` is almost white.

`testset` an array of length 10 that shows which stimulus was displayed on which test trial. <br> Example `[1,2,5,6,7,4,9,3,8,0]`: here, stim `50` was displayed on the first test trial, stim `25` was displayed on the last test trial.

`system512` a string representation of the category system, such as "1100000001". The 10 digits map on to the stim color array and tell you which category label was used for each stim. <br> 
1 = category label A (`word0`) and 0 = category label B (`word1`). <br>
In "1100000001", three stims `25`, `50`, and `250` have category label B.
