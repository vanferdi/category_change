require(lme4)

d1 <- readRDS("../experiment1.rds")

j1 <- subset(d1,iteration < 9)


#######################################################################
# cogsci lmer replication
#######################################################################

full <- lmer(N_boundaries ~ condition * iteration + (1|trajectory), data=j1)
r1 <- lmer(N_boundaries ~ condition + iteration + (1|trajectory), data=j1)
anova(full,r1) # lose the interaction
anova(r1,full)
"     Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
r1    5 2067.8 2090.0 -1028.9   2057.8                         
full  6 2069.2 2095.9 -1028.6   2057.2 0.5656      1      0.452"

r2 <- lmer(N_boundaries ~ condition + (1|trajectory), data=j1)
anova(r1,r2) # keep iteration
"   Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
r2  4 2143.7 2161.5 -1067.8   2135.7                             
r1  5 2067.8 2090.0 -1028.9   2057.8 77.924      1  < 2.2e-16 ***"

r3 <- lmer(N_boundaries ~ iteration + (1|trajectory), data=j1)
anova(r1,r3) # keep condition
"   Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
r3  4 2071.3 2089.2 -1031.7   2063.3                           
r1  5 2067.8 2090.0 -1028.9   2057.8 5.5695      1    0.01828 *"

best <- r1
summary(best)
"            Estimate Std. Error t value
(Intercept)  2.42911    0.19381  12.533
conditionI   0.52713    0.22293   2.365
iteration   -0.22182    0.02299  -9.648"


#######################################################################
# lmer
#######################################################################

full <- lmer(N_boundaries ~ distribution * condition * iteration + (1|trajectory), data=j1)
r1 <- lmer(N_boundaries ~ distribution * condition + iteration + (1|trajectory), data=j1)
anova(full,r1) # keep interaction with iteration
"     Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
r1    9 2073.4 2113.5 -1027.7   2055.4                             
full 14 2058.0 2120.3 -1015.0   2030.0 25.449      5  0.0001141 ***"

r2 <- lmer(N_boundaries ~ distribution + condition * iteration + (1|trajectory), data=j1)
anova(full,r2) # keep interaction with distribution
"     Df  AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
r2    8 2073 2108.6 -1028.5     2057                             
full 14 2058 2120.3 -1015.0     2030 27.038      6  0.0001424 ***"

# ok full model seems like it is going to be the best

r3 <- lmer(N_boundaries ~ distribution * condition + (1|trajectory), data=j1)
anova(full,r3) # keep iteration

r4 <- lmer(N_boundaries ~ distribution * iteration + (1|trajectory), data=j1)
anova(full,r4) # keep condition

r5 <- lmer(N_boundaries ~ condition * iteration + (1|trajectory), data=j1)
anova(full,r5) # keep distribution

best <- full
summary(full)
"                                   Estimate Std. Error t value
(Intercept)                         1.97177    0.37107   5.314
distributionR                       0.88538    0.53318   1.661
distributionU                       0.84394    0.52268   1.615
conditionI                          1.05972    0.45436   2.332 <-
iteration                          -0.14330    0.06849  -2.092 <-
distributionR:conditionI           -1.37584    0.64854  -2.121 <-
distributionU:conditionI           -0.65246    0.64076  -1.018
distributionR:iteration            -0.29133    0.10697  -2.723 <-
distributionU:iteration            -0.08795    0.09442  -0.931
conditionI:iteration               -0.09020    0.08355  -1.080
distributionR:conditionI:iteration  0.45739    0.12593   3.632 <-
distributionU:conditionI:iteration -0.01847    0.11584  -0.159"

# relevel distribution with U as the baseline to help interpret shit

j1$distribution <- relevel(j1$distribution, ref="U")
best <- lmer(N_boundaries ~ distribution * condition * iteration + (1|trajectory), data=j1)
summary(best)
"                                   Estimate Std. Error t value
(Intercept)                         2.81571    0.36810   7.649
distributionL                      -0.84394    0.52268  -1.615
distributionR                       0.04144    0.53112   0.078
conditionI                          0.40727    0.45180   0.901
iteration                          -0.23125    0.06500  -3.558 <-
distributionL:conditionI            0.65246    0.64076   1.018
distributionR:conditionI           -0.72338    0.64676  -1.118
distributionL:iteration             0.08795    0.09442   0.931
distributionR:iteration            -0.20337    0.10477  -1.941
conditionI:iteration               -0.10866    0.08024  -1.354
distributionL:conditionI:iteration  0.01847    0.11584   0.159
distributionR:conditionI:iteration  0.47585    0.12375   3.845 <-
"
j1$distribution <- relevel(j1$distribution, ref="R")
best <- lmer(N_boundaries ~ distribution * condition * iteration + (1|trajectory), data=j1)
summary(best)
"                                   Estimate Std. Error t value
(Intercept)                         2.85715    0.38287   7.462
distributionU                      -0.04144    0.53112  -0.078
distributionL                      -0.88538    0.53318  -1.661
conditionI                         -0.31611    0.46278  -0.683
iteration                          -0.43462    0.08217  -5.289 <-
distributionU:conditionI            0.72338    0.64676   1.118
distributionL:conditionI            1.37584    0.64854   2.121 <-
distributionU:iteration             0.20337    0.10477   1.941
distributionL:iteration             0.29133    0.10697   2.723 <-
conditionI:iteration                0.36719    0.09422   3.897 <-
distributionU:conditionI:iteration -0.47585    0.12375  -3.845 <-
distributionL:conditionI:iteration -0.45739    0.12593  -3.632 <-
"
# directions across all relevelings:
# iteration loses boundaries

# wtf is happenin
table(j1$N_boundaries,j1$distribution)

# look at the systems in this complex interaction
s <- subset(j1,distribution=="R" & condition=="I")
table(s$N_boundaries,s$iteration)

s <- subset(j1,distribution=="R" & condition=="I" & N_boundaries>2)
s$system512


x <- lmer(N_boundaries ~ distribution + condition + iteration + (1|trajectory), data=j1)










#######################################################################
# plot
#######################################################################

# forgot to include iteration
full <- lmer(N_boundaries ~ distribution * condition + (1|trajectory), data=d1)
r1 <- lmer(N_boundaries ~ distribution + condition + (1|trajectory), data=d1)
anova(full,r1) # lose the interaction
"     Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
r1    6 2191.6 2218.4 -1089.8   2179.6                         
full  8 2193.4 2229.1 -1088.7   2177.4 2.2145      2     0.3305"

r2 <- lmer(N_boundaries ~ distribution + (1|trajectory), data=d1)
anova(r1,r2) # keep condition
"   Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)   
r2  5 2196.7 2219.0 -1093.3   2186.7                            
r1  6 2191.6 2218.4 -1089.8   2179.6 7.1016      1   0.007702 **"

r3 <- lmer(N_boundaries ~ condition + (1|trajectory), data=d1)
anova(r1,r3) # lose distribution
"   Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
r3  4 2187.7 2205.6 -1089.9   2179.7                         
r1  6 2191.6 2218.4 -1089.8   2179.6 0.1221      2     0.9408"

best <- r3

summary(best)
"            Estimate Std. Error t value
(Intercept)   1.7865     0.1623  11.007
conditionI    0.5292     0.1976   2.678"




