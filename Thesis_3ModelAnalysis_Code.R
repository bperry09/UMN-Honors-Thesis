##Pre-processing:
library(readxl)
cancer <- read_excel("C:/Users/Bailey/Desktop/THESIS/ThesisPrep_Data_Draft2.xlsx")
#Remove the nulls - validated via the census information
cancer <- na.omit(cancer)
View(cancer)
dim(cancer)

cases <- cancer$`2010-14 Incidence`
white <- cancer$`% White`
black <- cancer$`% Black`
asian <- cancer$`% Asian`
pop <- cancer$Population
age <- cancer$`% Over 65`
income <- cancer$`Average Income`
smoke <- cancer$`% Tobacco Use`
insured <- cancer$`% Population Insured`
work <- cancer$`% Females (16+) in Laborforce`
miles <- cancer$`Mileage to Nearest Hospital`

##Plot the Data - Exploratory Phase
pairs(~cases+white+black+asian+pop+age+income+smoke+insured+work+miles, main="Simple Scatterplot Matrix")
#Some zoomed in examples of potential issues in the data
plot(income~asian)
plot(insured~pop)
#checking to see if variance is fanning out

#Underlying distribution
hist(cases) #right-skewed

#1. Fit model with all cases - full glm
fit1 <- glm(cbind(cases, pop-cases) ~ (white+black+asian+pop+age+income+smoke+insured+work+miles)^2, family=binomial(link="logit"))
#main effects and all interaction terms
summary(fit1)

#2. Diagnostics
par(mfrow = c(2,2))
plot(fit1)
par(mfrow = c(1,1))
# NO EDITING OR TRANSFORMATIONS - original data

#Comments: the data does not fit perfectly as we can see from the qqplot tails
# But there seem to be values that are highlighted to consider as influential
# or outliers (points 4, 58, 167, and 176) and they could be affecting this
# The residuals v fitted plot looks relatively good for taking the data as is

#3. Backwards elimination on model with everything (main and interactions) 
# to see what it comes up with & diagnostics on that

#Use backward elimination to confirm best regressors
#fit1 will be the full model
full <- fit1
null <- lm(cases ~ 1) #null is just the response with intercept
s1 <- step(full, scope=list(lower=null, upper=full), direction="backward", k=2)
summary(s1)

#Comments: backwards elimination found a lot more of the predictors 
# to be significant, the inconsistency may suggest some tweaks to the data
# Continue with next steps, and then return to these models for comparison

#4. Transform variables (not the response) - remove zeros from dataset 
# and add one to black and asian; possibly use judgement and do log of pop and 
# income instead of following just powertransform recommendations; 
# BUT avoid super large powers, if necessary change them to levels 
# (ie smoke or insurance being low, medium, high, etc)

#remove cases that have zeros from work and age; indices 51 and 136
cancer1 <- cancer[-c(51, 136),]
View(cancer1)

#Reset the variables for the cancer dataset that excludes the zero values
cases1 <- cancer1$`2010-14 Incidence`
white1 <- cancer1$`% White`
black1 <- cancer1$`% Black`
asian1 <- cancer1$`% Asian`
pop1 <- cancer1$Population
age1 <- cancer1$`% Over 65`
income1 <- cancer1$`Average Income`
smoke1 <- cancer1$`% Tobacco Use`
insured1 <- cancer1$`% Population Insured`
work1 <- cancer1$`% Females (16+) in Laborforce`
miles1 <- cancer1$`Mileage to Nearest Hospital`

#need to handle the zeros for black and asian in order to use powerTransform
hist(black1)
hist(asian1)
#based on the plots, adding one would be appropriate for handling zeros
#add one to the zeros in black and asian
black2 <- (black1+1)
asian2 <- (asian1+1)
library(alr4)
library(MASS)
####Multivariate Box Cox Method
#Transform the regressors together
transform1 <- powerTransform(cbind(white1,black2,asian2,age1,income1,smoke1,insured1,work1,miles1) ~ 1)
lambdas1 <- summary(transform1)
lambdas1
#just to see what may be suggested from the function

#Comments: Clearly there are some heroic powers being suggested which means
# we may want to tweak the data to try and be more normal WITHOUT using a 
# power transformation; ie insured will be split into levels - 
# low-high (70-80%), middle-high (80-90%), top-high (90-100%)
#Smoke will be left alone because all of the values are concentrated from
# 15.9-21.8%

# The log of pop and income (by judgement), as well as 
# black and asian will be done and we will see how this changes things
popt <- log(pop)
incomet <- log(income)
blackt <- log(black+1) #add one to do the log(x+1) trans - maps 0 to 0
asiant <- log(asian+1) #same case as black
inslevels <- cancer$`% Insured Levels` # THIS DID NOT HELP - IGNORE

fit2 <- glm(cbind(cases, pop-cases) ~ (white+blackt+asiant+popt+age+incomet+smoke+insured+work+miles)^2, family=binomial(link="logit"))
#main effects and all interaction terms
summary(fit2)

#5. Diagnostics
par(mfrow = c(2,2))
plot(fit2)
par(mfrow = c(1,1))
# POST TRANSFORMATIONS - changed data
#PLOT looks worse than original model; tails pull off qqplot
# Residuals v fitted skew to one side of the plot

#Try with quasibinomial
fit2.1 <- glm(cbind(cases, pop-cases) ~ (white+blackt+asiant+popt+age+incomet+smoke+insured+work+miles)^2, family=quasibinomial)
#main effects and all interaction terms
summary(fit2.1)
par(mfrow = c(2,2))
plot(fit2.1)
par(mfrow = c(1,1))
#quasibinomial doesn't do much to improve normality and the diagnostic plots

#Try with ONLY transforming income, black, asian (since response depends on pop)
fit2.2 <- glm(cbind(cases, pop-cases) ~ (white+blackt+asiant+pop+age+incomet+smoke+insured+work+miles)^2, family=binomial(link="logit"))
#main effects and all interaction terms
summary(fit2.2)
par(mfrow = c(2,2))
plot(fit2.2)
par(mfrow = c(1,1))

#Residuals v fitted saw improvement, but the qqplot still did not look the best

#TRY ONLY transforming income
fit2.3 <- glm(cbind(cases, pop-cases) ~ (white+black+asian+pop+age+incomet+smoke+insured+work+miles)^2, family=binomial(link="logit"))
#main effects and all interaction terms
summary(fit2.3)
par(mfrow = c(2,2))
plot(fit2.3)
par(mfrow = c(1,1))
#SIMILAR to original model, try looking at the significant ones from this compared
# to no transformation model

#Tried doing cbind(cases, pop-cases) on log of every variable
# DID NOT HELP, plots showed essentially NO change

#6. Backwards elimination of transformed variables and the interaction terms 
# and diagnostics of the model that produces
#Use backward elimination to confirm best regressors
#fit1 will be the full model
full <- fit2
null <- lm(cases ~ 1) #null is just the response with intercept
s2 <- step(full, scope=list(lower=null, upper=full), direction="backward", k=2)
summary(s2)

#Comments: Backwards elimination again finds a lot of the terms to be significant
# Would need to see if there are main effects that are NOT considered significat,
# but that have significant interactions because then they should stay in 

#BE on the third transformation model
full <- fit2.3 #only income is transformed in this
null <- lm(cases ~ 1) #null is just the response with intercept
s2.3 <- step(full, scope=list(lower=null, upper=full), direction="backward", k=2)
summary(s2.3)


#7. Fit the model for rate (lm for cases/pop)
fit3 <- lm((cases/pop) ~ (white+black+asian+pop+age+income+smoke+insured+work+miles)^2, weights=(1/pop))
summary(fit3)
#weighted due to non constant variance

#8. Diagnostics
par(mfrow = c(2,2))
plot(fit3)
par(mfrow = c(1,1))

#qqplot has clear highlighted points, but overall follows the line
#residuals v fitted looks terrible
#(tried fitting it without pop as a predictor, still had bad residvfitted graph)

#Try fitting rate with transformed income, black, and asian
fit3.1 <- lm((cases/pop) ~ (white+blackt+asiant+pop+age+incomet+smoke+insured+work+miles)^2, weights=(1/pop))
summary(fit3.1)
par(mfrow = c(2,2))
plot(fit3.1)
par(mfrow = c(1,1))
#qqplot is seeing less issues with possible outliers
#residuals v fitted is not seeing much improvement -- one case to far right! 
# looking at ways to fix that

#Investigate shapes of data in model
hist(white) # may need to log this too (lots of high, only a few low)
hist(blackt)
hist(asiant)
hist(pop) # may need to log this, but how does that affect the rate...
hist(age)
hist(incomet)
hist(smoke) #logging wouldn't help this separation
hist(insured)
hist(work) 
hist(log(work)) #could log it...
hist(miles) #should be fine

whitet <- log(white)
#Try model with log(white)
fit3.2 <- lm((cases/pop) ~ (whitet+blackt+asiant+pop+age+incomet+smoke+insured+work+miles)^2, weights=(1/pop))
summary(fit3.2)
par(mfrow = c(2,2))
plot(fit3.2)
par(mfrow = c(1,1))

#9. Look for influential points/outliers - reconsider analysis
#Probably original logistic regression with reconsideration of specific points
n <- 218
outlierTest(fit1, cutoff = 1*n, n.max = n, order = TRUE)
#gave us cases 4, 58, 141

#Get the value of cook's distance of each point
cook <- cooks.distance(fit1)
#Plot the cook's distance
plot(cook,ylab="Cooks distances")
#Find the maximum value
maxval <- max(cook)
maxval ##Corresponds to case 138

#10. Analysis with removal of outliers

#remove cases that are outliers; indices 4,58, and 141
cancer2 <- cancer[-c(4, 58, 141),]
View(cancer2) #215 entries, good to use

#Reset the variables for the cancer dataset that excludes the zero values
cases2 <- cancer2$`2010-14 Incidence`
white2 <- cancer2$`% White`
black2 <- cancer2$`% Black`
asian2 <- cancer2$`% Asian`
pop2 <- cancer2$Population
age2 <- cancer2$`% Over 65`
income2 <- cancer2$`Average Income`
smoke2 <- cancer2$`% Tobacco Use`
insured2 <- cancer2$`% Population Insured`
work2 <- cancer2$`% Females (16+) in Laborforce`
miles2 <- cancer2$`Mileage to Nearest Hospital`

fitout1 <- glm(cbind(cases2, pop2-cases2) ~ (white2+black2+asian2+pop2+age2+income2+smoke2+insured2+work2+miles2)^2, family=binomial(link="logit"))
#main effects and all interaction terms
summary(fitout1)
plot(fitout1)
#DOES this actually help, or does it just allow other points to be new outliers...
n <- 215
outlierTest(fitout1, cutoff = 1*n, n.max = n, order = TRUE)
#Cases 207, 54, 135, and 60 are all newly identified upon removing the other 3

popt2 <- log(pop2)
incomet2 <- log(income2)
blackt2 <- log(black2+1) #add one to do the log(x+1) trans - maps 0 to 0
asiant2 <- log(asian2+1)
fitout2 <- glm(cbind(cases2, pop2-cases2) ~ (white2+blackt2+asiant2+popt2+age2+incomet2+smoke2+insured2+work2+miles2)^2, family=binomial(link="logit"))
#main effects and all interaction terms
summary(fitout2)
plot(fitout2)
n <- 215
outlierTest(fitout2, cutoff = 1*n, n.max = n, order = TRUE)
#AGAIN, new outliers show up when the original ones are removed

fitout3 <- lm((cases2/pop2) ~ (white2+black2+asian2+pop2+age2+income2+smoke2+insured2+work2+miles2)^2, weights=(1/pop2))
summary(fitout3)
plot(fitout3)
n <- 215
outlierTest(fitout3, cutoff = 1*n, n.max = n, order = TRUE)
#AGAIN, new outliers show up when the original ones are removed

#Could try to extract values more than 3 sd from the mean
mean <- mean(cases) #145.35
sd <- sd(cases) #85.72
sd3 <- sd*3
upper <- mean+sd3
#remove values that are larger than mean+sd3=402.51
cancer2 <- cancer[-c(43, 47),]
#redo variables above HERE so that they don't have the cases listed
fitout1 <- glm(cbind(cases2, pop2-cases2) ~ (white2+black2+asian2+pop2+age2+income2+smoke2+insured2+work2+miles2)^2, family=binomial(link="logit"))
#main effects and all interaction terms
summary(fitout1)
plot(fitout1)
#DOES this actually help, or does it just allow other points to be new outliers...
n <- 216
outlierTest(fitout1, cutoff = 1*n, n.max = n, order = TRUE)
#STILL has outliers, doesn't change much of the graphs at all; not worth investigating further

#Also look at other things to do with identified outliers
#Unless if we think one of the models above works well enough
#NEED to review the backwards selection models, BUT the first model with all terms and
#interactions has decent plots... consider comparing the 1st model, the poisson model
# AND a transformed linear model (but change the powers on that one)
