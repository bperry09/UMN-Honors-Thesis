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

#THE FOLLOWING DISTRIBUTIONS AND MODELS WILL BE INVESTIGATED: quasibinomial,
# quasipoisson, and a linear model of rate:

#1. Fit Binomial in order to do AIC-based backwards selection
fitbin <- glm(cbind(cases, pop-cases) ~ (white+black+asian+pop+age+income+smoke+insured+work+miles)^2, family=binomial(link="logit"))
## Changed backwards elimination of the models to k=4 to be more strict
full <- fitbin
null <- lm(cases ~ 1) #null is just the response with intercept
s1 <- step(full, scope=list(lower=null, upper=full), direction="backward", k=4)
summary(s1)

#Variables to include are as follows:
#white + black + asian + 
#pop + age + income + smoke + insured + work + miles + white:asian + 
#white:pop + white:age + white:income + white:insured + white:miles + 
#black:pop + black:age + black:income + black:miles + asian:age + 
#asian:income + asian:miles + pop:age + pop:miles + age:work + 
#income:insured

#Quasibinomial with ALL terms
fitqb <- glm(cbind(cases, pop-cases) ~ (white+black+asian+pop+age+income+smoke+insured+work+miles)^2, family=quasibinomial(link="logit"))
plot(fitqb)

# Now fit QUASIBINOMIAL model with identified significant cases from BE
##a. Model
fit1 <- glm(cbind(cases, pop-cases) ~ white+black+asian+pop+age+income+smoke+insured+work+miles+white:asian+white:pop + white:age + white:income + white:insured + white:miles + 
              black:pop + black:age + black:income + black:miles + asian:age + asian:income + asian:miles + pop:age + pop:miles + age:work + 
              income:insured, family=quasibinomial(link="logit"))
#main effects and interaction terms as specified above
summary(fit1)
##b. Diagnostics
par(mfrow = c(2,2))
plot(fit1)
par(mfrow = c(1,1))

## Reduces the amount of variables included which is good for VIF, and the plots
# look just about as good as with all the main effects and interactions
# The tails pull off a tiny bit more, BUT doesn't have a significant shape

## We are missing parameters so a perfect model won't happen

#2. Fit Poisson in order to do AIC-based backwards selection (w/ offset=pop)
fitpoi <- glm(cases ~ (white+black+asian+pop+age+income+smoke+insured+work+miles)^2, offset=log(pop), family=poisson(link="log"))
## Changed backwards elimination of the models to k=4 to be more strict
full <- fitpoi
null <- lm(cases ~ 1) #null is just the response with intercept
s2 <- step(full, scope=list(lower=null, upper=full), direction="backward", k=4)
summary(s2)

#Terms from BE: work + miles + white:asian + white:pop + 
# white:age + white:income + white:insured + white:miles + 
# black:pop + black:age + black:income + black:miles + asian:age + 
# asian:income + asian:miles + pop:age + pop:miles + age:work + 
# income:insured

##Quasipoisson with ALL terms
fitqp <- glm(cases ~ (white+black+asian+pop+age+income+smoke+insured+work+miles)^2, offset=log(pop), family=quasipoisson(link="log"))
par(mfrow=c(2,2))
plot(fitqp)
#Residuals v fitted show that some values have lower predictions, may be interesting to 
#investigate those points
# Need to look at 210 and 55 in a Pearson residual plot so that weights are accted for 
pearsresid <- residuals(fitqp, type="pearson")
par(mfrow=c(1,1))
plot(pearsresid~fitted(fitqp))
#do this to identify case number
text(fitted(fitqp), pearsresid)
#Case 210 stands out still - zip code 60157; then 55 and 218 as shown in previous

# Fit QUASIPOISSON model with BE identified terms
##a. Model
fit2 <- glm(cases ~ work + miles + white:asian + white:pop + white:age + white:income + white:insured + white:miles + 
              black:pop + black:age + black:income + black:miles + asian:age + 
              asian:income + asian:miles + pop:age + pop:miles + age:work + 
              income:insured, offset=log(pop), family=quasipoisson(link="log"))
#mixed main effects and interaction terms
summary(fit2)
##b. Diagnostics
par(mfrow = c(2,2))
plot(fit2)
par(mfrow = c(1,1))

#Plots look relatively good, but we want to see what 210 and 55 look like with Pearson
pearsresid2 <- residuals(fit2, type="pearson")
par(mfrow=c(1,1))
plot(pearsresid2~fitted(fit2))
#do this to identify case number
text(fitted(fit2), pearsresid2)
## They again show up with higher residuals; will need to investigate them for outlier test

#3. Fit RATE model with all cases - full glm
##a. Model
fit3 <- lm((cases/pop) ~ (white+black+asian+pop+age+income+smoke+insured+work+miles)^2, weights=(pop))
summary(fit3)
#weighted due to non constant variance
##b. Diagnostics
par(mfrow = c(2,2))
plot(fit3)
par(mfrow = c(1,1))
##c. Backwards Elimination
full <- fit3
null <- lm(cases ~ 1) #null is just the response with intercept
s3 <- step(full, scope=list(lower=null, upper=full), direction="backward", k=4)
## This one runs
summary(s3)
