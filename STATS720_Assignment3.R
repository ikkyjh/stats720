#IKRAM JMOUHI 400550954
#STATS720 ASSIGNMENT 3

# Load required libraries
library(mlmRev)
library(lmerTest)
library(nlme)
library(ggplot2); theme_set(theme_bw())
library(lme4)
library(glmmTMB)
library(broom.mixed)
library(car)
library(dplyr)
library(pbkrtest)
library(dotwhisker)




# Fit models
lmer_model <- lmer(cog ~ age + trt + (1 + age | id), data = Early)
summary(lmer_model)
lme_model <- lme(fixed = cog ~ age + trt, random = ~ age | id, data = Early, control = lmeControl(opt = "optim"))
summary(lme_model)

# Compare log-likelihoods
logLik(lmer_model)
logLik(lme_model)
#The two likelihoods  are very similar to each other so it would be hard 
#to say which model is a better fit 
#the lme model is slightly a bit higher than the lmer model so I would say lme is a better fit

#2)

# Create a scaled version of the dataset with age scaled
scaled_data <- Early
scaled_data$age <- scale(Early$age)

# Fit a linear mixed model with lmer using scaled age
lmer_model_scaled <- lmer(cog ~ age + trt + (1 + age | id), data = scaled_data)

# Fit a linear mixed model with lme using scaled age
lme_model_scaled <- lme(fixed = cog ~ age + trt, random = ~1 + age | id, data = scaled_data, control = lmeControl(opt = "optim"))

# Extract fixed-effect coefficients
lmer_coefficients <- fixef(lmer_model_scaled)
lme_coefficients <- fixef(lme_model_scaled)

# Create coefficient plots
dwplot(lmer_model_scaled)
dwplot(lme_model_scaled)

# fixed-effect coefficients
print("lmer Coefficients:")
print(lmer_coefficients)
print("lme Coefficients:")
print(lme_coefficients)

# Get denominator degrees of freedom
anova_lme <- anova(lme_model_scaled)
anova_lmer <- anova(lmer_model_scaled)

# Display denominator degrees of freedom
print("lmer Denominator Degrees of Freedom:")
print(anova_lmer)
print("lme Denominator Degrees of Freedom:")
print(anova_lme)

# Calculate the difference in standard errors between the two models
se_lmer <- sqrt(diag(vcov(lmer_model_scaled)))
se_lme <- sqrt(diag(vcov(lme_model_scaled)))

se_difference <- se_lmer - se_lme

# Display the difference in standard errors
print("Difference in Standard Errors:")
print(se_difference)


#The estimates for the intercept, 'age,' and 'trtY' exhibit
#a high degree of similarity between the lmer and lme models.
#While slight differences exist, they generally fall within the same range.

#However, some distinctions emerge in the estimated denominator degrees of freedom.
#Specifically, for the 'age' variable, the lme model reports 205 degrees of freedom, 
#whereas the lmer model indicates approximately 108.83 degrees of freedom. 
#for 'trt,' both models share a similar count of 101 degrees of freedom.

#the differences in the estimated errors are minuscule, 
#indicating a high degree of similarity in the estimates between the two models. 
#To sum up, while there are subtle variations in the degrees of freedom for 'age,' 
#the estimates for the coefficients and errors are generally very close between the lmer 
#and lme models."



#3) 
anova(early_sca_lmer,ddf="Satterthwaite")
anova(early_sca_lmer,ddf="Kenward-Roger")

#the variations in degrees of freedom (ddf) between the Satterthwaite and Kenward-Roger
#approximations seem relatively minor. These differences, however, do not seem to exert 
#a substantial influence on the outcomes. 
#This is evident as both methods lead statistically significant p-values (< 0.001) 
#for the predictors 'age' and 'trt,' suggesting a robust statistical significance
#for these variables regardless of the specific approximation method employed.


#4) 
# Extract random effects for the 'age' predictor
# Extract random effects
random_effects <- ranef(early_sca_lmer)

# Create a data frame for random effects
random_effects_df <- data.frame(
  RandomIntercept = random_effects[[1]]$`(Intercept)`,
  RandomAgeEffect = random_effects[[1]]$age
)

# Create the scatter plot
with(random_effects_df, plot(
  RandomIntercept,
  RandomAgeEffect,
  xlab = "Random intercept",
  ylab = "Random effect of age",
  main = "random effect of age for each level against the corresponding random intercept"
))

#In the context of our analysis, treating 'trt' as a random variable seems inappropriate
#as it represents a binary outcome—'YES' or 'NO'—indicating whether an infant
#was part of the treatment group exposed to an enriched environment. 
#Since this characteristic doesn't naturally vary between individuals, 
#it is more sensible to employ 'trt' as a fixed effect. 
#This allows us to explore how belonging to the treatment group influences the outcome.

#On the other hand, we choose to model 'age' both as a random and fixed effect. 
#This decision is rooted in the desire to account for variations in 'age' across individuals 
#while simultaneously examining its impact within the observed age range in our dataset. 
#By employing this approach, we can effectively capture the diversity of 'age' values 
#in our sample, offering a comprehensive exploration of how 'age' influences
#cognitive scores within the specific context of our dataset.

# 
# Fit the model with independent intercept and age variation
model_random_slope <- lmer(cog ~ age + trt + (1 + age | id), data = early_scaled)

# Fit the model with intercept variation only
model_intercept_only <- lmer(cog ~ age + trt + (1 | id), data = early_scaled)

# Perform LRT to compare models
lrt_random_slope <- anova(early_sca_lmer, model_random_slope)
lrt_intercept_only <- anova(early_sca_lmer, model_intercept_only)
#when we have models with random slopes, they demonstrate equivalence, 
#by having identical values for AIC, BIC, log-likelihood, and deviance. 
#In scenarios involving models with solely an intercept as a random effect,
#the preference leans towards the model with random slopes. 
#However,the distinction between the two models is not statistically significant, as indicated by the results of the Likelihood Ratio Test.

#parametric bootstrap 

#model with independent slope/intercept vs. full model
pb_independent <- PBmodcomp(early_sca_lmer, model_random_slope, nsim = 1000)
PBmodcomp(largeModel=model_random_slope,smallModel=model_intercept_only)

#model with intercept only vs. full model
boot_intercept_only <- bootMer(model_intercept_only, nsim = 1000)
PBmodcomp(largeModel=early_sca_lmer,smallModel=model_random_slope)



