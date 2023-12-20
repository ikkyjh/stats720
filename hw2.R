## ---
## title: "STATS - 720 Assignment-2"
## author: "Ikram Jmouhi (Student number: 400550954)"
## ---

library(rpart)
library(ggplot2)
library(performance)
library(DHARMa)
library(coefplot)
library(bbmle)
library(brglm2)
library(brms)
library(arm)
library(lmtest)

############################################################"
#1
data("kyphosis")

#######Analyzing the data 
summary(kyphosis)
#boxplot 
boxplot(Age ~ Kyphosis, data = kyphosis, main = "Age by Kyphosis Status")
## Bar Chart of Kyphosis Status
barplot(table(kyphosis$Kyphosis), main = "Kyphosis Status")

#the number of children with kyphosis is significantly lower than kids without kyphosis

#glm model
kyphosis_model <- glm(Kyphosis ~ Age + Start + Number, data = kyphosis, family = binomial(link = "logit"))
summary(kyphosis_model)

#"Start" and "Number" are statistically significant predictors of the presence or absence 
#of kyphosis, as indicated by their low p-values (0.00229 and 0.06785)
#The coefficient for "Age" is not statistically significant
#The model improves the fit compared to a null model, as indicated by the residual deviance (lower than null deviance)

## BMB: latter is always true??

# Create diagnostic plots
plot(kyphosis_model)

##the first plot is the residuals vs fitted plot : 
#it seems that it is not horizontally straight, the assumption of constant variance (homoscedasticity) is not met
#this suggests that the model is not a good fit. 
#the qq plot suggest that our data is not normally distributed , the points deviate from the straight line,
#the scale location plot : the points aren't scattered evenly along the line, the variance of the residuals is not constant, the assumption of homoscedasticity is not met. 


#performance 
check_model(kyphosis_model)
#using DHARMA 
fitted_model <- simulateResiduals(kyphosis_model)
plot(fitted_model)

#DHARMa simulates residuals based on the model's assumptions and compares them to the observed 
#residuals. This makes it powerful for identifying issues like overdispersion.
#the plot of normality is slightly better using DHarma, this is because DHARMA is specialized
#in GLMs and uses a uses a simulation-based approach to assess normality.
#DHARMa generates Q-Q plots that are customized for GLMs.
#It compares the quantiles of the simulated residuals to the expected quantiles under the normal distribution. 

#Coefficents 
kyphosis_model$coefficients

#the age coefficient is 0.01093048 which means that for a one-unit increase in Age 
#the log-odds of having Kyphosis increases by about 0.0109 unit assuming all other variables are constant
#the start coefficient is -0.20651005, this means that as the starting vertebra number increases by one unit
#the odds of having Kyphosis decrease by a factor of exp(-0.2065).
#the coefficient for number is 0.41060119, this means that as the number of vertebrae involved
#increases by one unit, the odds of having Kyphosis increase by a factor of exp(0.4106), which is greater than 1. 
#It represents an increase in odds as the number of vertebrae involved increases.

#Coefficient plot 
coefplot(kyphosis_model, horizontal = TRUE)

## BMB: coef plots with unscaled predictors are always problematic ...

##################################################################
#QUESTION 2 

g_url <- "https://raw.githubusercontent.com/bbolker/mm_workshops/master/data/gopherdat2.csv"
g_data <- read.csv(g_url)


#Bar plot of shell types by year 
ggplot(g_data, aes(x = year, fill = type)) +
  geom_bar(position = "dodge") +
  labs(x = "Year", y = "Count") +
  ggtitle("Bar Plot of Shell Types by Year")

#"Scatterplot of Shells vs. Seroprevalence
ggplot(g_data, aes(x = shells, y = prev, color = type)) +
  geom_point() +
  labs(x = "Number of Shells", y = "Seroprevalence") +
  ggtitle("Scatterplot of Shells vs. Seroprevalence")


################ models 
# Fit a Poisson GLM
poisson_model <- glm(shells ~ year + prev + offset(log(Area)), data = g_data, family = poisson)
poisson_model
#Checking to see if we have overdispersion 
residual_deviance <- sum(residuals(poisson_model, type = "deviance")^2)
df <- df.residual(poisson_model)

#Pearson chi-squared statistic
pearson_chi_squared <- residual_deviance / df

# ratio of residual deviance to degrees of freedom
ratio_deviance_df <- residual_deviance / df

#pearson_chi_squared > 1 , no overdispersion detected, so the poisson model is a good fit 

## BMB: you mean < 1 ?

# Summary of the glm model

summary(poisson_model)

#####################
# Fit the Poisson model using bbmle formula interface
poisson_model_formula <- mle2(
  shells ~ dpois(lambda = exp(b1 + b2 * (year == 2005) + b3 * (year == 2006) + b4 * prev) * Area),
  start = list(b1 = 0, b2 = 0, b3 = 0, b4 = 0),
  data = g_data
)

# Summary of the bbmle model
summary(poisson_model_formula)

#b1 (Intercept): The estimated value of b1 is approximately -3.196.
#b2: The estimated value of b2 is approximately -0.645.
#b3: The estimated value of b3 is approximately -0.428.
#b4: The estimated value of b4 is approximately 0.021.

#The -2 log likelihood value is a measure of how well the Poisson regression model
#fits the data. The -2 log L is approximately 76.33738.
# the model fits the data relatively well, as indicated by a reasonable -2 log likelihood value.

#The intercept b1 is highly significant (***), with a very low p-value (< 2.2e-16)
#It is essential for explaining the variability in the number of shells.

#The coefficients for "Year 2005" (b2), "Year 2006" (b3), and "Prev - Seroprevalence" (b4) have different levels of significance. 
#"Year 2005" is marginally significant (.), "Year 2006" is not significant, and "Prev - Seroprevalence" is highly significant (***).

######################################################################
# Custom Negative Log-Likelihood Function for Poisson GLM
LL <- function(b1, b2, b3, b4, data=g_data){
  
  lambda_est <- exp(
    b1 + b2*data$year_2005 + b3*data$year_2006 + b4*data$prev)*data$Area
  
  neg_ll <- -sum(dpois(data$shells, lambda_est, log = TRUE))
  
  return(neg_ll)
}

#Fitting the model using the custom liklehood function


pois <- mle2(
  minuslogl = LL, start = list(b1=0,b2=0,b3=0,b4=0)
)
pois

# Summary of the model
summary(pois)

#################################################
#Question 3 
data(endometrial)
summary(endometrial)
str(endometrial)

model_glm <- glm(HG ~ NV + PI + EH, data = endometrial, family = binomial(link = "logit"))
model_bayesglm <- bayesglm(HG ~ NV + PI + EH, data = endometrial, family = binomial(link = "logit"))
model_brglmFit <- glm(HG ~ NV + PI + EH, data = endometrial, family = binomial(link = "logit"), method = "brglmFit")

summary(model_glm)
summary(model_bayesglm)
summary(model_brglmFit)

reduced_model_NV <- glm(HG ~ PI + EH, family = binomial(link = "logit"), data = endometrial)
reduced_model_EH <- glm(HG ~ PI + NV, family = binomial(link = "logit"), data = endometrial)
reduced_model_PI <- glm(HG ~ NV + EH, family = binomial(link = "logit"), data = endometrial)

lrt_result_NV <- lrtest(model_glm, reduced_model_NV)
lrt_result_EH <- lrtest(model_glm, reduced_model_EH)
lrt_result_PI <- lrtest(model_glm, reduced_model_PI)

print(lrt_result_NV)
print(lrt_result_EH)
print(lrt_result_PI)

#The summary statistics of the three models are presented
#showing variations in parameter estimates, standard errors, and AIC values.

#Likelihood ratio tests are performed for each predictor variable (NV, EH, and PI) by comparing full models to reduced models.

#For NV and EH, the likelihood ratio tests show significant results with small p-values
#indicating that these variables are highly significant predictors.
#For PI, the test yields a p-value greater than 0.05,
#suggesting that it may not be a significant predictor in the model.

  
#The analysis suggests that the choice of estimation method can influence parameter estimates and model fit. 

  
#Both NV and EH appear to be significant predictors of the HG variable,
#as indicated by highly significant likelihood ratio tests.
#The PI variable does not seem to be a significant predictor based on the likelihood ratio
#test.
#the choice of estimation method and the significance of
#predictor variables may impact the interpretation of logistic regression results.
#The likelihood ratio tests help identify the variables that contribute most to explaining 
#the outcome variable in the model.




## BMB: 9/10
