## ---
## title: "STATS - 720 Assignment-1"
## author: "Ikram Jmouhi (Student number: 400550954)"

# packages
library(dotwhisker)
library(effects)
library(stats)

# Load the dataset
data("USArrests")

# View the first few rows of the dataset
head(USArrests)

#I will perform an linear regression to predict the value of Murder rate
#Given the fact that we have 50 observations, I wouldn't include the 3 predictors
#"Rape", "Assault"and "UrbanPop" simultaneously. 
#Harrell's rule of thumb recommends having at least 10-20 events (states with murder data) per predictor 
#variable to ensure model stability and reliability.

#performing a correlation test for predictors 
predictor_data <- USArrests[, c("Rape", "Assault", "UrbanPop")]
correlation_matrix <- cor(predictor_data)
correlation_matrix

#The two predictors Rape and Assault have a high correlation coefficient 
#this suggests that the two predictors are highly correlated and bring the same information
#to the model. 
#to avoid multicollinearity problems, I will only keep one of the two predictors. 
#UrbanPop and assault have a low correlation coefficient 
##My predictors will be Assault and urbanPop

## BMB: OK. Collinearity in general is not a problem, but if you need to limit the
## number of parameters, eliminating highly correlated predictors is one sensible strategy

########################################################
#Murder and assaults represent the number of murder and assaults per 100,000 people in each state
#The variable urbanPop represents the percentage of people living in urban areas in each state. 
#I consider a change of 1 murder per 100,000 people as a small change,same for assault and urbanPop 
#########################################################


#linear regression model with Assault and UrbanPop
model <- lm(Murder ~ Assault + UrbanPop, data = USArrests)

#summary of the model
summary(model)
########################################################
#residual vs fitted plot 
plot(model, which = 1)
# linearity seems to hold reasonably well, the red line is close to the dashed line.
#We can also note the homosedasticity: as we move to the right on the x-axis, the spread of the individuals is equal.
#Georgia, Arizona and Delaware may be outliers. 
#######################################################

## BMB: note that plot.lm() **always** marks the three most extreme residuals, by default
## (so this is not based on a statistical judgement)

## BMB: you should generally assess normality *last*

#Q_Q plot to assess the normality of residuals:
plot(model, which = 2)
#Most of the point follow the same line which suggest a normal distribution 
#we can also detect Georgia, arizona and delaware as outliers in this plot too. 

## BMB: see comment above (these are the same three points)

#######################################################
#Residual vs leverage plot (cook's distance)
plot(model, which = 5)
#There are no points outside of cook's distance.

######################################################

## BMB: what about the  scale-location plot, which is more
##  reliable for detecting heteroscedasticity?

#coefficient plot using dotwhisker
dwplot(model, scales = "fixed", center = TRUE, scale = TRUE)

## BMB: I don't think these arguments do anything ???
## this looks the same as your plot ...
dwplot(model, scales = "junk", center = NA, scale = NA)

#UrbanPop as a larger magnitude than Assault 
#I will scale and center my predictors 

predictors <- USArrests[, c("Assault", "UrbanPop")]
scaled_and_centered_predictors <- scale(predictors)
USArrests2 <- data.frame(Murder = USArrests$Murder, scaled_and_centered_predictors)

model2 <- lm(Murder ~ Assault + UrbanPop, data = USArrests2)
summary(model2)

##########################################################

#effect model
effects_model <- allEffects(model2)
plot(effects_model)
#the assault variable is proportionnally poqitive with murder, when the values of assault increase
#murder increases too ,the urbanpop has an oppositive effect on our predicted value 
###########################################################################################

#Question 2 

## BMB: best not to use built-in names ('data', 'contrasts') for variable names

data <- data.frame(
  Period = as.factor(c("Before", "After", "Before", "After")),
  Treatment = as.factor(c("Control", "Control", "Impact", "Impact"))
)

#contrasts
contrasts <- matrix(
  c(
      0.5, 0.5, 0.5, 0.5,   # Average of Control and Impact during Before
      ## BMB: no, this is the (2x) the average of *all* levels (rep(0.25, 4) would be the average)
      1, -1, 0, 0,           # Difference between Control and Impact before treatment
      0.5, 0.5, -0.5, -0.5,  # Difference between (average of Control and Impact) between After and Before
      -1, 1, 1, -1           # Difference of (C-I) difference between Before and After periods
  ),
  nrow = 4,               
  byrow = TRUE            
)
#INVERSE 
c_m <- MASS::fractions(solve(contrasts))
print(c_m)

## BMB: note that your 'contrasts' is the *inverse* contrast matrix, c_m is the contrast matrix

# the minimal model matrix (additive model)
minimal_model_matrix<-model.matrix(~ Period + Treatment, data)
minimal_model_matrix

#############################################################
#minimal model.matrix for period*treatment 
model.matrix(~ Period * Treatment, data)

#############################################################
#model matrix for 0+Period:Treatment
model.matrix(~0+Period:Treatment,data)

## BMB: what do you conclude?

################################################################################
#Question 3 

## BMB: nice to allow the options. You didn't allow for varying *degrees* of misspecification though ...
#the simulation function with a chosen violation
sim_fun <- function(n = 100, slope = 1, sd = 1, intercept = 0, violation = "linearity") {
  x <- runif(n)
  if (violation == "linearity") {
    y <- rnorm(n, intercept + slope * x^2, sd = sd)  # Quadratic relationship
  } else if (violation == "homoscedasticity") {
    ## BMB: this would be bad if x<0
    y <- rnorm(n, intercept + slope * x, sd = sd * x)  # Heteroscedasticity
  } else if (violation == "normality") {
    y <- rt(n, df = 3)  # t-distributed errors
  } else {
    stop("Invalid violation specified.")
  }
  data.frame(x, y)
}

#run simulations
run_simulations <- function(violation = "linearity", num_simulations = 1000) {
  true_slope <- 1  # True slope value

  ## BMB: thank you for pre-allocating
  results <- data.frame(
    bias = numeric(num_simulations),
    rmse = numeric(num_simulations),
    power = numeric(num_simulations),
    coverage = numeric(num_simulations)
  )
  
  for (i in 1:num_simulations) {
    sim_data <- sim_fun(violation = violation)

    model <- lm(y ~ x, data = sim_data)
    
    # Checking if the model has valid results
    if (is.na(summary(model)$coefficients["x", "Estimate"])) {
      # Invalid model results
      results[i, ] <- c(NA, NA, NA, NA)
    } else {
      #coefficients
      coef_summary <- coef(summary(model))
  
      bias <- coef_summary["x", "Estimate"] - true_slope
    
      rmse <- sqrt(mean((coef_summary["x", "Estimate"] - true_slope)^2))
      
      #power (p < alpha)
      alpha <- 0.05
      p_value <- coef_summary["x", "Pr(>|t|)"]
      power <- mean(p_value < alpha)
      
      #coverage
      conf_interval <- confint(model)
      coverage <- (true_slope >= conf_interval["x", 1] & true_slope <= conf_interval["x", 2])
      
      results[i, ] <- c(bias, rmse, power, coverage)
    }
  }
  
  return(results)
}

# Run simulations for different violations
set.seed(123)
linearity <- run_simulations(violation = "linearity")
homoscedasticity <- run_simulations(violation = "homoscedasticity")
normality <- run_simulations(violation = "normality")

# Summary for each violation
 summary(linearity, na.rm = TRUE)
 #the mean bias and RMSE are small, the estimated slope is very close to the true slope under the linearity violation
 #the model doesn't always detect the linearity violation (power is less than 1)
##the confidence intervals don't always include the true value (coverage is not 1).

## BMB: we want the coverage to be 0.95 (i.e. 1-alpha), *not* 1.00.
## you should compute the *means* of these results to get the bias, rmse, etc... the individual values
##  are *not* power etc.

################################################################ 
summary(homoscedasticity, na.rm = TRUE)

#the model performs well in terms of bias, RMSE, and power. 
#It can often detect the violation (high power)
#the estimated slopes are generally close to the true slope

##############################################################
summary(normality, na.rm = TRUE)
#the model performs less well compared to the other violations. 
#The estimated slopes can be biased and far from the true slope (high RMSE)
#the model is not effective at detecting the violation (low power)

## BMB: interesting, but it is hard to compare magnitudes of violation
##  across different types of violation (i.e., is df = 3 comparable to a quadratic
##  rather than a linear function of x over the range (0,1) ?

## mark: 8.5/10
