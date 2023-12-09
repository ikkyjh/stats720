#IKRAM JMOUHI 400550954
#STATS720 ASSIGNMENT 4


library(faraway)
library(ggplot2)
library(lme4)
library(ggeffects)

################################################################################
#1
data(nepali)
##Exploring the dataset 
str(nepali)
summary(nepali)
#removing missing values 
nepali <- na.omit(nepali)
################################################################################
#Scatter plot of weight gain over time
plot(nepali$age, nepali$wt)
################################################################################
# Converting 'sex' to a factor with levels "male" and "female"
nepali$sex <- factor(ifelse(nepali$sex == 1, "male", "female"))
levels(nepali$sex)
################################################################################
# Converting id and lit variables to a factor
nepali$id <- as.factor(nepali$id)
nepali$lit <- as.factor(nepali$lit)

#Plotting sensible plt 
ggplot(nepali, aes(x = age, y = wt, color = as.factor(id))) +
  geom_point(alpha = 0.5, size = 3) +
  labs(title = "Age vs. Weight for Each Child",
       x = "Age",
       y = "Weight") +
  theme_minimal() +
  guides(color = FALSE) 

##plotted age vs weight for each child, can't put a different color for each child 
#so tried to make it as distinguishable as possible... 
#################################################################################
#The random effect is specified as (1 + age | id)
#indicating that we allow the intercept and age to vary randomly across different 
#levels of the grouping variable id
mixed_model <- lmer(wt ~ mage + died+ alive + age + sex + lit + (1+age|id), data = nepali)
summary(mixed_model)
#Random Slope for age within id (age):
#Variance: 0.001295
#Standard Deviation: 0.03598
#Correlation with Intercept: -0.58
#There's variability in the effect of age across different levels of id.
#Correlation of Fixed Effects:
#This matrix shows the correlations between the fixed effects.
#For example, the correlation between mage and the intercept is -0.910.

#The fixed effects provide estimates and significance levels for each predictor.
#The random effects show variability in intercepts and slopes across different levels of id.
#The correlation matrix provides insights into relationships between fixed effects.


plot(fitted(mixed_model), residuals(mixed_model))
abline(h = 0, col = "red", lty = 2)

##QQ plot 
qqnorm(residuals(mixed_model))
qqline(residuals(mixed_model))


#Random effects 
ranef(mixed_model)

#The plot of Fitted Values versus Residuals indicates a non-linear pattern,
#especially noticeable for small fitted values, suggesting potential model limitations
#. Additionally, there is evidence of heterogeneity in residual variance. 
#The QQ plot further illustrates that the residuals follow a light-tailed distribution
#########################################################################################
plot(mixed_model)
# the horizontal line along zero suggests that the variability of the residuals is consistent across different levels of the fitted values.
#this meets the assumption of homoscedasticity.
#there is no clear patterns which is good. 
#######################################################################################
#Plotting the data along with model predictions and confidence intervals
effect <- ggeffect(mixed_model, terms = c("mage", "age", "sex","lit"))
plot(effect)

#While the initial study investigated the impact of vitamin A on children's 
#early growth, my analysis focused on understanding factors influencing individual
#weight gain over time. Unlike the original study, 
#I did not differentiate between control and treatment groups,
#nor aimed to compare the effects of specific treatments across distinct groups.


##########################################################################################
#2
library(mlmRev)
data("Contraception", package = "mlmRev")
summary(Contraception)
summary(Contraception$age)
#######################################################################################
# Encoding categorical variables
Contraception$urban <- as.factor(Contraception$urban)

# removing rows with missing values
Contraception <- na.omit(Contraception)

# Verifying the changes
str(Contraception)
#fitting the data into logistic

# 'livch', 'age', and 'urban' are predictor variables
glm_model <- glmer(use ~ urban + age + livch +(urban|district), Contraception, binomial)

plot(fitted(glm_model), residuals(glm_model))
abline(h = 0, col = "red", lty = 2)

##QQ plot 
qqnorm(residuals(glm_model))
qqline(residuals(glm_model))

##predictions 
predictions_glm <- ggpredict(glm_model, terms = c("age[all]","urban"))
print(predictions_glm, n = Inf)
plot(predictions_glm)

##pooled analysis 
model_glm_2 <- glm(use ~ urban + scale(age) + livch , data = Contraception , family = "binomial")

##With quasi likelihood 
model_quasi <- MASS:::glmmPQL(use ~ urban+scale(age)+livch,
                          random = ~1|district,
                          data = Contraception,
                          family=binomial)
##Laplace 
model_laplace <- glmer(use ~ urban+scale(age)+livch+(1|district), Contraception, binomial)

#Gauss Hermite Quadrature

model_gauss<- update(model_laplace, nAGQ = 20)
                     
#The original study utilized a multilevel model to analyze factors affecting 
#immunization uptake, revealing residual household variation. 
#Our approach mirrors this, considering age, urban living, and district-specific 
#effects to address socioeconomic and geographical nuances.   


##Question 3 : 
library(brms)

# Defining the brm model
brm_model <- brm(
  wt ~ mage + died + alive + age + sex + lit + (1 + age | id),
  data = nepali,
  family = gaussian(),
  cores = 4  
)

# summary
summary(brm_model)

################################################################################
library(rstanarm)

# Defining the stan_glmer model
stan_glmer_model <- stan_glmer(
  wt ~ mage + died + alive + age + sex + lit + (1 + age | id),
  data = nepali,
  family = gaussian(),
  chains = 4,  
  cores = 4    
)

# summary
summary(stan_glmer_model)

#################################################################################
#Question 4 
sim_data_generator <- function(param1, param2, sample_size, num_groups){
  

  x_values <- rnorm(sample_size)
  
  # Creating a grouping variable with num_groups levels
  group_var <- factor(rep(1:num_groups, each = sample_size/num_groups))
  
  # Creating a dataframe
  dataset <- data.frame(x = x_values, group = group_var)
  
  # Simulating data
  sim_data <- simulate(
    ~1+x+(1|group), nsim = 1, newdata = dataset, family = poisson,
    newparams = list(beta = param1, theta = param2)
  )
  
  # Adding the simulated data to the dataframe
  dataset$y_values <- sim_data$sim_1
  
  return(dataset)
}

fitting_model <- function(data, num_AGQ){
  
  tryCatch({
    # Applying if conditions for num_AGQ values
    if (num_AGQ == -2) {
      
      # Fitting with a glm
      model <- glm(y_values ~ 1 + x, data = data, family = 'poisson')
      coefficients <- coef(model)
      confidence_interval <- confint(model)
      
    } else if (num_AGQ == -1){
      
      # Fitting with a glmmPQL
      model <- glmmPQL(y_values~1+x, random = ~1|group, data = data, family = poisson)
      coefficients <- fixef(model)
      confidence_interval <- intervals(model, which = 'fixed')$fixed[,-2]
      colnames(confidence_interval) <- c('2.5 %','97.5 %')
      
    } else if (num_AGQ >= 1){
      
      # Fitting with glmer using Laplace (num_AGQ=1) or AGHQ (num_AGQ>1)
      model <- glmer(y_values~1 + x + (1|group), data = data, family = poisson, nAGQ = num_AGQ)
      coefficients <- fixef(model)
      confidence_interval <- confint(model, parm="beta_", method = 'Wald')
      
    } else {
      print('Invalid number of num_AGQ parameter')
    }
    
    return(list(coefficients, confidence_interval))
    
  }, error=function(e){return(list(NA,NA))})
  
}

simulate_and_fit <- function(param1, param2, sample_size, num_groups, num_AGQ) {
  
  # Generating data
  dataset <- sim_data_generator(param1, param2, sample_size, num_groups)
  
  # Fitting model
  fitting_results <- fitting_model(dataset, num_AGQ)
  
  return(fitting_results)
}

check_na <- function(column) {
  any(sapply(column, function(x) any(is.na(x))))
}

remove_na_columns <- function(result_matrix){
  
  columns_with_na <- apply(result_matrix, 2, check_na)
  final_result_matrix <- result_matrix[,!columns_with_na]
  
  return(final_result_matrix)
}

evaluate_parameters <- function(result_matrix, param_values){
  
  # Creating vectors of length of a row in the result_matrix (omitting NAs)
  result_length <- sum(!is.na(result_matrix[1,]))
  
  # Removing NA columns in the result_matrix
  result_matrix <- remove_na_columns(result_matrix)
  
  intercept_estimates <- c(NA,result_length)
  intercept_up <- c(NA,result_length)
  intercept_lw <- c(NA,result_length)
  slope_estimates <- c(NA,result_length)
  slope_up <- c(NA,result_length)
  slope_lw <- c(NA,result_length)
  
  # Looping through result matrix columns
  for (i in 1:result_length) {
    
    intercept_estimates[i] <- result_matrix[1,][[i]][[1]]
    slope_estimates[i] <- result_matrix[1,][[i]][[2]]
    intercept_up[i] <- result_matrix[2,][[i]][1,2]
    intercept_lw[i] <- result_matrix[2,][[i]][1,1]
    slope_up[i] <- result_matrix[2,][[i]][2,2]
    slope_lw[i] <- result_matrix[2,][[i]][2,1]
  }
  
  # Calculating bias
  slope_bias <- mean(slope_estimates) - param_values[2]
  
  # Calculating variance
  slope_variance <- var(slope_estimates)
  
  # Calculating the scaled RMSE
  slope_rmse <- sqrt(mean((slope_estimates/param_values[2]-1)^2))
  
  # Calculating the coverage
  slope_coverage <- mean((param_values[2]>=slope_lw) & (param_values[2]<=slope_up))
  
  cat('bias: ', slope_bias, "\n")
  cat('variance: ', slope_variance, "\n")
  cat('scaled RMSE: ', slope_rmse, "\n")
  cat('coverage: ', slope_coverage, "\n")
  
}

# Parameters
param_set1 <- c(-2, 0.5)
param_set2 <- c(2, 0.5)
theta_value <- 1
sample_size_value <- 500
num_groups_value <- 100

# Analysis for parameter set 1
set.seed(3)
glm_results_param1 <- replicate(100, simulate_and_fit(param_set1, theta_value, sample_size_value, num_groups_value, -2))

set.seed(3)
glmmPQL_results_param1 <- replicate(100, simulate_and_fit(param_set1, theta_value, sample_size_value, num_groups_value, -1))

set.seed(3)
glmer_results_param1 <- replicate(100, simulate_and_fit(param_set1, theta_value, sample_size_value, num_groups_value, 3))

# Displaying results for parameter set 1
cat("ANALYSIS FOR PARAMETER SET 1\n\n")

cat("GLM Results:\n")
evaluate_parameters(glm_results_param1, param_set1)

cat("GLMM PQL Results:\n")
evaluate_parameters(glmmPQL_results_param1, param_set1)

cat("GLM with nAGQ = 3 Results:\n")
evaluate_parameters(glmer_results_param1, param_set1)


# Analysis for parameter set 2
set.seed(10)
glm_results_param2 <- replicate(100, simulate_and_fit(param_set2, theta_value, sample_size_value, num_groups_value, -2))

set.seed(10)
glmmPQL_results_param2 <- replicate(100, simulate_and_fit(param_set2, theta_value, sample_size_value, num_groups_value, -1))

set.seed(10)
glmer_results_param2 <- replicate(100, simulate_and_fit(param_set2, theta_value, sample_size_value, num_groups_value, 3))

# Displaying results for parameter set 2
cat("\nANALYSIS FOR PARAMETER SET 2\n\n")

cat("GLM Results:\n")
evaluate_parameters(glm_results_param2, param_set2)

cat("GLMM PQL Results:\n")
evaluate_parameters(glmmPQL_results_param2, param_set2)

cat("GLM with nAGQ = 3 Results:\n")
evaluate_parameters(glmer_results_param2, param_set2)
