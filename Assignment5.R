#ASSIGNMENT 5 
#IKRAM JMOUHI 400550954 


library(mgcv)
library(lme4)
library(broom)
library(broom.mixed)
library(dotwhisker)
library(ggeffects)

data("Contraception", package = "mlmRev")


Contraception <- transform(Contraception,
                           use_n = as.numeric(use) - 1,
                           age_sc = drop(scale(age)))

#glmer model
glmer_model <- glmer(use_n ~ age_sc * urban + (1 | district),
                     data = Contraception,
                     family = binomial)

#gam model
gam_model <- gam(use_n ~ s(age_sc) + urban + s(age_sc, by = urban) + s(district, bs = "re"),
                 data = Contraception,
                 family = binomial)

# Comparing the estimates and standard errors for fixed-effects coefficients
glmer_summary <- tidy(glmer_model, effects = "fixed")
gam_summary <- tidy(gam_model, effects = "fixed")

#summaries
glmer_summary
gam_summary  

#GLMER :
#The standard error is 0.0852, and the p-value is very small, 
#the intercept is significantly different from zero.
#The estimated coefficient for age is 0.101, with a standard error
#of 0.0574. The positive estimate suggests that, holding other
#variables constant, there is a positive association between age 
#and the log-odds of the response variable being 1.
#However, the p-value (0.0775) is greater than the typical 
#significance level of 0.05, so the evidence for the effect of age
#is not strong.
# The estimated coefficient for the urban/rural variable 
#(coded as 1 for urban) is 0.654, with a standard error of 0.116.
#The positive estimate suggests that, compared to rural areas 
#(coded as 0), urban areas are associated with higher log-odds of 
#the response variable being 1. The p-value is very small, indicating
#statistical significance.
#The estimated coefficient for the interaction between age and
#urban/rural is -0.0711, with a standard error of 0.108. 
#The p-value (0.510) is greater than 0.05, suggesting that there 
#is not enough evidence to reject the null hypothesis of no
#interaction effect.
#age and the interaction between age and urban/rural do not appear to have statistically significant effects on the log-odds of the response variable being 1. Urban areas, however, are associated with a statistically significant
#increase in the log-odds compared 
#to rural areas.



#GAM: 
#The smooth term for age is statistically significant (p-value < 0.05),
#indicating a nonlinear relationship between age and the log-odds 
#of the response variable being 1. The effective degrees of freedom 
#suggest a complex relationship that is not purely linear.
#For the smooth terms involving the interaction between age and 
#urban/rural, neither of them appears to be statistically significant. 
#The p-values are large, suggesting that the interaction terms do not
#contribute significantly to the model.
#The smooth term for district is statistically significant, 
#indicating that there is variability in the log-odds of
#the response variable being 1 across different districts.
#The effective degrees of freedom suggest a complex and possibly
#spatially varying effect of district on the response.
#the gam model suggests a nonlinear relationship between age and
#the log-odds of the response variable being 1. However, 
#the interaction terms and the district-specific smooth
#term do not appear to be statistically significant contributors
#to the model.





#The estimates and standard errors of age and urbanY in the glmer 
#model are higher compared to the GAM. However, for the interaction 
#term (age_sc:urbanY), the estimate is higher in the GAM model,
#while the standard error is higher in the glmer model.
.
#################################################################
#model with a fixed quadratic function of age
gam_quad_model <- gam(use_n ~ poly(age_sc, 2) + urban + s(district, bs = "re"),
                      data = Contraception,
                      family = binomial)

#model with an effect of age modeled as a thin-plate spline
gam_spline_model <- gam(use_n ~ s(age_sc, bs = "tp") + urban + s(district, bs = "re"),
                        data = Contraception,
                        family = binomial)

#predicted values for the model with a fixed quadratic function of age
pred_quad <- as.data.frame(predict(gam_quad_model, type = "response", se.fit = TRUE))

# Combining predicted values with the original data
pred_quad <- cbind(Contraception, pred_quad)

#predictions for the model with a fixed quadratic function of age
p1 <- ggplot(pred_quad, aes(x = age_sc, y = fit, color = urban)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit), alpha = 0.2) +
  ggtitle("Model with Fixed Quadratic Function of Age")

#predicted values for the model with a thin-plate spline for age
pred_spline <- as.data.frame(predict(gam_spline_model, type = "response", se.fit = TRUE))

#predicted values with the original data
pred_spline <- cbind(Contraception, pred_spline)

#predictions for the model with a thin-plate spline for age
p2 <- ggplot(pred_spline, aes(x = age_sc, y = fit, color = urban)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit), alpha = 0.2) +
  ggtitle("Model with Thin-Plate Spline for Age")

#plots side by side
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)

#The lines in p2 graph seem wider at the ends than
#those in the p1 model. If we focus on the chance of using
#contraception, it looks like the Urban Y line is higher around 
#the average age, compared to the Urban N line.


######################################################################
# Creating a quadratic-age term separately for urban and rural settings
Contraception$age_sc_squared_urban <- ifelse(Contraception$urban == "Y", poly(Contraception$age_sc, 2), 0)
Contraception$age_sc_squared_rural <- ifelse(Contraception$urban == "N", poly(Contraception$age_sc, 2), 0)

#model with a quadratic-age urban/rural interaction
gam_interaction_model <- gam(use_n ~ s(age_sc, by = urban) + age_sc_squared_urban + age_sc_squared_rural + s(district, bs = "re"),
                             data = Contraception,
                             family = binomial)


#model with separate thin-plate splines for age_sc for urban and rural settings
gam_splines_model <- gam(use_n ~ s(age_sc, bs = "tp", by = urban) + s(district, bs = "re"),
                         data = Contraception,
                         family = binomial)

#predicted values for the model with a quadratic-age urban/rural interaction
pred_interaction <- as.data.frame(predict(gam_interaction_model, type = "response", se.fit = TRUE))

# Combining predicted values with the original data
pred_interaction <- cbind(Contraception, pred_interaction)

#predictions for the model with a quadratic-age urban/rural interaction
p3 <- ggplot(pred_interaction, aes(x = age_sc, y = fit, color = urban)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit), alpha = 0.2) +
  ggtitle("Quadratic-Age Urban/Rural Interaction")


print(p3)

#predicted values for the model with separate thin-plate splines for age_sc for urban and rural 
pred_splines <- as.data.frame(predict(gam_splines_model, type = "response", se.fit = TRUE))

# Combining predicted values with the original data
pred_splines <- cbind(Contraception, pred_splines)

#predictions for the model with separate thin-plate splines for age_sc for urban and rural 
p4 <- ggplot(pred_splines, aes(x = age_sc, y = fit, color = urban)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit), alpha = 0.2) +
  ggtitle("Separate Thin-Plate Splines for Age_sc by Urban/Rural")

#plots side by side
print(p4)

#The p4 has wider lines at the ends compared to p3 model. 
#There's more mixing between Urban Y and Urban N in the p3 model. 
#the chance of using contraception is always higher in Urban Y compared to Urban N, especially around the average age, in both models.


########################################################################################

# glmer() model with a fixed quadratic-age by urban/rural interaction and random effect
quad_age_glmer <- glmer(use ~ urban + poly(age, 2):urban + (poly(age, 2) | district),
                        data = Contraception, binomial)

# gam() model with different population-level smooths for urban vs rural and age-smooths for each district
urban_age_district_gam <- gam(use ~ urban + s(age, bs = "tp", by = urban) + s(age, district, bs = 'fs'),
                              data = Contraception, family = "binomial")

#predictions for the glmer() model (quad_age_glmer)
pred_glm <- predict(quad_age_glmer, newdata = newdata, type = "response", re.form = NULL)

# Combining predictions with the newdata
pred_glm <- cbind(newdata, pred_glm)

#predictions for the gam() model (urban_age_district_gam)
pred_gam <- predict(urban_age_district_gam, newdata = newdata, type = "response", se.fit = TRUE)

#predictions with the newdata
pred_gam <- cbind(newdata, pred_gam$fit, pred_gam$se.fit)

#predictions for both models
p5 <- ggplot(pred_glm, aes(x = age, y = fit, color = urban)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit), alpha = 0.2) +
  ggtitle("glmer() Model Predictions")

p6 <- ggplot(pred_gam, aes(x = age, y = fit, color = urban)) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit), alpha = 0.2) +
  ggtitle("gam() Model Predictions")

#plots side by side
grid.arrange(p5, p6, ncol = 2)