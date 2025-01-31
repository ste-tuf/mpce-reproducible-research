# Script after Exercice 1

# To put in file R/descriptive --------------------------------------------

#' Compute summary statistics for a vector
#'
#' @param variable a vector
#
compute_descriptive_stats <- function(variable) {
  
  statistics <- NA
  
  # Steps for
  if (is.numeric(variable)) {
    statistics <- compute_numeric(variable)
  }
  
  if (is.factor(variable) || is.logical(variable)) {
    statistics <- compute_table(variable)
  }
  
  statistics
}

compute_table <- function(variable) {
  # Return frequency table as a dataframe
  table(variable, useNA = "always")
}

compute_numeric <- function(variable) {
  mean_value <- mean(variable, na.rm = TRUE)
  sd_value <- sd(variable, na.rm = TRUE)
  quantiles <- quantile(variable, na.rm = TRUE)
  
  # Return statistics
  list(
    "mean" = mean_value,
    "sd" = sd_value,
    "quantiles" = quantiles
  )
}


compute_descriptive_graph <- function(variable) {
  # Steps for
  if (is.numeric(variable)) {
    p <- ggplot2::ggplot(mapping = aes(x=variable)) + 
      ggplot2::geom_histogram()
  }
  
  if (is.factor(variable) || is.logical(variable)) {
    p <- ggplot2::ggplot(mapping = aes(x=variable)) + 
      ggplot2::geom_bar()
  }
  
  p
}

# In starting_script.R ----------------------------------------------------


library(tidyverse)

nh2007 <- load("data/nh2007.Rdata")

# Data management ---------------------------------------------------------
nh2007$id<-factor(nh2007$id)
nh2007$gender<-factor(nh2007$gender)

nh2007$age_screening<-as.numeric(nh2007$age_screening)

nh2007$education<-factor(nh2007$education)
nh2007$education_child<-factor(nh2007$education_child)
nh2007$marital_status<-factor(nh2007$marital_status)

nh2007$id<-factor(nh2007$id)
nh2007$gender<-factor(nh2007$gender)
nh2007$age_screening<-factor(nh2007$age_screening)
nh2007$education<-factor(nh2007$education)
nh2007$education_child<-factor(nh2007$education_child)
nh2007$marital_status<-factor(nh2007$marital_status)

nh2007$asthma<- nh2007$asthma%in%1
nh2007$heart_failure<-nh2007$heart_failure%in%1
nh2007$coronary_heart_disease<-nh2007$coronary_heart_disease%in%1
nh2007$heart_attack<-nh2007$heart_attack%in%1
nh2007$stroke<-nh2007$stroke%in%1
nh2007$chronic_bronchitis<-nh2007$chronic_bronchitis%in%1
nh2007$cancer<-nh2007$cancer%in%1

summary(nh2007)
head(nh2007)


# Summary statistics ------------------------------------------------------

# Numbers
compute_descriptive_stats(nh2007$gender)
compute_descriptive_stats(nh2007$education)
compute_descriptive_stats(nh2007$education_child)
compute_descriptive_stats(nh2007$asthma)
compute_descriptive_stats(nh2007$heart_failure)
compute_descriptive_stats(nh2007$coronary_heart_disease)
compute_descriptive_stats(nh2007$creatinine)
compute_descriptive_stats(nh2007$lead)
compute_descriptive_stats(nh2007$barium)
compute_descriptive_stats(nh2007$cadmium)


# Graph
compute_descriptive_graph(nh2007$creatinine)
compute_descriptive_graph(nh2007$lead)
compute_descriptive_graph(nh2007$barium)
compute_descriptive_graph(nh2007$cadmium)


# Models ------------------------------------------------------------------
# Creatinine
model.1a <- glm(asthma ~ barium + age_screening + gender,data=nh2007)
model.1.b <- glm(heart_failure~barium+age_screening+gender,data=nh2007)
model.1.c <- glm(coronary_heart_disease~ barium +age_screening +gender,data=nh2007)
model.1.d <- glm(heart_attack~barium + age_screening + gender,data=nh2007)
model.1.e <- glm(asthma~barium + age_screening + gender,data=nh2007)

# Models ------------------------------------------------------------------


# Lead
lead2a <- glm(asthma ~ lead + age_screening + gender,data=nh2007)
lead2b <- glm(heart_failure~lead+age_screening+gender,data=nh2007)
lead2c <- glm(coronary_heart_disease~ lead +age_screening +gender,data=nh2007)
lead2d <- glm(heart_attack~lead + age_screening + gender,data=nh2007)
lead2e <- glm(asthma~lead + age_screening + gender,data=nh2007)

# Barium
Model3a <- glm(asthma ~ barium+ age_screening + gender,data=nh2007)
Model3b <- glm(heart_failure~barium+age_screening+gender,data=nh2007)
Model3c <- glm(coronary_heart_disease~ barium +age_screening +gender,data=nh2007)
Model3d <- glm(heart_attack~barium + age_screening + gender,data=nh2007)
Model3e <- glm(asthma~barium + age_screening + gender,data=nh2007)


# Cadmium
Model_4_a <- glm(asthma ~ cadmium+ age_screening + gender,data=nh2007)
Model_4_b <- glm(heart_failure~cadmium+age_screening+gender,data=nh2007)
Model_4_c <- glm(coronary_heart_disease~ cadmium +age_screening +gender,data=nh2007)
Model_4_d <- glm(heart_attack~cadmium + age_screening + gender,data=nh2007)
Model_4_e <- glm(asthma~cadmium + age_screening + gender,data=nh2007)

# Results 
summary(model.1a)
# Call:
#   glm(formula = asthma ~ barium + age_screening + gender, data = nh2007)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.1517608  0.0172807   8.782  < 2e-16 ***
#   barium         0.0031189  0.0019226   1.622 0.104897    
# age_screening -0.0014257  0.0003947  -3.612 0.000312 ***
#   gender2        0.0488173  0.0151511   3.222 0.001293 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 0.116699)
# 
# Null deviance: 240.0  on 2033  degrees of freedom
# Residual deviance: 236.9  on 2030  degrees of freedom
# AIC: 1408.9
# 
# Number of Fisher Scoring iterations: 2

summary(lead2a)

# Call:
#   glm(formula = asthma ~ lead + age_screening + gender, data = nh2007)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.161264   0.017175   9.390  < 2e-16 ***
#   lead          -0.001971   0.005037  -0.391 0.695597    
# age_screening -0.001451   0.000395  -3.673 0.000246 ***
#   gender2        0.048409   0.015263   3.172 0.001538 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 0.1168415)
# 
# Null deviance: 240.00  on 2033  degrees of freedom
# Residual deviance: 237.19  on 2030  degrees of freedom
# AIC: 1411.4
# 
# Number of Fisher Scoring iterations: 2

summary(Model3a)

# Call:
#   glm(formula = asthma ~ barium + age_screening + gender, data = nh2007)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.1517608  0.0172807   8.782  < 2e-16 ***
#   barium         0.0031189  0.0019226   1.622 0.104897    
# age_screening -0.0014257  0.0003947  -3.612 0.000312 ***
#   gender2        0.0488173  0.0151511   3.222 0.001293 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 0.116699)
# 
# Null deviance: 240.0  on 2033  degrees of freedom
# Residual deviance: 236.9  on 2030  degrees of freedom
# AIC: 1408.9
# 
# Number of Fisher Scoring iterations: 2

summary(Model_4_a)
# Call:
#   glm(formula = asthma ~ cadmium + age_screening + gender, data = nh2007)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.1578261  0.0169630   9.304  < 2e-16 ***
#   cadmium        0.0083376  0.0166260   0.501 0.616087    
# age_screening -0.0015073  0.0004058  -3.714 0.000209 ***
#   gender2        0.0489803  0.0151609   3.231 0.001255 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 0.1168358)
# 
# Null deviance: 240.00  on 2033  degrees of freedom
# Residual deviance: 237.18  on 2030  degrees of freedom
# AIC: 1411.3
# 
# Number of Fisher Scoring iterations: 2




