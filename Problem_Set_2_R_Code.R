

### Question 2a ###############################################################

# Step 1: Creates up a pool of 100,000 observations with the appropriate correlations. 
# Step 2: Samples of 50, 100, 250 and 1000 from the toy dataset 10,000 times
# Step 3: Run OLS and IV regressions on each sample and recover the x coefficients
# Step 4: Calculate means for OLS and IV regressions for each sample size
# Step 5: Graphs the distribution of coefficients by Method(OLS and IV) by sample size. Also identified means

## Packages Required

library(tidyverse)
library(faux)
library(ivreg)
library(ggplot2)
library(stargazer)

theme_set(theme_classic())

# Step 1

  # Base Data
set.seed(1000) #sample
obs = 100000 # observations

  # Initial Parameters
beta = 1 # Stated Beta
alpha = 0 # Stated alpha
delta_zero =0 #Stated delta_zero


  #Generate correlated variables x, epsilon and z
toy_dfa = rnorm_multi(n = obs,
                      r=c(1,0.4,0.5, #  x correlated with epsilon 40%. x correlated with z 50%
                          0.4,1,0, #x correlated with epsilon 40%. e correlated with z 0%
                          0.5,0,1), # x correlated with epsilon 50%. epsilon correlated with z 0%
                      varnames=c("x","epsilon","z"))

  # Combine generate variables into dataframe
toy_dfa = data.frame(toy_dfa, alpha, beta, delta_zero)

 # random sample
  # Create Y variable
toy_dfa = toy_dfa %>% mutate(u=x-z,
                             y=alpha+z+epsilon) #back out y from known variables

  # Check correlations
cor.test(toy_dfa$x,toy_dfa$e) #approx 40%
cor.test(toy_dfa$z,toy_dfa$e) #approx 0%
cor.test(toy_dfa$x,toy_dfa$z) #approx 50%
  
  # Test Regression
test_reg_a = lm(y ~ x, data = toy_dfa)
summary(test_reg_a) 

  # alpha is not zero, but is not statistically significant. Fair to assume it is noise due to sample size. 

# Step 2
  
  # Define sample sizes ans simulations
samples = c(50, 100, 250, 1000)
simulations = 10000
beta_list_a = list()

for (i in 1:length(samples)) {
  sample_size = samples[i]
  
  betas_ols_a = numeric(simulations)
  betas_iv_a = numeric(simulations)
  
  for (j in 1:simulations) {
    sample_index = sample(1:nrow(toy_dfa), sample_size, replace = FALSE)
    sample_data = toy_dfa[sample_index, ]

    # Step 3    
    # OLS
    fit_ols = lm(y ~ x, data = sample_data)
    betas_ols_a[j] = coef(fit_ols)["x"]
    
    # IV
    fit_iv <- ivreg(y ~ x | z, data = sample_data)
    betas_iv_a[j] = coef(fit_iv)["x"]
    
    # Print the iteration number
    print(paste("Iteration", j, "of sample size", sample_size, "done"))
  }
  
  # Convert the vectors to a data frame 
  betas_df_a = data.frame(method = c(rep("OLS", length(betas_ols_a)),rep("IV", length(betas_iv_a))),
                          beta = c(betas_ols_a, betas_iv_a),
                          sample_size = rep(sample_size, length(betas_ols_a) + length(betas_iv_a)))
  
  beta_list_a[[i]] <- betas_df_a
  
}

  # Merge lists into dataframe. 
final_df_a = bind_rows(beta_list_a)

# Step 4
  # Recover means for each estimator and sample size
means_df_a = final_df_a %>%
  group_by(sample_size, method) %>%
  summarize(mean_beta = mean(beta))

stargazer(means_df_a, type="text", title="Table 1: Summary Statistics", out="Table1.txt")
# Step 5
  # Plot the coefficients distributions. Contains 15 non-finite values, extreme values which cannot be represented on plot
Plot_2a = ggplot(final_df_a, aes(x = beta, fill = method)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ sample_size, ncol = 2) +
  scale_fill_manual(values = c("OLS" = "black", "IV" = "grey")) +
  scale_x_continuous(limits = c(-1, 5), expand = c(0, 0)) +
  xlab("Coefficient Estimate") +
  ylab("Density") +
  ggtitle("Coefficient Distribution by Sample Size and Method") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        axis.line = element_line(color = "black"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold")) +
  geom_vline(xintercept = mean(final_df_a[final_df_a$method == "OLS",]$beta),
             color = "black",
             linetype = "dotted", lwd=1) +
  geom_vline(xintercept = mean(final_df_a[final_df_a$method == "IV",]$beta),
             color = "grey",
             linetype = "dotted", lwd=1)

Plot_2a
### Question 2b ###############################################################

#Repetitive of 2a, but with modifications of step 2 to reflect changes in correlations. 
# Step 1: Creates up a pool of 100,000 observations with the appropriate correlations. 
# Step 2: Samples of 50, 100, 250 and 1000 from the toy dataset 10,000 times
# Step 3: Run OLS and IV regressions on each sample and recover the x coefficients
# Step 4: Calculate means for OLS and IV regressions for each sample size
# Step 5: Graphs the distribution of coefficients by Method(OLS and IV) by sample size. Also identified means


  #Generate correlated variables x, epsilon and z
toy_dfb = rnorm_multi(n = obs,
                      r=c(1,0.4,0.15, #  x correlated with epsilon 40%. z correlated with z 15%
                          0.4,1,0, #x correlated with epsilon 40%. e correlated with z 0%
                          0.15,0,1), # x correlated with epsilon 15%. epsilon correlated with z 0%
                      varnames=c("x","epsilon","z"))

  # Combine generate variables into dataframe
toy_dfb = data.frame(toy_dfb, alpha, beta, delta_zero)

  # Create Y variable
toy_dfb= toy_dfb %>% mutate(u=x-z,
                            y=alpha+z+epsilon) #back out y from known variables

  # Check correlations
cor.test(toy_dfb$x,toy_dfb$e) #approx 40%
cor.test(toy_dfb$z,toy_dfb$e) #approx 0%
cor.test(toy_dfb$x,toy_dfb$z) #approx 50%

  # Test Regression
test_reg_b = lm(y ~ x, data = toy_dfb)

  #Summary Results. Beta is not equal to one. Assume it is related to the correlation between x and epsilon
summary(test_reg_b) 

  # Define sample sizes ans simulations

# Step 2
beta_list_b = list()

for (i in 1:length(samples)) {
  sample_size = samples[i]
  
  betas_ols_b = numeric(simulations)
  betas_iv_b = numeric(simulations)
  
  for (j in 1:simulations) {
    sample_index = sample(1:nrow(toy_dfb), sample_size, replace = FALSE)
    sample_data = toy_dfb[sample_index, ]

    # Step 3    
    # OLS
    fit_ols = lm(y ~ x, data = sample_data)
    betas_ols_b[j] = coef(fit_ols)["x"]
    
    # IV
    fit_iv = ivreg(y ~ x | z, data = sample_data)
    betas_iv_b[j] = coef(fit_iv)["x"]
    
    # Print the iteration number
    print(paste("Iteration", j, "of sample size", sample_size, "done"))
  }
  
  # Convert the vectors to a data frame
  betas_df_b = data.frame(method = c(rep("OLS", length(betas_ols_b)),rep("IV", length(betas_iv_b))),
                          beta = c(betas_ols_b, betas_iv_b),
                          sample_size = rep(sample_size, length(betas_ols_b) + length(betas_iv_b)))
  
  beta_list_b[[i]] = betas_df_b
  
}

  # Form data frame from list of coefficient estimates
final_df_b = bind_rows(beta_list_b)

# Step 4
  # Calculate means for each estimator and sample size
means_df_b = final_df_b %>%
  group_by(sample_size, method) %>%
  summarize(mean_beta = mean(beta))

# Step 5
  # Plot the coefficients distributions. Cannot plot 9,297 due to non-finite values.The results become to extreme for ggplot to graph
Plot_2b = ggplot(final_df_b, aes(x = beta, fill = method)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ sample_size, ncol = 2) +
  scale_fill_manual(values = c("OLS" = "black", "IV" = "grey")) +
  scale_x_continuous(limits = c(0, 20), expand = c(0, 0)) +
  xlab("Coefficient Estimate") +
  ylab("Density") +
  ggtitle("Coefficient Distribution by Sample Size and Method") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        axis.line = element_line(color = "black"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold")) +
  geom_vline(xintercept = mean(final_df_b[final_df_b$method == "OLS",]$beta),
             color = "black",
             linetype = "dotted", lwd=1) +
  geom_vline(xintercept = mean(final_df_b[final_df_b$method == "IV",]$beta),
             color = "grey",
             linetype = "dotted", lwd=1)

Plot_2b

##Q3
library(useful)
library(stargazer)
library(texreg)
library(ggplot2)
library(haven)
library(foreign)
library(binsreg)
library(ggplot2)
library(ivreg)


dfdata <- read.dta("C:/UCD/Sem 2/Econometrics/Assignment/assign2.dta")
summary(dfdata)
#data check methods
sum(is.na(dfdata))
print("Position of missing values -")
which(is.na(dfdata))
print("Count of total missing values - ")
sum(is.na(dfdata))
#creating variables
dfdata$age_4 <- dfdata$age^4
dfdata$yob_4 <- dfdata$yob^4
dfdata$age_3 <- dfdata$age^3
dfdata$yob_3 <- dfdata$yob^3
dfdata$age_2 <- dfdata$age^2
dfdata$yob_2 <- dfdata$yob^2

#3a) OLS
reg1 <- lm(formula = logearn~schooling+age+age_2+age_3++age_4+yob+yob_2+yob_3+yob_4, data = dfdata)
summary(reg1)
summary(reg1)$coefficient
## exporting the regression table
stargazer(reg1)

#3c)
dfdata$schoolleaver <- ifelse(dfdata$schooling <15, 1, 0)
## getting probabilities
model1 <- glm(schoolleaver ~ age+age_2+age_3+age_4+yob+yob_2+yob_3+yob_4, data = dfdata, family = binomial(link = "probit"))
predicted_probs <- predict(model1, type = "response")
dfdata$pred_prob <- predicted_probs
## making binscatter of probabilty that aperson leaves before school and yob
plot1 <- binsreg(dfdata$pred_prob, dfdata$yob, line = c(3,3))
sp_1 <- ggplot(data=dfdata, aes(x=yob, y=pred_prob)) + geom_point(alpha = 0) + stat_summary_bin(fun.y='mean', bins=2000, color='black', size=2, geom='point')
sp_1 + geom_vline(xintercept = 33, color = "blue", size=0.8, linetype ="dashed")
## making binscatter schooling and year of birth
plot2 <- binsreg(dfdata$schooling, dfdata$yob, line = c(3,3))
sp_2 <- ggplot(data=dfdata, aes(x=yob, y=schooling)) + geom_point(alpha = 0) + stat_summary_bin(fun.y='mean', bins=2000, color='black', size=2, geom='point') + ylim(14,16)
sp_2 + geom_vline(xintercept = 33, color = "blue", size=0.8, linetype ="dashed")
## making binscatter of log earnings and year of birth
plot_3 <- binsreg(dfdata$logearn, dfdata$yob, line = c(3,3))
sp_3 <- ggplot(data=dfdata, aes(x=yob, y=logearn)) + geom_point(alpha = 0) + stat_summary_bin(fun.y='mean', bins=2000, color='black', size=2, geom='point') + ylim(5.45,5.9)
sp_3 + geom_vline(xintercept = 33, color = "blue", size=0.5, linetype ="dotted")

#3d) Wald estimator and 2SLS
dfdata$LAW <- ifelse(dfdata$yob >= 33, 1, 0)
#first stage
reg2 <- lm(schooling~LAW,
      data = dfdata)
summary(reg2)
#reduced form
reg3 <- lm(logearn~LAW,
           data = dfdata)
summary(reg3)
0.161749/0.99034
#2sls
iv_2 <- ivreg(logearn ~ schooling |
                LAW,
              data = dfdata)
summary(iv_2)
stargazer(iv_2)
#compare OLS and 2SLS
m_list <- list(OLS = reg1, IV = iv_1)
msummary(m_list)



