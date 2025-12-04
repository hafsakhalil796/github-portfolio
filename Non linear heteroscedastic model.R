#Question 1
#install.packages("openxlsx")
#install.packages("sandwich")
#install.packages("lmtest")
#install.packages("boot")
#install.packages("tseries")
library(openxlsx)
df <- read.xlsx("D:\\Preply\\homework2.xlsx", sheet = 1)
attach(df)
log_model <- lm(
  log(Ranking) ~ GDPPC + Social + Freedom + log(Health),
  data = df
)
summary(log_model)

confint(log_model, parm = "GDPPC", level = 0.99)

df_sorted <- df[order(Happy), ]
n <- nrow(df_sorted)
drop <- floor(n/3)  # drop middle third
group1 <- df_sorted[1:(n - drop)/2, ]
group2 <- df_sorted[(n + drop)/2 + 1:n, ]
model1 <- lm(log(Ranking) ~ GDPPC + Social + Freedom + log(Health), data = group1)
model2 <- lm(log(Ranking) ~ GDPPC + Social + Freedom + log(Health), data = group2)
RSS1 <- sum(resid(model1)^2)
RSS2 <- sum(resid(model2)^2)

df1 <- df.residual(model1)
df2 <- df.residual(model2)

# F = larger RSS/df divided by smaller RSS/df
F_stat <- (RSS2/df2) / (RSS1/df1) 
F_stat 
F_critical <- qf(0.95, df2, df1)
F_critical

library(nlme)

# GLS with different variances for two groups (based on Happy)
df$group <- ifelse(df$Happy <= median(df$Happy), "low", "high")

gls_model <- gls(
  log(Ranking) ~ GDPPC + Social + Freedom + log(Health),
  data = df,
  weights = varIdent(form = ~1 | group)  # allows different variance for each group
)

summary(gls_model)
beta_hat <- coef(gls_model)["GDPPC"]
se_hat <- sqrt(gls_model$varBeta["GDPPC","GDPPC"])
z <- qnorm(0.995)   # 99% CI

CI <- beta_hat + c(-1, 1) * z * se_hat
CI

library(sandwich)
library(lmtest)
# White's robust variance-covariance matrix
robust_vcov <- vcovHC(log_model, type = "HC0")  # HC0 is White's original formula

# Robust standard errors
robust_se <- sqrt(diag(robust_vcov))

# Display robust standard errors
robust_se
# Robust t-test
coeftest(log_model, vcov = robust_vcov)

# 99% confidence interval for beta2 (GDPPC)
beta2_hat <- coef(log_model)["GDPPC"]
se2 <- robust_se["GDPPC"]
z <- qnorm(0.995)  # 99% CI

CI <- beta2_hat + c(-1,1) * z * se2
CI


library(tseries)
resid_ols <- residuals(log_model)
jarque.bera.test(resid_ols)


library(boot)
set.seed(1234)
boot_fn <- function(data, indices) {
  # Subset data by bootstrap indices
  d <- data[indices, ]
  # Fit log-linear model
  fit <- lm(log(Ranking) ~ GDPPC + Social + Freedom + log(Health), data = d)
  # Return coefficients
  return(coef(fit))
}
boot_results <- boot(data = df, statistic = boot_fn, R = 5000)

# Bootstrap standard errors
boot_se <- apply(boot_results$t, 2, sd)
boot_se

# 99% confidence interval for beta2 (GDPPC)
boot_ci <- boot.ci(boot_results, index = 2, type = "perc")  # index=2 for beta2
boot_ci


###### Question  2
#regression with interaction
int_model <- lm(Happy ~Corruption+ Health + developed + developed*Corruption,
  data = df
)
summary(int_model)

#regression with another interaction term
int2_model<- lm(Happy ~Corruption+ Health + developed + developed*Corruption+developed * Health,
                data = df)
summary(int2_model)

#Regression approach to detect multicollinearity
aux1_model<-lm(Corruption~ Health + developed + developed*Corruption +developed * Health,
               data = df
)
summary(aux1_model)

aux2_model<-lm(Health~Corruption + developed + developed*Corruption +developed * Health,
               data = df
)
summary(aux2_model)

aux3_model<-lm(developed ~Health+Corruption+ developed*Corruption +developed * Health,
               data = df
)
summary(aux3_model)

aux4_model<-lm(developed*Corruption~developed+Health+Corruption+developed * Health,
               data = df
)
summary(aux4_model)

aux5_model<-lm(developed * Health~developed*Corruption+developed+Health+Corruption,
               data = df
)
summary(aux5_model)

#Detecting multicolinearity through correlation matrix
df$CorruptionDeveloped<-c(df$Corruption*df$developed)
df$HealthDeveloped<-c(df$Health*df$developed)
df_sub <- df[, c("Corruption", "Health", "developed", "CorruptionDeveloped", "HealthDeveloped")]
cor(df_sub)
#install.packages("corrplot")
library(corrplot)
corrplot(cor(df_sub))
cor.test(df$Corruption, df$Health)
cor.test(df$Corruption, df$developed)
cor.test(df$Health, df$developed)
#Barlet's test
install.packages("psych")
library(psych)
corMat <- cor(df[, c("Corruption", "Health", "developed")])
cortest.bartlett(corMat, n = nrow(df))

###### Question  3
linear_model<- lm(Happy~ Corruption +Social+ Health + Freedom, data=df)
summary(linear_model)

#Hausman test of endogenity
install.packages("AER")
library(AER)
iv <- ivreg(Happy ~ Health + Corruption + developed | 
              Generosity + Corruption + developed, data = df)
summary(iv, diagnostics = TRUE)
  
#regression with instrumental variable estimator for health
library(AER)
iv_model <- ivreg(Happy ~ Corruption + Health + developed + CorruptionDeveloped |
    Corruption + developed + CorruptionDeveloped,
  data = df)
summary(iv_model)

#Durbin Watson
install.packages("lmtest")
library(lmtest)
dwtest(linear_model)

#assuming AR(1)
install.packages("nlme")
library(nlme)

# Re-estimate with AR(1) errors
ar1_model <- gls(
  Happy ~ Corruption + Social + Health + Freedom,
  data = df,
  correlation = corAR1(form = ~ 1)  # AR(1) structure
)

summary(ar1_model)
