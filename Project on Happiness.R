#Project on Happiness
#install.packages("openxlsx")
#install.packages("sandwich")
#install.packages("lmtest")
#install.packages("boot")
#install.packages("tseries")
#install.packages("GGally")
#install.packages("psych")
#install.packages("corrplot")
#install.packages("AER")
#install.packages("nlme")
#install.packages("interactions")
df <- openxlsx::read.xlsx("D:\\Preply\\Happiness data.xlsx", sheet = 1)
attach(df)

library(ggplot2)
# Histogram of happiness
ggplot(df, aes(Happy)) + geom_histogram(bins=20, fill="steelblue", color="white") +
  theme_minimal() + ggtitle("Distribution of Happiness Scores")
#Histogram v/s GDP
ggplot(df, aes(GDPPC, Happy)) + geom_point() + geom_smooth(method="lm") +
  theme_minimal() + ggtitle("Happiness vs GDP per capita")

#Exploring dependencies
library(corrplot)
numeric_vars <- df[, c("Happy", "GDPPC", "Social", "Freedom", "Health", "Corruption", "Generosity")]
cor_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot(cor_matrix, method="circle", type="upper")

#scatter plots to closely watch the variables found correlated above
GGally::ggpairs(df[, c("Happy", "GDPPC", "Social", "Freedom","Health")])

#Happiness in Developed v/s Developing countries
df$developed <- factor(df$developed, labels=c("Developing","Developed"))

ggplot(df, aes(developed, Happy)) +
  geom_boxplot(fill="lightgreen") +
  geom_jitter(width=0.2, alpha=0.5) +
  ggtitle("Happiness Scores by Development Status")


#linear modelling
linear_model<-lm(Happy~ GDPPC +Social+ Freedom +Health +Corruption, data=df)
summary(linear_model)

#Test of model's misspecification
library(lmtest)
resettest(linear_model)

#log- non linear model
log_model <- lm(
  log(Happy) ~ GDPPC + Social + Freedom + log(Health),
  data = df
)
summary(log_model)

confint(log_model, parm = "GDPPC", level = 0.99)

#
library(interactions)
interact_plot(int_model, pred = Corruption, modx = developed)

plot(log_model$fitted.values, resid(log_model),
     xlab="Fitted values", ylab="Residuals",
     main="Residual plot for log-linear model")
abline(h=0, col="red")

#Goldfeld- Quandt test with obs. ordered acc. to happy
df_sorted <- df[order(Happy), ]
n <- nrow(df_sorted)
drop <- floor(n/3)  # drop middle third
group1 <- df_sorted[1:(n - drop)/2, ]
group2 <- df_sorted[(n + drop)/2 + 1:n, ]
model1 <- lm(log(Happy) ~ GDPPC + Social + Freedom + log(Health), data = group1)
model2 <- lm(log(Happy) ~ GDPPC + Social + Freedom + log(Health), data = group2)
RSS1 <- sum(resid(model1)^2)
RSS2 <- sum(resid(model2)^2)

df1 <- df.residual(model1)
df2 <- df.residual(model2)

# F = larger RSS/df divided by smaller RSS/df
F_stat <- (RSS2/df2) / (RSS1/df1) 
F_stat 
F_critical <- qf(0.95, df2, df1)
F_critical # if F stat > F critical, there is heteroscedasticity 

#heteroscedastic can be dealt with gls
library(nlme)
# GLS with different variances for two groups (based on Happy)
df$group <- ifelse(df$Happy <= median(df$Happy), "low", "high")

gls_model <- gls(
  log(Ranking) ~ GDPPC + Social + Freedom + log(Health),
  data = df,
  weights = varIdent(form = ~1 | group)  # allows different variance for each group
)

summary(gls_model)
#gls on linear model
gls_linear_model <- gls(Ranking ~ GDPPC + Social + Freedom + Health,
                        data = df,
                        weights = varIdent(form = ~1 | group)  # allows different variance for each group
)

summary(gls_linear_model)
#multicollinearity test
library(car)
vif(linear_model)
vif(log_model)

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

#Corrected standard errors for linear model
library(sandwich)
library(lmtest)
# White's robust variance-covariance matrix
robust_lin_vcov <- vcovHC(linear_model, type = "HC0")  # HC0 is White's original formula

# Robust standard errors
robust_lin_se <- sqrt(diag(robust_vcov))

# Display robust standard errors
robust_lin_se
# Robust t-test
coeftest(linear_model, vcov = robust_lin_vcov)
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

library(corrplot)
corrplot(cor(df_sub))
cor.test(df$Corruption, df$Health)
cor.test(df$Corruption, df$developed)
cor.test(df$Health, df$developed)
#Barlet's test

library(psych)
corMat <- cor(df[, c("Corruption", "Health", "developed")])
cortest.bartlett(corMat, n = nrow(df))

###### Question  3
linear_model<- lm(Happy~ Corruption +Social+ Health + Freedom, data=df)
summary(linear_model)

#Hausman test of endogenity

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

library(lmtest)
dwtest(linear_model)

#assuming AR(1)

library(nlme)

# Re-estimate with AR(1) errors
ar1_model <- gls(
  Happy ~ Corruption + Social + Health + Freedom,
  data = df,
  correlation = corAR1(form = ~ 1)  # AR(1) structure
)

summary(ar1_model)
