#Question 1

# Create agegroup variable
Dhpv$agegroup <- cut(
  Dhpv$age,
  breaks = c(11, 14, 18, 22, 26),
  labels = c(1, 2, 3, 4),
  right = TRUE,
  include.lowest = TRUE
)
attach(Dhpv)

# Compute totals, frequency, and proportion of completed vaccination
library(dplyr)

summary_table <- Dhpv %>%
  group_by(agegroup, race) %>%
  summarise(
    total_patients = n(),
    completed = sum(completed == 1, na.rm = TRUE),
    proportion_completed = completed / total_patients
  )

summary_table
#Question 2
library(ggplot2)
ggplot(summary_table,
       aes(x = agegroup, y = proportion_completed, fill = race)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of Patients Completing Vaccination",
    x = "Age Group",
    y = "Proportion Completed"
  ) +
  theme_minimal()
#Question 3
plot(agegroup)
plot(race)

##Question 4
install.packages("epitools")
library(dplyr)
library(epitools)   # for odds ratio and risk ratio

# 1. Create the agegroup variable
Dhpv<- Dhpv %>%
  mutate(agegroup = case_when(
    age >= 11 & age <= 14 ~ 1,
    age >= 15 & age <= 18 ~ 2,
    age >= 19 & age <= 22 ~ 3,
    age >= 23 & age <= 26 ~ 4
  ))

# 2. Filter to youngest age group (1)
age1 <- Dhpv %>% filter(agegroup == 1, race %in% c("black", "white"))

# 3. Create 2x2 table: Completed (1) vs Not Completed (0)
tab <- table(age1$race, age1$completed)
tab
# Compute odds
odds_white <- tab["white", "1"] / tab["white", "0"]
odds_black <- tab["black", "1"] / tab["black", "0"]

# Odds ratio(lower odds)
OR <- odds_black / odds_white
OR
p_black <- tab["black", "1"] / sum(tab["black", ])
p_white <- tab["white", "1"] / sum(tab["white", ])
RR <- p_black / p_white
RR #lower probability of  completion in blacks vs whites
#Question 5, logistic
#Question 6
model <- glm(
completed ~ age + race + medassist + locationType + practiceType,
  data = Dhpv,
  family = binomial(link = "logit")
)

#Question 7
summary(model)
confint(model, level = 0.95)
#Question 8, Beta=0?
#Question 9,  because the model is taking 1 category of race (white) as a reference
# white has no coeeficient because its coefficient has been absorbed into the intercept, this is done to avoid perfect multicollinearity

#Question 10, for every  one year increase in age the log odds of completing the vaccination regime decreases by 8%
# The odds of completing the vaccination regime in black is 42% less than in reference group
#The odds of completion of vaccination regime in haspanic is similar to the reference category only a 3-4% decrease
# The other races has approx. 12% lower odds of vaccine completion compared to the reference.

#Question 11
#Yes, race has an influence on the vaccination regime since the black race shows statistically different odds compared to the white race

#Question 12
anova(model, test = "Chisq")
model_null <- glm(completed ~ 1, data = Dhpv, family = binomial)
anova(model_null, model, test = "Chisq")
install.packages("pscl")
library(pscl)
pR2(model)


#Question 13
#Condition_1, if locationType is associated with medassist
chisq.test(locationType, medassist)
#Condition_2, including locationType changes the effect of medassist
model1 <- glm(completed ~ medassist,
              data=Dhpv, family=binomial)

model2 <- glm(completed ~ medassist + locationType,
              data=Dhpv, family=binomial)
summary(model1)
summary(model2)
#Urban clinics may offer medassist more often and also have higher completion rates suggesting confounding.

#Question 14
#combining all races other than black is meaningfull because it simplifies the model and combining similar non significant
#categories will increase statistical power, eliminates complexities and unecessary parameters,  stabalizes the estimates

#Question 15
# Create new aggregated race variable
Dhpv$race_agg <- ifelse(Dhpv$race == "black", "Black", "Who")
Dhpv$race_agg <- factor(Dhpv$race_agg, levels = c("Who", "Black"))  # Who as reference

# Fit the model
model_agg <- glm(completed ~ age + race_agg + medassist + locationType + practiceType,
                 data = Dhpv,
                 family = binomial)

# Summary of the model
summary(model_agg)
library(pscl)
pR2(model_agg)
install.packages("pROC")
library(pROC)

# Task 6: original model
roc_task6 <- roc(Dhpv$completed, fitted(model))
auc_task6 <- auc(roc_task6)
auc_task6

# Task 15: aggregated race model
roc_task15 <- roc(Dhpv$completed, fitted(model_agg))
auc_task15 <- auc(roc_task15)
auc_task15

#Question 17
newdata_task6 <- data.frame(
  age = 11,
  race = factor(c("white", "black"), levels = c("white","black","hispanic","other")),
  medassist = "no",               # no government medical assistance
  locationType = "urban",      # assuming this matches the variable in your data
  practiceType = "OG-Gyn"      # assuming this matches your data
)

newdata_task15 <- data.frame(
  age = 11,
  race_agg = factor(c("Who", "Black"), levels = c("Who","Black")),
  medassist = "no",
  locationType = "urban",
  practiceType = "OG-Gyn"
)
pred_task6 <- predict(model, newdata = newdata_task6, type = "response")
pred_task6

pred_task15 <- predict(model_agg, newdata = newdata_task15, type = "response")
pred_task15
RR_task15<-pred_task15[2]/pred_task15[1]
# RR increase after aggregating race by 15%, black people have lower probability of completion compared to other races

#Question 19
#linear regression is inappropriate because the number of shorts is categorical with 4 possible values only

#Question20
# Chi-square test
chisq_test <- chisq.test(practiceType,shots)
chisq_test
