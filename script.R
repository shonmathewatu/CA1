#loading The dataset
heart_failure <- read.csv("HeartFailure.csv", header = TRUE)
heart_failure

print(heart_failure)
summary(heart_failure)
sum(is.na(heart_failure))

# Setting up Hypothesis

#***************************Statement 1***************************#
#* Smoking vs Death_Event
#* Both are categorical variables
#* Null Hypothesis, H0 : Smoking is not related to death 
#* Alternate Hypothesis H1 : Smoking is related to death
#* Death_Event is the independent variable
#* Smoking is dependent variable 
#* 
#*
unique(heart_failure$smoking)
unique(heart_failure$DEATH_EVENT)
# display 2 charts in 1 row
#par(mfrow = c(1,2))

hist(heart_failure$smoking, 
     col = "steelblue", 
     main = "frequency chart of smoking data")
hist(heart_failure$DEATH_EVENT, 
     col = "steelblue", 
     main = "frequency chart of DEATH_EVENT")

# display 2 charts in 1 row
#par(mfrow = c(1,2))
# Create a QQ plot for heart_failure$smoking
qqnorm(heart_failure$smoking)
qqline(heart_failure$smoking, col = "red")

qqnorm(heart_failure$DEATH_EVENT)
qqline(heart_failure$DEATH_EVENT, col = "red")


plot(heart_failure$smoking, heart_failure$DEATH_EVENT,
     xlab = "smoking", ylab = "DEATH_EVENT",
     main = "Scatter Plot of smoking vs DEATH_EVENT")

shapiro.test(heart_failure$smoking)
# p value <0.05, hence not normally distributed
shapiro.test(heart_failure$DEATH_EVENT)
# p value <0.05, hence not normally distributed


# Perform Kruskal-Wallis test
result <- chisq.test(heart_failure$smoking, heart_failure$DEATH_EVENT)
print(result)
# p value >.05, and hence smoking is not related to death.

#***************************Statement 2***************************#
#* serum creatinine vs platelets
#* Null Hypothesis, H0 :  serum creatinine has no role in platelets count
#* Alternate Hypothesis H1 : serum creatinine has effect in platelets count
#* Both are continues variables
#* serum creatinine is the dependent variable
#* platelets is the independent variable

hist(heart_failure$creatinine, 
     col = "steelblue", 
     main = "frequency chart of creatinine")
hist(heart_failure$platelets, 
     col = "steelblue", 
     main = "frequency chart of platelets")

# display 2 charts in 1 row
par(mfrow = c(1,2))
# Create a QQ plot for creatinine
qqnorm(heart_failure$creatinine)
qqline(heart_failure$creatinine, col = "red")

# Create a QQ plot for serum_creatinine

qqnorm(heart_failure$platelets)
qqline(heart_failure$platelets, col = "red")


plot(heart_failure$serum_creatinine, heart_failure$platelets,
     xlab = "Serum Creatinine", ylab = "Platelets",
     main = "Scatter Plot of Serum Creatinine vs Platelets")

result <- shapiro.test(heart_failure$serum_creatinine)
print(result)
#p-value < 2.2e-16, since p value < .05,  normally distributed

# Perform Shapiro-Wilk test for platelets
result <- shapiro.test(heart_failure$platelets)
print(result)
# p-value = 2.883e-12, since p value < .05,  normally distributed

# wilcox  correlation test
result <- wilcox.test(heart_failure$platelets, heart_failure$creatinine)
print(result)
# null hypo rejected

#***************************Statement 3***************************#
#* creatinine_phosphokinase vs serum_sodium
#* Null Hypothesis, H0 : creatinine_phosphokinase is not affected by serum_sodium
#* Alternate Hypothesis H1 : creatinine_phosphokinase is affected by serum_sodium
#* Both are continues variables
#* 


hist(heart_failure$creatinine_phosphokinase, 
     col = "steelblue", 
     main = "frequency chart of creatinine_phosphokinase")
hist(heart_failure$serum_sodium, 
     col = "steelblue", 
     main = "frequency chart of serum_sodium")

# display 2 charts in 1 row
par(mfrow = c(1,2))
# Create a QQ plot for creatinine_phosphokinase
qqnorm(heart_failure$creatinine_phosphokinase)
qqline(heart_failure$creatinine_phosphokinase, col = "red")

# Create a QQ plot for serum_creatinine

qqnorm(heart_failure$serum_sodium)
qqline(heart_failure$serum_sodium, col = "red")


plot(heart_failure$creatinine_phosphokinase, heart_failure$serum_sodium,
     xlab = "creatinine_phosphokinase", ylab = "serum_sodium",
     main = "Scatter Plot of creatinine_phosphokinase vs serum_sodium")

# Perform Shapiro-Wilk test for creatinine_phosphokinase
result <- shapiro.test(heart_failure$creatinine_phosphokinase)
print(result)

# Perform Shapiro-Wilk test for serum_sodium
result <- shapiro.test(heart_failure$serum_sodium)
print(result)

# wilcox  correlation test
result <- wilcox.test(heart_failure$serum_sodium, heart_failure$creatinine_phosphokinase)
print(result)

# Spearman's rank correlation test
result <- cor.test(heart_failure$serum_sodium, heart_failure$creatinine_phosphokinase, method = "spearman")
print(result)

#***************************Statement 4***************************#
#*ejection_fraction vs serum_creatinine
#* Null Hypothesis, H0 : ejection_fraction has no affect on serum_creatinine
#* Alternate Hypothesis H1 : ejection_fraction has affect on serum_creatinine
#* Both are continues variables
#* ejection_fraction is the independent variable
#*  serum_creatinine is the dependent variable


summary(heart_failure$ejection_fraction)

hist(heart_failure$ejection_fraction, 
     col = "steelblue", 
     main = "frequency chart of ejection_fraction")
hist(heart_failure$serum_creatinine, 
     col = "steelblue", 
     main = "frequency chart of serum_creatinine")

# display 2 charts in 1 row
par(mfrow = c(1,2))
# Create a QQ plot for ejection_fraction
qqnorm(heart_failure$ejection_fraction)
qqline(heart_failure$ejection_fraction, col = "red")

# Create a QQ plot for serum_creatinine

qqnorm(heart_failure$serum_creatinine)
qqline(heart_failure$serum_creatinine, col = "red")


plot(heart_failure$ejection_fraction, heart_failure$serum_creatinine,
     xlab = "ejection_fraction", ylab = "serum_creatinine",
     main = "Scatter Plot of ejection_fraction vs serum_creatinine")



# Perform Shapiro-Wilk test for ejection_fraction
result <- shapiro.test(heart_failure$ejection_fraction)
print(result)

# Perform Shapiro-Wilk test for serum_creatinine
result <- shapiro.test(heart_failure$serum_creatinine)
print(result)


# Spearman's rank correlation test
result <- wilcox.test(heart_failure$ejection_fraction, heart_failure$serum_creatinine)

print(result)

# null hypo rejected

#***************************Statement 5***************************#
#* creatinine_phosphokinase vs DEATH_EVENT
#* Null Hypothesis, H0 : creatinine_phosphokinase has no effect in DEATH_EVENT
#* Alternate Hypothesis H1 : creatinine_phosphokinase has effect in DEATH_EVENT
#* One is continues and one is categorcal variable
#* DEATH_EVENT is the dependent variable
#* creatinine_phosphokinase is the independent variable.

summary(heart_failure$creatinine_phosphokinase)
summary(heart_failure$DEATH_EVENT)

# Boxplot
boxplot(heart_failure$creatinine_phosphokinase ~ heart_failure$DEATH_EVENT, 
        ylab = "Creatinine Phosphokinase", xlab = "Death Event")

# Create a QQ plot for creatinine_phosphokinase
qqnorm(heart_failure$creatinine_phosphokinase)
qqline(heart_failure$creatinine_phosphokinase, col = "red") 

qqnorm(heart_failure$DEATH_EVENT)
qqline(heart_failure$DEATH_EVENT, col = "red")

# Perform Kruskal-Wallis test
result <- kruskal.test(heart_failure$creatinine_phosphokinase, heart_failure$DEATH_EVENT)
print(result)

# p value greater than .05, and hence null hypo accepted