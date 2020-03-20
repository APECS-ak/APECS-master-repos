#Looking at C and N data
library(MASS)
library(lme4)

whisker <- read.csv("SI/whiskers.csv")

#normality? - NO
plot(density(whisker$C));plot(density(whisker$N))
shapiro.test(whisker$C); shapiro.test(whisker$N)
qqnorm(whisker$C)
qqnorm(whisker$N)


#lme
test.model <- lmer(C~Season + (1 | OtterID), data = whisker)
summary(test.model)
coef(summary(test.model))

str(resid(test.model))

# from Franz's Pollock condition r file
visreg(test.model)
VarCorr(test.model) # season and otter ID are equally influential or their contribution to the variablilty is equal

ranef(test.model) # slope for each Otter
hist(ranef(test.model)[,1]) #this isn't working, don't know why
plot(test.model)

anova(test.model)

ggplot(fortify(test.model), aes(Season, C)) +
  stat_summary(fun.data=mean_se, geom="pointrange") +
  stat_summary(aes(y=.fitted), fun.y=mean, geom="line")

