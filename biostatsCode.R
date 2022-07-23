library(car)
data <- Prelim_Dataset_Sheet
logpollution <- log(data$Pollution)
### response variable (air pollution)
boxplot(data$Pollution, horizontal = TRUE, main = "Distribution of Air Pollution Rates \n in
Sample Countries", ylab = "All Countries", xlab = "Air Pollution Rates (mcg/m^3)")
### used log transformation to make a more normal boxplot distribution
boxplot(logpollution, horizontal = TRUE, main = "Log Distribution of Air Pollution Rates \n in
Sample Countries", ylab = "All Countries", xlab = "Air Pollution Rates (log(mcg/m^3))")
fivenum(data$Pollution)
IQR(data$Pollution)
sd(data$Pollution)
### explanatory variable 1 (GDP Growth)
boxplot(data$`GDP Growth`, horizontal = TRUE, main = "Distribution of GDP Growth in
        Sample Countries", ylab = "All Countries", xlab = "GDP Growth (annual %)")
plot(data$`GDP Growth`, logpollution, main = "Log Air Pollution Rates vs GDP Growth", xlab
= "GDP Growth (annual %)", ylab = "Air Pollution Rates (log(mcg/m^3))", pch = 20)
abline(lm(logpollution ~ data$`GDP Growth`), col = 'slateblue')
fivenum(data$`GDP Growth`)
mean(data$`GDP Growth`)
sd(data$`GDP Growth`)
## explanatory variable 2 (Type of Government)
govt <- table(data$Government)
barplot(govt, main = "Government Type In Sample Countries", xlab = "Type of Government",
ylab = "Number of Countries", col = c("salmon", "thistle", "darkseagreen1"))
boxplot(logpollution~data$Government, main = "Log of Air Pollution Rates vs Government
        Type", ylab = "Air Pollution Rates (log(mcg/m^3))", xlab = "Government Type", col =
c("salmon", "thistle", "darkseagreen1"))


### testing equal variance assumption for Government Type
leveneTest(logpollution~Goverment,data=data)

### testing normality assumption for GDP Growth
Gdp_model <-lm(logpollution~`GDP Growth`,data=data)
qqnorm(Gdp_model$residuals, main = "QQ-Plot of Model Residuals")
qqline(Gdp_model$residuals, col='red')

### testing equal variance assumption for GDP Growth
plot(Gdp_model$fitted.values, Gdp_model$residuals, main = "Residual Plot", xlab='Fitted Values', ylab='Residuals', pch=20)
abline(h=0, col='red')

### Running a multi-factor ANOVA test
int_anova1 <-lm(logpollution~`GDP Growth`*Government,data=data)
Anova(int_anova1,type=3)
summary(int_anova1)$adj.r.squared

### post-hoc analysis
library(emmeans)
emmeans(int_anova1, pairwise~Government)

### interaction plot
library(ggplot2) 
qplot(x = `GDP Growth`, y = logpollution, data = data, color = Goverment, xlab = "GDP Growth (%)", ylab = "Mean Air Pollution Rates (log(mcg/m^3))" )+ ggtitle("Mean Log Air Pollution Rates by GDP Growth and Government Type") +
  +     geom_smooth(method = "lm") + theme(plot.title = element_text(size=11), axis.title = element_text(size = 8))
