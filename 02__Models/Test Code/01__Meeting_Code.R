library(ggplot2)

ggplot(data = health_data, aes(x = Total_procrastination, y = Alcohol)) +
  geom_jitter(width = 0, height = .1) +
  geom_smooth(se = FALSE, method = "glm", formula = y ~ poly(x,2),
              method.args = list(family = "binomial"))

fit <- glm(Alcohol ~ Total_procrastination + I(Total_procrastination^2), 
           family = "binomial", data = health_data)

anova(fit, test = "Chisq")

summary(fit)


f <- function(x){
  exp(coef(fit)[2]) * exp(coef(fit)[3]) * exp(2*x*coef(fit)[3])
}

curve(f(x), xlim = c(0, 60))

abline(h = 1, lty = 2)

uniroot(function(x) f(x)-1, lower = 0, upper = 60)


(-coef(fit)[2] - coef(fit)[3]) / (2*coef(fit)[3])
