library(ggplot2)

ggplot(data = health_data, aes(x = Total_procrastination, y = Alcohol)) +
  geom_jitter(width = 0.5, height = .1) +
  geom_smooth(se = FALSE, method = "glm", formula = y ~ poly(x,2),
              method.args = list(family = binomial(link = "logit"))) +
  ylim(c(0, 1)) +
  theme_bw()

fit <- glm(Alcohol ~ Total_procrastination + I(Total_procrastination^2), 
           family = "binomial", data = health_data)

# anova(fit, test = "Chisq")

# summary(fit)

f <- function(x){
  exp(coef(fit)[2]) * exp(coef(fit)[3]) * exp(2*x*coef(fit)[3])
}

curve(f(x), xlim = c(0, 60), ylim = c(0.95, 1.05))

abline(h = 1, lty = 2)

uniroot(function(x) f(x)-1, lower = 0, upper = 60)


(-coef(fit)[2] - coef(fit)[3]) / (2*coef(fit)[3])

t <- function(x){
  exp(-0.001119291 * x^2 + 0.05644689 * x - 0.1336388) * (0.05644689 - 0.001119291 * (2 * x))
}

ggplot(data = health_data, aes(x = Total_procrastination, y = Alcohol)) +
  stat_function(fun = t, geom = "line", colour = "blue", linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .5, colour = "red") +
  theme_bw()





