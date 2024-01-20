library(ggplot2)

ggplot(data = health_data, aes(x = Total_procrastination, y = Alcohol)) +
  geom_jitter(width = 0.5, height = .1) +
  geom_smooth(se = FALSE, method = "glm", formula = y ~ poly(x,2),
              method.args = list(family = binomial(link = "logit"))) +
  ylim(c(0, 1)) +
  theme_bw()

health_data %>%
  select(Education, Alcohol) %>%
  filter(complete.cases(.)) %>%
  mutate(Alcohol = factor(Alcohol)) %>%
  mutate(Education = factor(case_when(
    Education == "0" ~ "No Degree",
    Education == "1" ~ "GED",
    Education == "2" ~ "High School Diploma",
    Education == "3" ~ "College (2 yrs)",
    Education == "4" ~ "College (4 yrs)",
    Education == "5" ~ "Master Degree",
    Education == "6" ~ "Professional Degree",
  ), levels = c("No Degree", "GED", "High School Diploma", "College (2 yrs)", 
                "College (4 yrs)", "Master Degree", "Professional Degree"))) %>%
  ggplot(aes(x = Education, fill = Alcohol)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(x = "Highest Educational Status") +
  theme_bw()


ggplot(data = health_data, aes(x = factor(Education), y = factor(Alcohol))) +
  geom_bar(stat = "identity")

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



