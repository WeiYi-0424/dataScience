#####Problem set 1 - Question 2#####
#####Part(a)#####
install.packages("AER", type="source")
install.packages("stargazer")
library("AER")
library(stargazer)
data("TeachingRatings")
help("TeachingRatings")
#The unit of observation is an individual course evaluation.
#Each observation is a specific course taught by a professor at the University of Texas at Austin during the academic years 2000–2002.

#####part(b)#####
subset_rating <- subset(TeachingRatings, select = c("eval", "beauty", "age", "allstudents"))
stargazer(subset_rating, type="text", summary = TRUE)


missing_value <- data.frame(
  Variable = c("eval", "beauty", "age", "allstudents", "gender", "minority"),
  MissingValues = c(sum(is.na(TeachingRatings$eval)), sum(is.na(TeachingRatings$beauty)),
                    sum(is.na(TeachingRatings$age)), sum(is.na(TeachingRatings$allstudents)),
                    sum(is.na(TeachingRatings$gender)), sum(is.na(TeachingRatings$minority)))
)
print(missing_value)

#####part(c)#####
plot(TeachingRatings$beauty, TeachingRatings$eval,
     xlab = "Beauty Score",
     ylab = "Teaching Evaluation Score",
     main = "Scatterplot of Teaching Evaluation vs. Beauty",
     pch = 19, # Use solid circles for points
     col = "blue")
#Plotting data can help us to identify the pattern and relationships between variables (e.g positive or negative correlation).
#It can also help us to find the outliers.

#####part(d)#####
mean_beauty <- mean(TeachingRatings$beauty)
mean_eval <- mean(TeachingRatings$eval)
var_beauty <- var(TeachingRatings$beauty)
cov_beauty_eval <- cov(TeachingRatings$beauty, TeachingRatings$eval)
beta_hat_2 <- cov_beauty_eval/var_beauty
beta_hat_1 <- mean_eval - beta_hat_2 * mean_beauty
print(paste0("beta_hat_1 = ", beta_hat_1))
print(paste0("beta_hat_2 = ", beta_hat_2))
#beta_hat_1 = 3.9982721298984, beta_hat_2 = 0.133001448286825.
#Relative to the average courses, rating of the instructor's physical appearance increases by 1, the course overall teaching evaluation score increases by 0.133.

#####part(e)#####
#It is important to include a constant in the above regressions, because the constant means the eval score when the beauty is 0.
#Also there exists negative beauty value, without constant term, the eval score will also be negative and it will increase the residual.