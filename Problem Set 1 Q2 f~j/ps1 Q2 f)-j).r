#Preparations
install.packages("AER", type="source")
install.packages("stargazer")
library("AER")
library(stargazer)
data("TeachingRatings")

#Question 2(f)
ols<-lm(TeachingRatings$eval~TeachingRatings$beauty+TeachingRatings$age+TeachingRatings$gender)

ols<-lm(eval~beauty+age+I(age**2)+log(allstudents)+gender+minority+gender:minority,data=TeachingRatings)
summary(ols)

#*OUTCOMES:
#Call:
#lm(formula = eval ~ beauty + age + I(age^2) + log(allstudents) + 
#    gender + minority + gender:minority, data = TeachingRatings)

#Residuals:
#     Min       1Q   Median       3Q      Max 
#-1.82048 -0.36540  0.07108  0.39548  1.13393 
#
#Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#(Intercept)               3.6978272  0.5732534   6.451 2.85e-10 ***
#beauty                    0.1566171  0.0333042   4.703 3.41e-06 ***
#age                       0.0345926  0.0234193   1.477  0.14034    
#I(age^2)                 -0.0003929  0.0002403  -1.635  0.10267    
#log(allstudents)         -0.0885579  0.0299777  -2.954  0.00330 ** 
#genderfemale             -0.1839788  0.0568162  -3.238  0.00129 ** 
#minorityyes              -0.0164786  0.1108320  -0.149  0.88187    
#genderfemale:minorityyes -0.2610383  0.1486546  -1.756  0.07976 .  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.5284 on 455 degrees of freedom
#Multiple R-squared:  0.1069,    Adjusted R-squared:  0.09317 
#F-statistic: 7.781 on 7 and 455 DF,  p-value: 6.612e-09
#*

stargazer(ols,type="text")

Residuals <- resid(ols)
print(Residuals)
cov_beauty_resid<-cov(TeachingRatings$beauty,Residuals)
print(cov_beauty_resid)
# [1] -1.231115e-17

install.packages("ggplot2")
library(ggplot2)
plot3<-ggplot(TeachingRatings, aes(x=log(allstudents), y=Residuals))
plot3<-plot3+geom_point()+labs(title='A Scatter Plot of the Residuals against ln(allstudents_i)')
plot3
ggsave('Figures&Tables/scatterPlotResid.png',plot = plot3, width = 10, height = 7, dpi = 300)

plot_temp<-ggplot(TeachingRatings)+geom_density(aes(x=Residuals),color="black")
TeachingRatings$"normalDistribution_x"=seq(-2,2,4.0/(nrow(TeachingRatings)-1)) 
TeachingRatings$"normalDistribution_y"=dnorm(seq(-2,2,4.0/(nrow(TeachingRatings)-1)),mean=mean(Residuals),sd=sd(Residuals))
plot4<-plot_temp+geom_line(aes(x=normalDistribution_x,y=normalDistribution_y),color="grey")+labs(x='Residuals',y='',title='Density Plot of Residuals over Normal Distribution Plot')
plot4
ggsave('Figures&Tables/densityOfResidualsOverANormalDistribution.png',plot = plot4, width = 10, height = 7, dpi = 300)