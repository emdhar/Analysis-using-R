#---------------------------------------------------------#
#Code file for Assignment 3#
#---------------------------------------------------------#



setwd("/Users/mriduldhar/POL40950/HW3") #setting working directory
getwd()
rm(list = ls())                     #clearing the environment
load("income.RData")


#packages to use 
#install.packages("margins")
#install.packages("foreign")  
library(ggplot2)  
library(stargazer)
library(lm.beta)
library(sjPlot)  
library(margins)        #calculate marginal effects






#Answer for Model1: Effect of education and the dummy variables = White, Black, Other on Annual income, keeping White as reference


income$race
class(income$White_dummy)
class(income$Black_dummy)
class(income$Other_dummy)
class(income$educ)
class(income$race)

#creating a subset of all relevant data columns (I am adding age and childs as per Prof. Jungs comment on Slack)
HW3 <-  income[, c("income", "educ", "age", "childs","race", "White_dummy", "Black_dummy", "Other_dummy")]

#Running a multiple regression
model1<- lm(formula = income ~ educ + Black_dummy + Other_dummy + age + childs, data=HW3)
summary(model1)
stargazer(model1,   type="text",
          title = "Regression Results for Model 1",
          dep.var.labels = "Annual Income (USD)",
          covariate.labels = c("Education (Years)" ,"Black Dummy", "Other Dummy", "Age", "Number of Children"),
          out="model1.txt")

#Visualising coefficients of regression 
coefplot1 <- plot_model(model1, show.values = TRUE, show.p = TRUE) +
  labs( y = "Coefficient Estimate", x = "Predictor Variables",  axis.labels = c("Education (Years)" ,"Black Dummy", "Other Dummy", "Age", "Number of Children"),          
        title = "Regression Coefficients for Model 1")   
coefplot1
ggsave("coefplot1.png") 










#Answer for Model 2: Variations in the effect of education on Annual income across different races

# Visualising the scatter plot with multiple slopes+intercepts for education based on race
ggplot(HW3,  aes(x=educ, y=income, color=race)) +  geom_point()+
  geom_smooth(method = "lm",  se = FALSE, size = 1)+
  labs(
    x = "Education",
    y = "Income",
    title = "Relationship between Income and Education with Race"
  ) + theme_classic()
ggsave("Model2_slope.pdf") 


# Interaction term (educ x Black_dummy); dropping Other_dummy as its effects on Income are statistically insignificant according to regression results in Model 1
model2<- lm(formula = income ~ educ + Black_dummy + Other_dummy + educ*Black_dummy + educ*Other_dummy + age + childs, data=HW3)
summary(model2) 

stargazer(model2,   type="text",
          title = "Interaction: educ X Black X Other",
          dep.var.labels = "Annual Income (USD)",
         covariate.labels = c("Education (Years)" ,"Black dummy", "Other dummy", "Age", "Number of Children", "Educ x Black", "Educ x Other"),
          out="model2.txt")

#calculating average marginal effect of education by race
margins(model2, variables ="educ", 
        at = list(Black_dummy= as.factor(c("0", "1"))) )
m2 <- margins(model2, variables ="educ", 
        at = list(Black_dummy= as.factor(c("0", "1"))) )
summary(m2)
ame.educ1  <- summary(m2)

#visualising the margins
ggplot(data = ame.educ1, aes(x=Black_dummy, y=AME)) +
  geom_point(size = 2) +  
  theme_classic() 

ggplot(data = ame.educ1, aes(x=Black_dummy, y=AME)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)  +
  geom_hline(yintercept = 0) +
  labs(
    title = "AME of Education by Race (95% CIs)",
    x = "Race",
    y = "AME"
  ) + 
  scale_x_discrete(labels = c("White", "Black")) + 
  theme_classic() 

ggsave("ame_race_educ.png") 









#Answer for Model 3: Variations in the effect of education on Annual income with the number of children

model3 <- lm(formula = income ~ educ + Black_dummy + Other_dummy + age + childs + educ*childs, data=HW3)
summary(model3)
stargazer(model3,   type="text",
          title = "Interaction: educ X childs",
          dep.var.labels = "Annual Income (USD)",
         covariate.labels = c("Education (Years)" ,"Black dummy","Other dummy", "Age", "Number of Children","Educ X N Children" ),
          out="model3.txt")


#calculating average marginal effect of education by number of children
summary(income$childs)
margins(model3, variables ="educ", 
        at = list(childs= c(0 , 1, 2, 3, 4, 5, 6, 7, 8)) )
m3 <- margins(model3, variables ="educ", 
              at = list(childs= c(0 , 1, 2, 3, 4, 5, 6, 7, 8)) )
summary(m3)
ame.educ2  <- summary(m3)

#plotting margins
ggplot(data = ame.educ2, aes(x=childs, y=AME)) +
  geom_point(size = 2) +  
  theme_classic() 

ggplot(data = ame.educ2, aes(x= childs, y=AME)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  scale_x_continuous(breaks = c(0 , 1, 2, 3, 4, 5, 6, 7, 8),
                     labels = c("0(min)", "1", "2", "3", "4", "5", "6", "7", "8(max)")) +
  geom_line() +
  geom_hline(yintercept = 0) +
  labs(
    title = "AME of Education by Number of Children(95% CIs)",
    x = "Number of Children",
    y = "AME"
  )  + theme_classic() 

ggsave("ame_childs_educ.png") 

