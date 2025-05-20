#---------------------------------------------------------#
#Code file for Assignment 2#
#---------------------------------------------------------#


setwd("/Users/mriduldhar/POL40950/HW2") #setting working directory
getwd()
rm(list = ls())                     #clearing the environment
load("gssdata.RData")


#install and run all visualisation packages

install.packages("ggplot2")
install.packages("stargazer")
install.packages("lm.beta")
install.packages("sjPlot")


library(ggplot2)  
library(stargazer)
library(lm.beta)
library(sjPlot)  





#Answer for Model 1: sei vs. educ

gssdata$sei                             #inspecting the variable sei, educ
class(gssdata$sei)
summary(gssdata$sei)                       #it has 112 NA's
gssdata$educ
class(gssdata$educ)
summary(gssdata$educ)                       # it has 5 NA's
HW2 <- gssdata[, c("sei", "educ", "age", "tvhours")]      #subset creation from gssdata R file of variables required for this homework

cor(HW2$sei, HW2$educ, method = "pearson", use="complete.obs")   
#Pearsons R coefficient: 0.5957052, indicates a weak positive relationship between sei and edu


#Scatter plot b/w sei and educ using ggplot package
ggplot(HW2, aes(x=educ, y=sei)) + 
  geom_point(position = position_jitter(width = 0.4, height = 0.3)) +
  stat_smooth(method = "lm", col = "blue", se = FALSE,   size = 0.5) + labs(
    x = "Education (Years)",
    y = "Socio-Economic Index",
    title = "Relationship between Socio-Economic Index and Education" )
ggsave("scatter_sei_educ.jpeg", width = 10, height = 5) 


#Results of Linear regression model, sei vs educ using stargazer package
model1 <- lm(formula = sei ~ educ, data=HW2)
summary(model1)  
stargazer(model1, type="text",
          dep.var.labels = "Socio-Economic Index",
          covariate.labels = "Education (Years)",
          digits = 2, out="model1.txt")







#Answer for Model 2: sei with educ and age

gssdata$age               #inspecting the variable age
class(gssdata$age)
summary(gssdata$age)      #has 10 NA's


#Scatter plot b/w sei, educ and age using ggplot package
ggplot(HW2, aes(x=educ, y=sei, color=age)) + 
  geom_point(position = position_jitter(width = 0.6, height = 0.6)) +
  stat_smooth(method = "lm", col = "red", se=FALSE,   size = 1) +
  labs(
    x = "Education (Years)",
    y = "Socio-Economic Index",
    title = "Relationship between Socio-Economic Index and Education with Age"
  )

ggsave("scatter_sei_educ_age.jpeg",  width = 10, height = 5) 

#Results of Linear regression model of sei with educ and age  using stargazer package
model2 <- lm(formula = sei ~ educ + age, data=HW2)
summary(model2)  
stargazer(model2, type="text",
          dep.var.labels = "Socio-Economic Index",
          covariate.labels = c("Education (Years)", "Age"),
          digits = 2, out="model2.txt")










#Answer for Model 3: sei with educ, age, and tvhours

gssdata$tvhours               #inspecting the variable tvhours
class(gssdata$tvhours)
summary(gssdata$tvhours)      #has 699  NA's

#Results of Linear regression model of sei with educ and age  using stargazer package
model3 <- lm(formula = sei ~ educ + age +tvhours, data=HW2)
summary(model3)  
stargazer(model3, type="text",
          dep.var.labels = "Socio-Economic Index",
          covariate.labels = c("Education (Years)", "Age", "TV Hours"),
          digits = 2, out="model3.txt")








#Answer for Model 4: Model Comparison

#Descriptive statistics 
descriptive <- as.data.frame(HW2)
stargazer(descriptive, type="text", 
          title="Descriptive Statistics",
          digits=2, out="table1.txt") 

#Labeling for presentation
stargazer(descriptive, type="text", 
          title="Descriptive Statistics",
          digits=2, out="descriptive.txt",
          covariate.labels = 
            c("Socio-Economic Index", "Education (Years)", "Age", "TV Hours") )

#Combine M1, M2 and M3
stargazer(model1, model2, model3, type="text",
          title = "Regression Results for Socioeconomic Status (GSS 2010)",
          dep.var.labels = "Socio-Economic Index",
          covariate.labels = c("Education (Years)" ,"Age", "TV Hours"),
          digits = 2,  out="model1_model2_model3.txt")


#For comparison
Q4_Plot <- plot_models(model3, model2, model1, show.values = TRUE,  grid=TRUE,  show.legend = FALSE,
                       line.size = 0.8,   colors = "bw",
                       axis.labels = c("TV Hours" ,"Age", "Education (Years)" ),
                       m.labels = c("Model 3", "Model 2", "Model 1" ) ,   axis.title = "Estimates on Socio-Economic Index")  +  aes(shape = group) 
Q4_Plot 
ggsave("Q4_Plot.jpeg", width = 12, height = 5) 


