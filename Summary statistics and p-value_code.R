#---------------------------------------------------------#
#Code file for Assignment 1#
#---------------------------------------------------------#
setwd("/Users/mriduldhar/POL40950")
getwd()
rm(list = ls())
load("ourdata.RData")


#Answer for 2.1: Gender
ourdata$gender
class(ourdata$gender)
table(ourdata$gender)
ourdata$newgender <- as.factor(ourdata$gender)
levels(ourdata$newgender)
table(ourdata$newgender)
freq_gender <- table(ourdata$newgender)
barplot(freq_gender)
barplot(freq_gender, main="Distribution of Gender Among Respondents",
        xlab = "Gender",
        ylab = "Number of respondents")
pdf(file="gender_bar.pdf", height=5, width=5)
summary(ourdata$newgender) #summary won't show var or sd cause it is a nominal variable
table(ourdata$newgender) #newgender is a nominal variable, we can calculate only the frequency of the levels of this variable.



#Answer for 2.2: Respondents' Educational Attainment
ourdata$degree
class(ourdata$degree)
ourdata$newdegree <- as.factor(ourdata$degree)
levels(ourdata$newdegree)
ourdata$newdegree <- factor(ourdata$newdegree, exclude = "NA")
levels(ourdata$newdegree)
table(ourdata$newdegree)
ourdata$newdegree <- factor(ourdata$newdegree, levels = c("LT HS", "HS", "Jun Coll", "Bachelor", "Grad deg"))
range(as.numeric(ourdata$newdegree), na.rm = TRUE)     #calculating the range of ordinal variable newdegree
median_newdegree <- median(as.numeric(ourdata$newdegree), na.rm = TRUE)
median_category <- levels(ourdata$newdegree)[median_newdegree]
median_category              #calculating the median of ordinal variable newdegree
levels(ourdata$newdegree)
summary(ourdata$newdegree)    #calculating the mode of ordinal variable newdegree
freq_newdegree <- table(ourdata$newdegree)
barplot(freq_newdegree)
barplot(freq_newdegree, main="Distribution of educational degrees amongst respondents",
        xlab = "Degree",
        ylab= "Number of respondents")
pdf(file="newdegree_bar.pdf", height=5, width=5 )



#Answer for 2.3: Hours per day watching TV
ourdata$tvhours
class(ourdata$tvhours)
summary(ourdata$tvhours)           #we have to remove the NAs to calculate var and sd
table(ourdata$tvhours)
range((ourdata$tvhours), na.rm = TRUE)
sum(is.na(ourdata$tvhours))
table(ourdata$tvhours, useNA = "ifany")
summary(ourdata$tvhours)
summary(ourdata)
var(ourdata$tvhours, na.rm = TRUE)
var_tvhours <- var(ourdata$tvhours, na.rm = TRUE)
sd(ourdata$tvhours, na.rm = TRUE)
sd_tvhours <- sd(ourdata$tvhours, na.rm = TRUE)
print(paste(var_tvhours, sd_tvhours))
print(paste("Var:", var_tvhours, "Std:", sd_tvhours))
print(paste("Var:", round(var_tvhours,2) , "Std:", round(sd_tvhours,2)))
freq_tvhours <- table(ourdata$tvhours)
hist(ourdata$tvhours, freq=FALSE)
hist(ourdata$tvhours, col ="white", breaks = 20,
     xlab = "tvhours",
     main = "Distribution of respondent's tvhours")
summary(ourdata$tvhours)                 #mean= 2.982 #median= 2.000
table(ourdata$tvhours)                   #mode= 2
abline(v = 2.982, col="blue", lwd=3)
abline(v = 2, col="purple", lwd=3)
abline(v = 2.000, col="red", lwd=3, lty = 3)              #using a dotted line as median and mode are same
text(x = 2.982, y = 360, labels = "Mean", col = "blue", pos = 4)
text(x = 2, y = 350, labels = "Mode", col = "purple", pos = 3)
text(x = 2, y = 310, labels = "Median", col = "red", pos = 3)





#Answer for 2.4: Education and Health Care System
class(ourdata$newdegree)                     #check the already created newdegree variable (Ans 2)
table(ourdata$newdegree)
class(ourdata$healthcare)                     #convert the healthcare variable (character) to newhealth (factor) and exclude the "NA"
ourdata$newhealth <- as.factor(ourdata$healthcare)
ourdata$newhealth <- factor(ourdata$healthcare, exclude = "NA")
class(ourdata$newhealth)                       #inspect the newhealth variable
levels(ourdata$newhealth)
table(ourdata$newhealth)
table(ourdata$newhealth, ourdata$newdegree)                    #creating a contingency table between newhealth and newdegree
chi <- chisq.test(ourdata$newhealth, ourdata$newdegree)
chi
chi$observed
chi$expected
chi$residuals                             #tells us the difference observed and expected
#---------------------------------------------------------#
#---------------------------------------------------------#
