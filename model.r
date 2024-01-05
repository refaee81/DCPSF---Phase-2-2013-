
getwd()
setwd('C:/Users/ramsey/Documents/Darfur/Stata Module/')
library(readxl)
library(writexl)
library(boot)
library(mlogit)#require(mlogit) package
library(distr)#require(distr) package 
library(stats)
library(pscl)#require(pscl) package
library(ROCR)
install.packages('foreign')
library('foreign')
mydata <- read.dta('C:/Users/ramsey/Documents/Darfur/Stata Module/AB3.dta')

names(mydata)
dim(mydata)

write_xlsx(mydata, "mydata.xlsx")

head(mydata)

###Missing Value Code 
mice(mydata, m = 5, method = vector("character", length = ncol(data)), predictorMatrix = (1 - diag(1, ncol(data))), visitSequence = (1:ncol(data))[apply(is.na(data), 2, any)], form = vector("character", length = ncol(data)), post = vector("character", length = ncol(data)), defaultMethod = c("polyreg"), maxit = 5, diagnostics = TRUE, printFlag = TRUE, seed = NA)
###



library(sqldf)

d1<- sqldf('select * from mydata where country = "Sudan"')

write_xlsx(d1, "C:/Users/ramsey/Desktop/CRC Interview/d1.xlsx")

names(d1) [c(9, 10, 12, 13, 16, 19, 21, 24, 279, 280,291)]=c("Region","Urbanity", "Gender", "Eco_Satisfaction", "Living_Condition", "Intention_Immigration", "Sense_Safety","Sense_Equality", "Marital", "Profession", "Ethnicity")
mydata<-d1[,c("Region","Urbanity", "Gender", "Eco_Satisfaction", "Living_Condition", "Intention_Immigration", "Sense_Safety","Sense_Equality", "Marital", "Profession", "Ethnicity")]
dim(mydata)

########Stage 2: Grouping, Leveling, plotting, & Relabeling
#note: sequence of original data levels should be considered when labeling and recoding levels 

setwd('C:/Users/ramsey/Desktop/CRC Interview/Visualization')

##### Region
Region<-factor(mydata$Region)
summary(Region)
levels(Region)<-c("The Red Sea"=0, "Northern"=0, "Khartoum"=0,"al-Jazeera"=0,"Sennar"=0, "North Kurdufan"=1, "South Darfur"=2, "Kassala"=0, "al-Qadarif"=0, "White Nile"=1, "North Darfur"=1, "South Kordofan"=2, "Nile"=3, "Blue Nile"=4, "West Darfur"=4)
Region_Labeled <- factor(Region, levels = c(0,1,2,3,4), labels = c("Other", "N.Darfur", "S.Darfur", "E.Darfur", "W.Darfur"))#
summary(Region_Labeled)
Region<- as.numeric(Region)
hist(Region, freq = TRUE, labels = TRUE, nclass = 6, plot = TRUE, breaks = 5, col=c("darkblue","red"))
plot(Region_Labeled, main="Regions of Respondents", col=c("darkblue","red"))

######Urbanity

Urbanity<-factor(mydata$Urbanity)
summary(Urbanity)
levels(Urbanity)<-c("Urban"=0, "Rural"=1)
Urbanity_Labeled <- factor(Urbanity, levels = c(0,1), labels = c("Urban","Rural")) 
summary(Urbanity_Labeled)
Urbanity<- as.numeric(Urbanity)
hist(Urbanity, freq = TRUE, labels = TRUE, nclass = 6, plot = TRUE, breaks = 5, col = "lightblue", border = "pink")
plot(Urbanity_Labeled, main="Urbanity", col=c("darkblue","red"))

#### Gender

Gender<-factor(mydata$Gender)
summary(Gender)
levels(Gender)<-c("Male"=0, "Female"=1)
Gender_Labeled <- factor(Gender, levels = c(0,1), labels = c("Male","Female")) 
summary(Gender_Labeled)
Gender<- as.numeric(Gender)
hist(Gender, freq = TRUE, labels = TRUE, nclass = 2, plot = TRUE, breaks = 2, col=c("darkblue","red"))
plot(Gender_Labeled, main="Gender", ylab = "Freq", col=c("darkblue","red"))

##### Eco_Satisfaction

Eco_Satisfaction<-factor(mydata$Eco_Satisfaction)
summary(Eco_Satisfaction)
levels(Eco_Satisfaction)<-c("Very good"=0, "Good"=0, "Bad"=1, "Very bad"=1, "Don't know"=2, "Refuse"=2)
Eco_Satisfaction_Labeled <- factor(Eco_Satisfaction, levels = c(0,1,2), labels = c("Good","Bad", "Unsure")) 
summary(Eco_Satisfaction_Labeled)
Eco_Satisfaction<- as.numeric(Eco_Satisfaction)
plot(Eco_Satisfaction_Labeled, main="Eco_Satisfaction", ylab = "Freq", col=c("darkblue","red", "green"))

##### Living_Condition

Living_Condition<-factor(mydata$Living_Condition)
summary(Living_Condition)
levels(Living_Condition)<-c("Much worse"=1, "Worse"=1, "Similar"=2, "Better"=0, "Much better"=0, "Don't know"= 3, "Refuse"=3)
Living_Condition_Labeled <- factor(Living_Condition, levels = c(0,1,2,3), labels = c("Better","Similar", "Worse", "Unsure")) 
summary(Living_Condition_Labeled)
Living_Condition<- as.numeric(Living_Condition)
plot(Living_Condition_Labeled, main="Living_Condition", ylab = "Freq", col=c("darkblue","red", "green"))

##### Intention_Immigration (DV)

Intention_Immigration<-factor(mydata$Intention_Immigration)
summary(Intention_Immigration)
levels(Intention_Immigration)<-c("Yes, for economic reasons"=0, "Yes, for political reasons"=0, "Yes, for economic and political reasons"=0, "No, I do not think about emigrating"=1, "Don't know"=1, "Refuse"=1)
Intention_Immigration_Labeled <- factor(Intention_Immigration, levels = c(0,1), labels = c("Yes","No")) 
summary(Intention_Immigration_Labeled)
Intention_Immigration<- as.numeric(Intention_Immigration)
plot(Intention_Immigration_Labeled, main="Intention for Immigration", ylab = "Freq", col=c("darkblue","red", "green"))

###### Sense_Safety


Sense_Safety<-factor(mydata$Sense_Safety)
summary(Sense_Safety)
levels(Sense_Safety)<-c("Fully ensured"=0, "Ensured"=0, "Not ensured"=1, "Absolutely not ensured"=1, "Don't know"=1, "Refuse"=1)
Sense_Safety_Labeled <- factor(Sense_Safety, levels = c(0,1), labels = c("Yes","No")) 
summary(Sense_Safety_Labeled)
Sense_Safety<- as.numeric(Sense_Safety)
plot(Sense_Safety_Labeled, main="Sense of Safety", ylab = "Freq", col=c("darkblue","red", "green"))

### Sense_Equality

Sense_Equality<-factor(mydata$Sense_Equality)
summary(Sense_Equality)
levels(Sense_Equality)<-c("To a great extent"=0, "To a medium extent"=1, "To a limited extent"=2, "Not at all"=3, "Don't know"=4, "Refuse"=4)
Sense_Equality_Labeled <- factor(Sense_Equality, levels = c(0,1,2,3,4), labels = c("High","Med", "Low", "None", "Unsure")) 
summary(Sense_Equality_Labeled)
Sense_Equality<- as.numeric(Sense_Equality)
plot(Sense_Equality_Labeled, main="Sense of Equality", ylab = "Freq", col=c("darkblue","red", "green"))

#### Marital

Marital<-factor(mydata$Marital)
summary(Marital)
levels(Marital)<-c("Bachelor"=0, "Married"=1, "Divorced"=2)
Marital_Labeled <- factor(Marital, levels = c(0,1,2), labels = c("Single","Married", "Divorced")) 
summary(Marital_Labeled)
Marital<- as.numeric(Marital)
plot(Marital_Labeled, main="Marital Status", ylab = "Freq", col=c("darkblue","red", "green"))

#### Profession

Profession<-factor(mydata$Profession)
summary(Profession)
levels(Profession)<-c(levels(Profession),"Unknown")  #Add the extra level to your factor
Profession[is.na(Profession)] <- "Unknown"           #Change NA to "None"
levels(Profession)<-c("Agricultural worker/Owner of a farm"=1, "Craftsperson"=1, "Does not work"=0, "Don't know"=1, "Employer/director of an institution with 10 employees or more"=1, "Employer/director of an institution with less than 10 employees"=1, "Government employee"=1, "Housewife"=0, "Manual laborer"=1, "Member of the armed forces/public security"=1, "Owner of a shop/grocery store"=1, "Private sector employee"=1, "Professional such as lawyer, accountant, teacher, doctor, etc."=1, "Refuse"=0, "Retired"=0, "Student"=0, "Unknown"=2)
Profession_Labeled <- factor(Profession, levels = c(0,1,2), labels = c("Unemployed","Employed", "Undeclared")) 
summary(Profession_Labeled)
Profession<- as.numeric(Profession)
plot(Profession_Labeled, main="Employment", ylab = "Freq", col=c("darkblue","red", "green"))


install.packages('mice')
install.packages('tibble')

library(mice)
mice(mydata, m = 5, defaultMethod = c("polyreg"), maxit = 5, diagnostics = TRUE, printFlag = TRUE, seed = NA)


#### Ethnicity 

Ethnicity<-factor(mydata$Ethnicity)
summary(Ethnicity)
levels(Ethnicity)<-c("Arabic"=0, "English"=1, "French"=1, "Nubian"=1, "Hausa"=1, "Bija"=1, "Masalit"=1, "Nubian-Dongolawi"=1, "Arnqa"=1, "Burqu"=1, "Tama"=1, "Dongolawi"=1, "Zaghawa"=1, "Fuwari"=1, "Berita"=1, "Rutani-Hausa"=1, "Flata"=1, "Kneen"=1, "Hadendoa"=1, "Does not speak second language"=0)
Ethnicity_Labeled <- factor(Ethnicity, levels = c(0,1), labels = c("Arabic","Other")) 
summary(Ethnicity_Labeled)
Ethnicity<- as.numeric(Ethnicity)
plot(Ethnicity_Labeled, main="Ethnicity", ylab = "Freq", col=c("darkblue","red", "green"))

####

crosstab(mydata, row.vars = c("Ethnicity", "Gender"), col.vars = c("Sense_Equality", "Intention_Immigration"), 
         type = "f", addmargins = FALSE)

ggplot(mydata, aes(x = Ethnicity_Labeled, y = frequency(mydata)))+
  geom_col(aes(fill = Sense_Equality_Labeled), width = 0.7)

ggplot(mydata, aes(x = Intention_Immigration_Labeled, y = frequency(mydata)))+
  geom_col(aes(fill = Sense_Equality_Labeled), width = 0.7)

ggplot(mydata, aes(y = Sense_Equality_Labeled, x = Sense_Safety_Labeled))+
  geom_col(aes(fill = Intention_Immigration_Labeled), width = 0.5)



##### 

Binary_Logistic_Data<-data.frame(Intention_Immigration,Region, Urbanity, Gender, Eco_Satisfaction, Marital, Living_Condition,Sense_Safety,Profession, Ethnicity)
head(Binary_Logistic_Data)

write_xlsx(Binary_Logistic_Data, "C:/Users/ramsey/Desktop/CRC Interview/Binary_Logistic_Data.xlsx")

##### Modeling 

CAT_regression<- lm(Intention_Immigration~Region+Urbanity+Gender+Eco_Satisfaction+Marital+Living_Condition+Gender+Sense_Safety+Profession+Ethnicity, data = Binary_Logistic_Data)
summary(CAT_regression)

####

Logistic_regression <- glm(Intention_Immigration~Region+Urbanity+Gender+Eco_Satisfaction+Marital+Living_Condition+Gender+Sense_Safety+Profession+Ethnicity, data = Binary_Logistic_Data)
summary(Logistic_regression)

Logistic_regression_Labeled <- glm(Intention_Immigration_Labeled~Region_Labeled+Urbanity_Labeled+Gender_Labeled+Eco_Satisfaction_Labeled+Marital_Labeled+Living_Condition_Labeled+Sense_Safety_Labeled+Profession_Labeled+Ethnicity_Labeled, data = Binary_Logistic_Data, binomial)
summary(Logistic_regression_Labeled)

##### Sense of Safety glm
 
Logistic_regression_Labeled <- glm(Sense_Safety_Labeled ~ Intention_Immigration_Labeled+Region_Labeled+Urbanity_Labeled+Gender_Labeled+Eco_Satisfaction_Labeled+Marital_Labeled+Living_Condition_Labeled+Profession_Labeled+Ethnicity_Labeled, data = Binary_Logistic_Data, binomial)
summary(Logistic_regression_Labeled)



library(rattle)
rattle()

######## RF model 


install.packages('randomForest')
library(randomForest)
train=sample(1:nrow(Binary_Logistic_Data),300)

Binary_Logistic_Data.rf=randomForest(Sense_Safety_Labeled ~ . , data = Binary_Logistic_Data , subset = train)
Binary_Logistic_Data.rf





#### results of logit analysis 

glm.diag.plots(Logistic_regression_Labeled, glmdiag = glm.diag(Logistic_regression_Labeled), subset = NULL,
               iden = FALSE, labels = NULL, ret = FALSE)

pR2(Logistic_regression_Labeled)

###### Accuracy of model 

data.test<-Binary_Logistic_Data[,c("Intention_Immigration","Region", "Urbanity","Gender","Eco_Satisfaction","Marital","Living_Condition","Sense_Safety","Profession","Ethnicity")]

library(ROCR)
p <- predict(Logistic_regression_Labeled, newdata=subset(data.test), type="response")
pr <- prediction(p, data.test)
summary(p)
fitted.results <- ifelse(p > 0.5,1,0)
misClasificError <- mean(fitted.results != data.test$Voting, na.rm=TRUE)
print(paste('Accuracy',1-misClasificError))

plot(Logistic_regression_Labeled, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))



