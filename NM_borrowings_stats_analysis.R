#read in the file #hello
nm <- read.csv("/Users/SanchoPanza/Desktop/mydata.csv", header = TRUE)

#isolate the words from the rest of the data
nmwords <- nm[18:54]

#to recode the words
nmwords[nmwords < 2] =0 #don't know
nmwords[nmwords == 2] =1 #recognize but don't know
nmwords[nmwords > 2] =2 #recognize and use these words

#to calculate the percentage of used
n <- nrow(nmwords)

#to sum based on the codes made above and add a column to the original data frame
for (i in 1:n){
  nm$WordsUsed[i] <- sum(nmwords[i,]==2, na.rm=TRUE)
  nm$WordsRec[i] <- sum(nmwords[i,]==1, na.rm=TRUE)
  nm$WordsNotUsed[i] <- sum(nmwords[i,]==0, na.rm=TRUE)
}


#recode education/highest degree obtained
#nm$Q24

nm$degreeAttained[nm$Q24 == 1 ] = "Less than High School"
nm$degreeAttained[nm$Q24 == 2 ] = "High School/GED"
nm$degreeAttained[nm$Q24 == 3 ] = "BA/BS"
nm$degreeAttained[nm$Q24 == 4 ] = "Master's"
nm$degreeAttained[nm$Q24 == 5 ] = "Doctorate"
nm$degreeAttained[nm$Q24 == 6 ] = "Other professional degree"

#Heritage Speaker
nm$heritageSpeaker[nm$Q40_2 > 2 ] = "non-heritage Speaker"
nm$heritageSpeaker[nm$Q40_1 > 2 ] = "heritage Speaker"
table(nm$heritageSpeaker)

#Native language
nm$nativeLanguage[nm$Q37 == 1] = "English"
nm$nativeLanguage[nm$Q37 == 2] = "Spanish"
nm$nativeLanguage[nm$Q37 == 3] = "English/Spanish"
nm$nativeLanguage[nm$Q37 >3] = "Other"
table(nm$nativeLanguage)

#bilingual
nm$Bilingual[nm$Q38 == 1] = "Yes"
nm$Bilingual[nm$Q38 == 2] = "No"
table(nm$Bilingual)

#Dominant language
nm$langDominant[nm$Q39 == 1] = "English"
nm$langDominant[nm$Q39 == 2] = "Spanish"
nm$langDominant[nm$Q39 == 3] = "Bilingual EN/ES"
nm$langDominant[nm$Q39 >3] = "other"
table(nm$langDominant)


#calculating the percentages and adding subsequent columns to the original data frame
nm$sumAnswered <- nm$WordsUsed + nm$WordsRec +nm$WordsNotUsed

nm$PercUsed <- nm$WordsUsed / nm$sumAnswered
nm$PercRec <- nm$WordsRec / nm$sumAnswered
nm$PercNotUsed <- nm$WordsNotUsed / nm$sumAnswered
nm$PercUsedAndRec <- nm$PercUsed + nm$PercRec

#lived in NM after 15
nm$Arrival[nm$Q29 == 1] <- "Adulthood"
nm$Arrival[nm$Q29 == 2] <- "Childhood"

table(nm$arrival)

#lived in NM before 15
nm$Arrival[nm$Q21 == 1 & nm$Q29 == 1] <- "Childhood" #could use this and one below to separate into both for childhood and adulthood
nm$Arrival[nm$Q21 == 1 & nm$Q29 != 1] <- "Childhood" 
nm$Arrival[nm$Q21 == 2] <- "Adulthood"

table(nm$Arrival)
table(nm$arrival)

#Ethnicity
nm$Ethnicity[nm$Q34_2==1] <- 'Hispanic'
nm$Ethnicity[nm$Q34_2==0 & nm$Q34_1==1] <- 'White non-Hispanic'
nm$Ethnicity[nm$Q34_2==0 & nm$Q34_3==1] <- "Other"
nm$Ethnicity[nm$Q34_2==0 & nm$Q34_4==1] <- "Other"
nm$Ethnicity[nm$Q34_2==0 & nm$Q34_5==1] <- "Other"
nm$Ethnicity[nm$Q34_2==0 & nm$Q34_6==1] <- "Other"
table(nm$Ethnicity)

#Create sum variable for ethnicity questions (total selected)
nm$SumEth <- nm_ethnicity$Q34_2 + nm_ethnicity$Q34_1 + nm_ethnicity$Q34_3 + nm_ethnicity$Q34_4 + nm_ethnicity$Q34_5 + nm_ethnicity$Q34_6
table(nm_ethnicity$Ethnicty , nm_ethnicity$Q34_2)

#do first, second, and third....which makes sense....
nm$Generation[nm$Q21==1 | nm$Q29==1] <- 'First'
nm$Generation[nm$Q30==1 | nm$Q31==1] <- 'Second'
nm$Generation[nm$Q32==1 | nm$Q33==1] <- 'Third'

######age no recoding necessary

age <- nm$Q17

###Gender

nm_gender <- nm$Q16

#merge the data frame:
nmdf <- data.frame(age, nm$Generation, nm$Ethnicity, nm$PercUsed, nm$PercRec, nm_gender, nm$nativeLanguage, nm$Bilingual, nm$langDominant, nm$heritageSpeaker, nm$Arrival, nm$PercUsedAndRec )

nmdf <- data.frame(nm$Generation, nm$Ethnicity, nm$PercUsedAndRec)
dim(nmdf)

# running anovas with PercUsed:

nm_gen.aov <- aov(nmdf$nm.PercUsed ~ nmdf$nm.Generation)
summary(nm_gen.aov)
TukeyHSD(nm_gen.aov)

nm_gender.aov <- aov(nmdf$nm.PercUsed ~ nmdf$nm_gender)
summary(nm_gender.aov)

nm_ethnicity.aov <- aov(nmdf$nm.PercUsed ~ nmdf$nm.Ethnicity)
summary(nm_ethnicity.aov)
plot(TukeyHSD(nm_ethnicity.aov))

nm_langDom.aov <- aov(nmdf$nm.PercUsed ~ nmdf$nm.langDominant)
summary(nm_langDom.aov)
TukeyHSD(nm_langDom.aov)

nm_nativeLanguage.aov <- aov(nmdf$nm.PercUsed ~ nmdf$nm.nativeLanguage)
summary(nm_nativeLanguage.aov)
TukeyHSD(nm_nativeLanguage.aov)

nm_heritage.aov <- aov(nmdf$nm.PercUsed ~ nmdf$nm.heritageSpeaker)
summary(nm_heritage.aov)
TukeyHSD(nm_heritage.aov)

TukeyHSD(nm_gen.aov)

#to plot and look at what was stat sig of PercUsed
#need this for paper
boxplot(nmdf$nm.PercUsed ~ nmdf$nm.Ethnicity, xlab = "Ethnicity", ylab ="Percentage used", main = "Ethnicity and Usage")
boxplot(nmdf$nm.PercUsed ~ nmdf$nm.Generation, xlab = "Generation", ylab ="Percentage used", main = "Generation and Usage")
#ned this one for paper
boxplot(nmdf$nm.PercUsed ~ nmdf$nm.heritageSpeaker, xlab = "Spanish as a Heritage Language", ylab ="Percentage used", main = "SHL and Usage")
boxplot(nmdf$nm.PercUsed ~ nmdf$nm.langDominant, xlab = "Language Dominance", ylab ="Percentage used", main = "title")
#need this for paper
boxplot(nmdf$nm.PercUsed ~ nmdf$nm.nativeLanguage, xlab = "Native Language", ylab ="Percentage used", main = "Native Language as predictor of Usage")

#running linear regression models of PercUsed
#for the continuous variable:
summary(lm(nmdf$nm.PercUsed ~ nmdf$age))

#for the significant results
#currently ordered by looks of the smallest P value from ANOVAs

#model4
summary(lm(nmdf$nm.PercUsed ~ nmdf$nm.heritageSpeaker + nmdf$nm.Ethnicity + nmdf$nm.Generation + nmdf$nm.nativeLanguage + nmdf$nm.langDominant))
#model3 WINNER MODEL for lowest AIC value and highest r^2 value. determined comparing AIC values and differences in values
summary(lm(nmdf$nm.PercUsed ~ nmdf$nm.heritageSpeaker + nmdf$nm.Ethnicity + nmdf$nm.Generation + nmdf$nm.nativeLanguage))
#model2
summary(lm(nmdf$nm.PercUsed ~ nmdf$nm.heritageSpeaker ))
#model1
summary(lm(nmdf$nm.PercUsed ~ nmdf$nm.heritageSpeaker + nmdf$nm.Ethnicity))

#summary(lm(nmdf$nm.PercUsed ~  nmdf$nm.nativeLanguage + nmdf$nm.heritageSpeaker + nmdf$nm.langDominant))

# testing AIC values
model4 <- lm(nmdf$nm.PercUsed ~ nmdf$nm.heritageSpeaker + nmdf$nm.Ethnicity + nmdf$nm.Generation)

model3 <- lm(nmdf$nm.PercUsed ~ nmdf$nm.heritageSpeaker + nmdf$nm.Ethnicity)

model2 <- lm(nmdf$nm.PercUsed ~ nmdf$nm.heritageSpeaker)

#model1 <- lm(nmdf$nm.PercUsed ~ nmdf$nm.heritageSpeaker + nmdf$nm.Ethnicity)

#AIC(model1)
AIC(model2)
AIC(model3)
AIC(model4)

#AIC(model1) - AIC(model3)
AIC(model2) - AIC(model3)
AIC(model4) - AIC(model3)

#
summary(lm(nmdf$nm.PercUsed ~ nmdf$nm.heritageSpeaker + nmdf$nm.Ethnicity + nmdf$nm.Generation + nmdf$nm.nativeLanguage))
plot(nmdf$nm.heritageSpeaker, nmdf$nm.PercUsed)
abline(lm(nmdf$nm.PercRec ~ nmdf$nm.heritageSpeaker))
#testing colinearity with vif() in car package
??vif
library(car)

colinearity1 <- vif(lm(nm.PercUsed ~ nm.heritageSpeaker + nm.nativeLanguage + nm.langDominant, data = nmdf))

summary(colinearity1)

#results are all under 4, so no colinearity

#look at same tests but with Percent Recognized

# running anovas:

nm_gen.aov <- aov(nmdf$nm.PercRec ~ nmdf$nm.Generation)
summary(nm_gen.aov)

nm_gender.aov <- aov(nmdf$nm.PercRec ~ nmdf$nm_gender)
summary(nm_gender.aov)

nm_ethnicity.aov <- aov(nmdf$nm.PercRec ~ nmdf$nm.Ethnicity)
summary(nm_ethnicity.aov)

nm_langDom.aov <- aov(nmdf$nm.PercRec ~ nmdf$nm.langDominant)
summary(nm_langDom.aov)
TukeyHSD(nm_langDom.aov)

nm_nativeLanguage.aov <- aov(nmdf$nm.PercRec ~ nmdf$nm.nativeLanguage)
summary(nm_nativeLanguage.aov)
TukeyHSD(nm_nativeLanguage.aov)

nm_heritage.aov <- aov(nmdf$nm.PercRec ~ nmdf$nm.heritageSpeaker)
summary(nm_heritage.aov)
TukeyHSD(nm_heritage.aov)

TukeyHSD(nm_gen.aov)

#plotting the stat sig results:
boxplot(nmdf$nm.PercRec ~ nmdf$nm.heritageSpeaker, xlab = "Heritage Language", ylab ="Percentage Recognized", main = "title")
boxplot(nmdf$nm.PercRec ~ nmdf$nm.heritageSpeaker, xlab = "Heritage Language", ylab ="Percentage Recognized", main = "title")


#running linear regression models
#for the continuous variable:
summary(lm(nmdf$nm.PercRec ~ nmdf$age))

#for the significant results
#currently ordered by looks of the smallest P value from ANOVAs

#the model run based on stat sig ANOVAs
summary(lm(nmdf$nm.PercRec ~  nmdf$nm.Generation + nmdf$nm.Ethnicity + nmdf$nm.heritageSpeaker))

#summary(lm(nmdf$nm.PercRec ~ nmdf$nm.heritageSpeaker + nmdf$nm.Ethnicity + nmdf$nm.Generation + nmdf$nm.nativeLanguage))

#summary(lm(nmdf$nm.PercRec ~ nmdf$nm.heritageSpeaker + nmdf$nm.Ethnicity + nmdf$nm.Generation))

#summary(lm(nmdf$nm.PercRec ~ nmdf$nm.heritageSpeaker + nmdf$nm.Ethnicity))

summary(lm(nmdf$nm.PercUsed ~  nmdf$nm.nativeLanguage + nmdf$nm.heritageSpeaker + nmdf$nm.langDominant))

#testing colinearity with vif() in car package
??vif
library(car)

colinearityRec <- vif(lm(nm.PercRec ~ nm.heritageSpeaker + nm.Ethnicity + nm.Generation, data = nmdf))

summary(colinearityRec)

#not colinear? 

#with PercUsed and PercRec
# running anovas:

nm_genUR.aov <- aov(nmdf$nm.PercUsedAndRec ~ nmdf$nm.Generation)
summary(nm_genUR.aov)
TukeyHSD(nm_genUR.aov)

nm_genderUR.aov <- aov(nmdf$nm.PercUsedAndRec ~ nmdf$nm_gender)
summary(nm_genderUR.aov)

nm_ethnicityUR.aov <- aov(nmdf$nm.PercUsedAndRec ~ nmdf$nm.Ethnicity)
summary(nm_ethnicityUR.aov)
TukeyHSD(nm_ethnicityUR.aov)

nm_langDomUR.aov <- aov(nmdf$nm.PercUsedAndRec ~ nmdf$nm.langDominant)
summary(nm_langDomUR.aov)
TukeyHSD(nm_langDom.aov)

nm_nativeLanguageUR.aov <- aov(nmdf$nm.PercUsedAndRec ~ nmdf$nm.nativeLanguage)
summary(nm_nativeLanguageUR.aov)
TukeyHSD(nm_nativeLanguage.aov)

nm_heritageUR.aov <- aov(nmdf$nm.PercUsedAndRec ~ nmdf$nm.heritageSpeaker)
summary(nm_heritageUR.aov)
TukeyHSD(nm_heritage.aov)

nm_BilingualUR.aov <- aov(nmdf$nm.PercUsedAndRec ~ nmdf$nm.Bilingual)
summary(nm_BilingualUR.aov)
TukeyHSD(nm_gen.aov)

#plotting the stat sig results:
boxplot(nmdf$nm.PercUsedAndRec ~ nmdf$nm.heritageSpeaker, xlab = "Spanish as Heritage Language", ylab ="Percent", main = "SHL Usage/Recognition")
boxplot(nmdf$nm.PercUsedAndRec ~ nmdf$nm.Generation, xlab = "Generation", ylab ="Percentage", main = "Generation Usage/Recognition")
boxplot(nmdf$nm.PercUsedAndRec ~ nmdf$nm.Ethnicity, xlab = "Ethnicity", ylab ="Percentage", main = "Ethnicity and Usage/Recognition")


#running linear regression models
#for the continuous variable:
summary(lm(nmdf$nm.PercUsedAndRec ~ nmdf$age))

#for the significant results
#currently ordered by looks of the smallest P value from ANOVAs

#the model run based on stat sig ANOVAs
#summary(lm(nmdf$nm.PercUsedAndRec ~  nmdf$nm.Generation + nmdf$nm.Ethnicity + nmdf$nm.heritageSpeaker))
#model3
summary(lm(nmdf$nm.PercUsedAndRec ~ nmdf$nm.heritageSpeaker + nmdf$nm.Ethnicity + nmdf$nm.Generation))
#model2
summary(lm(nmdf$nm.PercUsedAndRec ~ nmdf$nm.heritageSpeaker + nmdf$nm.Ethnicity))
#model1
summary(lm(nmdf$nm.PercUsedAndRec ~ nmdf$nm.heritageSpeaker))


#summary(lm(nmdf$nm.PercUsed ~  nmdf$nm.nativeLanguage + nmdf$nm.heritageSpeaker + nmdf$nm.langDominant))
nmdf$nm.Per
# testing AIC values
model3RU <- lm(nmdf$nm.PercUsedAndRec ~ nmdf$nm.heritageSpeaker + nmdf$nm.Ethnicity + nmdf$nm.Generation)

model2RU <- lm(nmdf$nm.PercUsedAndRec ~ nmdf$nm.heritageSpeaker + nmdf$nm.Ethnicity)

model1RU <- lm(nmdf$nm.PercUsedAndRec ~ nmdf$nm.heritageSpeaker)

#model1RU <- lm(nmdf$nm.PercUsed ~ nmdf$nm.heritageSpeaker + nmdf$nm.Ethnicity)

AIC(model1RU)
AIC(model2RU)
AIC(model3RU)
#AIC(model4)

AIC(model1RU) - AIC(model2RU)
AIC(model3RU) - AIC(model2RU)

#results from AICs are that models 2 and models 1 are the best for UsedAndRec

#AIC(model4) - AIC(model3)

#testing colinearity with vif() in car package
??vif
library(car)

colinearityRec <- vif(lm(nm.PercRec ~ nm.heritageSpeaker + nm.Ethnicity + nm.Generation, data = nmdf))

summary(colinearityRec)


#How to subset data#Ethnicity
hispanics.df <- subset(nmdf, nm.Ethnicity == "Hispanic")
whitepeeps.df <- subset(nmdf, nm.Ethnicity == "White non-Hispanic")
others.df <- subset(nmdf, nm.Ethnicity == "Other")

hs_whitepeeps <- 
  
  #Generation
  generation3.df <- subset(nmdf, nm.Generation == "Third")
dim(generation3.df)
generation2.df <- subset(nmdf, nm.Generation == "Second")
dim(generation2.df)
generation1.df <- subset(nmdf, nm.Generation == "First")
dim(generation1.df)

#Ethnicity specific

#White people
white_arrival.aov <- aov(whitepeeps.df$nm.PercUsed ~ whitepeeps.df$nm.Generation)
summary(white_arrival.aov)
table(whitepeeps.df$nm.Generation)

white_gen.aov <- aov(whitepeeps.df$nm.PercUsed ~ whitepeeps.df$nm.Generation)
summary(white_gen.aov)
table(whitepeeps.df$nm.Generation)

white_SHL.aov <- aov(whitepeeps.df$nm.PercUsed ~ whitepeeps.df$nm.heritageSpeaker)
summary(white_SHL.aov)
table(whitepeeps.df$nm.heritageSpeaker)

table(whitepeeps.df$nm.Generation)
boxplot(whitepeeps.df$nm.PercUsedAndRec ~ whitepeeps.df$nm.Generation, xlab = "Generation", ylab ="Percentage", main = "non-Hispanic: Generation and Usage/Recognition")


#Hispanics
hispanic_SHL.aov <- aov(hispanics.df$nm.PercUsed ~ hispanics.df$nm.heritageSpeaker)
summary(hispanic_SHL.aov)
table(hispanics.df$nm.heritageSpeaker)

hispanic_generation.aov <- aov(hispanics.df$nm.PercUsed ~ hispanics.df$nm.Generation)
summary(hispanic_generation.aov)

hispanic_generationUR.aov <- aov(hispanics.df$nm.PercUsedAndRec ~ hispanics.df$nm.Generation)
summary(hispanic_generationUR.aov)

boxplot(hispanics.df$nm.PercUsedAndRec ~ hispanics.df$nm.Generation, xlab = "Generation", ylab ="Percentage", main = "Hispanic: Generation and Usage/Recognition")
boxplot(hispanics.df$nm.PercUsed ~ hispanics.df$nm.Generation, xlab = "Generation", ylab ="Percentage", main = "Hispanic: Generation and Usage")
boxplot(hispanics.df$nm.PercUsedAndRec ~ hispanics.df$nm.heritageSpeaker, xlab = "SHL", ylab ="Percentage", main = "SHL and Usage/Recognition")
boxplot(hispanics.df$nm.PercUsedAndRec ~ hispanics.df$nm.Ethnicity, xlab = "Ethnicity", ylab ="Percentage", main = "Ethnicity and Usage/Recognition")

#Others
others_SHL.aov <- aov(others.df$nm.PercUsed ~ others.df$nm.heritageSpeaker)
summary(others_SHL.aov)
table(others.df$nm.heritageSpeaker)

table(hispanics.df$age)
table(hispanics.df$nm.Generation)
#Generation1 arrival

gen1_arrival.aov <- aov(generation1.df$nm.PercUsed ~ generation1.df$nm.Arrival)
summary(gen1_arrival.aov)
table(nmdf$arrival)

#Heritage Language
SHL.df <- subset(nmdf, nm.heritageSpeaker =="heritage Speaker")
head(SHL.df)

SHL_genU.aov <- aov(SHL.df$nm.PercUsed ~ SHL.df$nm.Generation)
summary(SHL_genU.aov)
TukeyHSD(SHL_genU.aov)

SHL_ethnicityU.aov <- aov(SHL.df$nm.PercUsed ~ SHL.df$nm.Ethnicity)
summary(SHL_ethnicityU.aov)
TukeyHSD(SHL_ethnicityU.aov)

boxplot(SHL.df$nm.PercUsed ~ SHL.df$nm.Generation, xlab = "Generation", ylab ="Percentage", main = "SHL:Generation Usage")
boxplot(SHL.df$nm.PercUsed ~ SHL.df$nm.Ethnicity, xlab = "Ethnicity", ylab ="Percentage", main = "SHL:Ethnicity and Usage")


summary(lm(SHL.df$nm.PercUsed ~ SHL.df$nm.Ethnicity + SHL.df$nm.Generation))

#heritage
heritage.df <- subset(nmdf, nm.heritageSpeaker == "heritage Speaker")
mean(heritage.df$nm.PercUsed)

nonheritage.df <- subset(nmdf, nm.heritageSpeaker = "non-heritage Speaker")

t.test(heritage.df$nm.PercUsed, generation3.df$nm.PercUsed)

head(heritage.df)
#Generation
generation3.df <- subset(nmdf, nm.Generation == "Third")
dim(generation3.df)
generation2.df <- subset(nmdf, nm.Generation == "Second")
dim(generation2.df)
generation1.df <- subset(nmdf, nm.Generation == "First")
dim(generation1.df)

#bar plot with ggplot2
ggplot(nm_heritage, aes(x=nm.Generation, y = nm.PercUsed, fill=nm.heritageSpeaker))+
  + geom_bar(position = position_dodge(), stat="identity")+
  + geom_errorbar(aes(ymin=nm.PercUsed - se, ymax = nm.PercUsed +se), width = .2, position = position_dodge(.9)) +
  + xlab("Generation") +
  + ylab("Percent")

ggplot(nm_heritage, aes(x=nm.heritageSpeaker, y = nm.PercUsed,))+
  + geom_bar(position = position_dodge(), stat="identity")+
  + geom_errorbar(aes(ymin=nm.PercUsed - se, ymax = nm.PercUsed +se), width = .2, position = position_dodge(.9)) +
  + xlab("Spanish as Heritage Language") +
  + ylab("Percent")

myData <- aggregate(nmdf$PercUsed, by = list(SHL = nmdf$nm.heritageSpeaker, Generation = nmdf$nm.Generation), FUN = function(x) c(mean= mean(x), sd = sd(x), n = length(x)))

#third generation1.df
gen3_shlU.aov <- aov(generation3.df$nm.PercUsed ~ generation3.df$nm.heritageSpeaker)
summary(gen3_shlU.aov)
TukeyHSD(gen3_shlU.aov)

gen3_ethnicityU.aov <- aov(generation3.df$nm.PercUsed ~ generation3.df$nm.Ethnicity)
summary(gen3_ethnicityU.aov)
TukeyHSD(gen3_ethnicityU.aov)

#first generation
gen1_shlU.aov <- aov(generation1.df$nm.PercUsed ~ generation1.df$nm.heritageSpeaker)
summary(gen1_shlU.aov)
TukeyHSD(gen1_shlU.aov)

gen1_ethnicityU.aov <- aov(generation1.df$nm.PercUsed ~ generation1.df$nm.Ethnicity)
summary(gen1_ethnicityU.aov)
TukeyHSD(gen1_ethnicityU.aov)

a <- table(nmdf$nm.Generation)
barplot(a, ylim=c(0, 90), main= "Generation of Participants", ylab= "Count", xlab="Generation", col ='#018571')
b <- table(nmdf$nm.heritageSpeaker)
barplot(b, ylim=c(0, 90), main= "Heritage Speakers", ylab= "Count", col ='#b2abd2')


barplot(c, ylim=c(0, 90), main= "Ethnicity of Participants", ylab= "Count", xlab="Ethnicity", col ='cyan2')

matrix(1:2, ncol=1)
[,1] [,2]
[1,]    1    2
layout(matrix(1:2, ncol=2))

matrix(1:3, ncol=3)
layout(matrix(1:3, ncol=3))

hs_whitepeeps <- table(whitepeeps.df$nm.heritageSpeaker)
hs_hispanics <- table(hispanics.df$nm.heritageSpeaker)

barplot(hs_whitepeeps, ylim=c(0, 30), main= "Non-Hispanic participants and HS variable", ylab= "Count", col ='orange')
barplot(hs_hispanics, ylim=c(0, 70), main= "Hispanic participants and HS variable", ylab= "Count", col ='darkolivegreen2')

#boxplots

boxplot(nmdf$nm.PercUsed~ nmdf$nm.Generation, main= "Percent use vs. generation", xlab = "Generation", ylab= "Percent Use", col = "#018571")
boxplot(nmdf$nm.PercUsed~ nmdf$nm.heritageSpeaker, main= "Percent use vs. heritage speaker", ylab= "Percent Use", col = "#b2abd2")
boxplot(nmdf$nm.PercUsed~ nmdf$nm.Ethnicity, main= "Percent use vs. ethnicity", xlab= "Ethnicity", ylab= "Percent Use", col = "cyan2")

#histogram
hist(nonheritage.df$nm.PercUsed, main= "Histogram of Percent Use of Spanish Borrowings in English", xlab="Percent Use", ylab="Count", col='#31a35475',ylim=c(0,30),xlim=c(0, 1.0))
hist(heritage.df$nm.PercUsed, main= "Histogram of Percent Use of Spanish Borrowings in English", xlab="Percent Use", ylab="Count", col='#31a35475',ylim=c(0,30),xlim=c(0, 1.0))
hist(heritage.df$nm.PercUsed,col='#756bb175',add=T)
hist(nonheritage.df$nm.PercUsed,col='#756bb175')
