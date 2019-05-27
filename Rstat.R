if(!require(lsr)){install.packages("lsr")}

library(tidyverse)
library(plyr)
library (dplyr)

Q1a <- EAI.with.Axapta.and.web.services %>% select(2)
class(Q1a)

str(Q1a)
Q1aa <- as.matrix(Q1a)
class(Q1aa)

VFamiliar <- filter(Q1a, (Q1a %>% select(1)=="Very familiar"))

Q3 <- EAI.with.Axapta.and.web.services %>% select(6)

Q20 <- EAI.with.Axapta.and.web.services %>% select(26)

Q3Q20 <- EAI.with.Axapta.and.web.services %>% select(6, 26)

library(rcompanion)

t.test ( ~ , var.equal=TRUE, data = Q3.20)



dt <- as.table(as.matrix(Q3.20))
install.packages("gplots")
library ("gplots")
companyIntegration <- Q3.20 %>% select(1)

balloonplot(t(dt), main ="companyIntegration", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)


# Import the data
file_path <- "https://raw.githubusercontent.com/Esther96/RData/master/housetasks.txt"
barrier <- read.delim(file_path, row.names = 1)
# head(housetasks)
# 1. convert the data as a table
dt <- as.table(as.matrix(barrier))
# 2. Graph
balloonplot(t(dt), main ="barrier", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)


file_path <- "https://raw.githubusercontent.com/Esther96/RData/master/Q3-20.txt"
housetasks <- read.delim(file_path, row.names = 1)
# head(housetasks)
library("gplots")
# 1. convert the data as a table
dt <- as.table(as.matrix(housetasks))
# 2. Graph
balloonplot(t(dt), main ="Company integration barrier consideration ", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)


file_path <- "https://raw.githubusercontent.com/Esther96/RData/master/Q3-20aa.txt"
q7q11 <- read.delim(file_path, row.names = 2)
q7q11a <- q7q11 %>%select(1,2)

attach(q7q11)
t.test(Satisfaction.on.Axapta.integration, mu = 4)
t.test(Satisfaction.on.Axapta.integration ~ Experience )

dt <- as.table(as.matrix(q7q11a))
# 2. Graph
balloonplot(t(dt), main ="barrier", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

chisq.test(table(Experience, Satisfaction.on.Axapta.integration))

wilcox.test(Experience ~ Satisfaction.on.Axapta.integration)

fisher.test(table(Experience, Satisfaction.on.Axapta.integration))

fisher.test(table(Experience, Satisfaction.on.Axapta.integration))


# Simulated data
 cand = c(rep("Stewart", 22), rep("Macrander", 21), rep("Miller", 7))
 party=c(rep("Republican", 20), rep("Democratic", 23), rep("None", 7))

 print(table(cand, party))
 
 print(chisq.test(table(cand, party), simulate.p.value = T))
 
 ##### Demonstrating the Chi-Squared Test of Independence in R
 ##### By Eric Cai
 ##### The Chemical Statistician
 
 # Entering the data into vectors
 men = c(100, 120, 60)
 women = c(350, 200, 90)
 
 # combining the row vectors in matrices, then converting the matrix into a data frame
 ice.cream.survey = as.data.frame(rbind(men, women))
 
 # assigning column names to this data frame
 names(ice.cream.survey) = c('chocolate', 'vanilla', 'strawberry')
 
 chisq.test(ice.cream.survey)
 
years1.3 = c(8,6,55)
years0.1 = c(8,0,8)
years3.6 = c(12,0,12)
years.6plus = c(16, 0, 39)

experience.type.satisfaction = as.data.frame(rbind(years1.3, years0.1, years3.6, years.6plus ))

names(experience.type.satisfaction) = c('Desktop application', "null", "Web application")
chisq.test(experience.type.satisfaction)


intra.organizational = c(6, 6, 3, 6)
both = c(16, 24, 21, 22)
extra.organizational = c(3,9,7,0)

integration.type.benefits = as.data.frame(rbind(intra.organizational, both, extra.organizational))

names (integration.type.benefits) = c('Streamlined business processes', 'Productivity', 'Communication', 'Data accuracy')

chisq.test(integration.type.benefits)


file_path <- "https://raw.githubusercontent.com/Esther96/RData/master/integrationdetailed.txt"
housetasks <- read.delim(file_path, row.names = NULL)
# head(housetasks)
library("gplots")
# 1. convert the data as a table
dt <- as.table(as.matrix(integration.type.benefits))
# 2. Graph
balloonplot(t(dt), main ="Company integration barrier consideration ", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

t.test (Type.of.integration ~ Data.accuracy, var.equal=TRUE, data = housetasks)
wilcox.test (Type.of.integration ~ Data.accuracy, var.equal=TRUE, data = housetasks)
fisher.test (table(Type.of.integration ~ Data.accuracy))


scatter.smooth(x=housetasks$Type.of.integration, y=housetasks$Productivity, main="Type.of.integration ~ Productivity ")  # scatterplot
warnings()




 