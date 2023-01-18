#Survival final project data cleaning and Table 1
#Meg Rosales

#packages
library(tidyr)
library(dplyr)
library(lubridate)
library(psych)
library(survival)

#change working directory
setwd("C:/Users/meg/Desktop/Courses/Spring 2020 Courses/Survival Analysis")

#read in the data
animals <- read.csv("animals.csv")

#start with N = 150,842

#get rid of unnecessary columns
#these are the columns relevant for this analysis
cols <- c("AnimalID", "AnimalType","IntakeDate","IntakeType","PrimaryColor","Gender","OutcomeDate","OutcomeType")
animals <- animals%>%
  select(cols)

##################################################
#animal ID

#convert to character
animals$AnimalID <- as.character(animals$AnimalID)

# #get rid of the A at the beginning of each ID
# #function that gets rid of A in a string and returns resulting string
# idfun <- function(x){
#   id <- gsub("A","",x)
#   return(id)
# }
# animals$AnimalID <- sapply(animals$AnimalID, idfun)
# #convert to numeric
# animals$AnimalID <- as.numeric(animals$AnimalID)

###################################################

#animal type
animals$AnimalType <- as.character(animals$AnimalType)

#only interested in dogs and cats
animals <- animals%>%
  filter(AnimalType == "CAT"|AnimalType == "DOG")

#N = 145,886

#use dogs as reference category
animals$dog <- ifelse(animals$AnimalType == 'DOG',1,0)
####################################################

#intake date and time
#will need to work with dates, split date and time down the middle
animals <- animals %>%
  separate(IntakeDate, c("IntakeDate","IntakeTime"), sep = " ")

#don't think I'll do anything with time

#months/days should suffice
#convert to date type with lubridate package
animals$IntakeDate <- ymd(animals$IntakeDate)

#####################################################

#intake type
animals$IntakeType <- as.character(animals$IntakeType)

#add variable for stray
animals$Stray <- ifelse(animals$IntakeType == "STRAY", 1, 0)

#####################################################
#primary color
animals$PrimaryColor <- as.character(animals$PrimaryColor)

#take only the first color
animals$PrimaryColor <- ifelse(grepl (' ', animals$PrimaryColor),
                               unlist(strsplit(animals$PrimaryColor, ' '))[1],
                               animals$PrimaryColor)

#recode chocolate, mahogany, and brindle as brown
animals$PrimaryColor[animals$PrimaryColor == "CHOCOLATE"|animals$PrimaryColor == "MAHOGANY"|
                       animals$PrimaryColor == "BRINDLE"] <- "BROWN"

#recode fawn and beige and cream as tan
animals$PrimaryColor[animals$PrimaryColor == "FAWN"|animals$PrimaryColor == "BEIGE"|
                       animals$PrimaryColor == "CREAM"] <- "TAN"

#summary of animal colors
colorcount <- animals%>%
  group_by(PrimaryColor)%>%
  summarize(count = n())%>%
  arrange(desc(count))

#top 5 are white, black, brown, gray, and tan
#consolidate all others into a category "other"
animals$PrimaryColor[!(animals$PrimaryColor%in%c("WHITE","BLACK","BROWN","GRAY","TAN"))] <- "OTHER"

table(animals$PrimaryColor)

# result of cleaning
# BLACK BROWN  GRAY OTHER   TAN WHITE 
# 41280 16621 10781 23250  9598 44356 
                                 
#####################################################

#gender
animals$Gender <- as.character(animals$Gender)

#not going to pay attention to spayed/neutered - just male, female, and unknown
#unknown becomes NA

animals$Gender[animals$Gender == "LITTER"|animals$Gender == "UNKNOWN"] <- "UNKNOWN"
animals$Gender[animals$Gender == "NEUTERED MALE"] <- "MALE"
animals$Gender[animals$Gender == "SPAYED FEMALE"] <- "FEMALE"

######################################################

#outcome date and time
#will need to work with dates, split date and time down the middle
animals$OutcomeDate <- as.character(animals$OutcomeDate)
animals <- animals %>%
  separate(OutcomeDate, c("OutcomeDate","OutcomeTime"), sep = " ")

#got warning for missing pieces in 198 rows - will have to drop
#these cases at it isn't reasonable to go back and examine each case individually

#convert outcome to date type
animals$OutcomeDate <- ymd(animals$OutcomeDate)

#198 cases of NA

########################################################

#outcome type
animals$OutcomeType <- as.character(animals$OutcomeType)

outcomecount <- animals%>%
  group_by(OutcomeType)%>%
  summarize(count = n())%>%
  arrange(desc(count))

#we want to examine what features are related to time to adoption
#so it makes sense that we would only want to look at animals for
#whom adoption/euthanization is a possibility

#there is unfortunately no data dictionary, so this represents my best interpretation of the data present

#I had to make sense earlier of how to combine categories, which is why there is all this work
#for just adopted vs. non-adopted

#don't count "no show" because that's basically missing data -> NA
#418 " " -> NA
animals$OutcomeType[animals$OutcomeType == ""|animals$OutcomeType == "NO SHOW"] <- NA
#can't say whether these are part of target population or not so will omit

#relocate/transport can go under "transfer"
animals$OutcomeType[animals$OutcomeType == "RELOCATE"|animals$OutcomeType == "TRANSPORT"] <- "TRANSFER"

#other = et process, indefinite, lost exp, missing, missing ex (members of the adopt/euth process with
#miscellaneous outcomes)
animals$OutcomeType[animals$OutcomeType == "ET PROCESS"|animals$OutcomeType == "INDEFINITE"|
                      animals$OutcomeType == "LOST EXP"|animals$OutcomeType == "MISSING"|
                      animals$OutcomeType == "MISSING EX"] <- "OTHER"

animals$OutcomeType[animals$OutcomeType == "ET PROCESS"] <- "OTHER"
animals$OutcomeType[animals$OutcomeType == "INDEFINITE"] <- "OTHER"

#snr, tnr, rtf (spay/trap/neuter release/return to field), release, released probably shouldn't be counted
#because those animals were not going to be up for adoption/euthanization

#exclude after doing the dates
animals <- animals%>%
  filter(!(OutcomeType%in%c("RTF","RTO","SNR","TNR","RELEASE","RELEASED")))

#N = 123,135
addmargins(table(animals$OutcomeType))

# ADOPTION     DIED DISPOSAL     EUTH   FOSTER    OTHER TRANSFER      Sum 
# 25117     2455     1700    68358     4574     1359    19107   122670 

#N = 123,135
#465 NAs as expected

########################################################
#create time to event variable

#definte interval between intake date and outcome date
tte_int <- animals$IntakeDate %--% animals$OutcomeDate

#time to event variable in months
animals$tte <- as.duration(tte_int)/ddays(1)

#filter out time to events < 0 because they don't make sense
animals <- animals%>%
  filter(tte >= 0)

#N = 122,897 - loss of 238 to data entry issues

#check tte variable
summary(animals$tte)

#the maximum value of 5849 seems feasible unless there is a typo on intake/outcome date
#checked and the dates look like they were intentional
#one dog was adopted after 16 years

#########################################################
#create censoring variable for adoption
animals$adopted <- ifelse(animals$OutcomeType=="ADOPTION",1,0)

#########################################################
#check number of missing values in each field
print(sapply(animals, function(x){sum(is.na(x))}))

#filter out NAs for outcome type/adopted
animals <- animals%>%drop_na(adopted)

#N = 122,621 (loss of 276 as expected)

#number of unique IDs
length(unique(animals$AnimalID))
#only 114,589 - 8032 duplicates

#handling shleter repeat animals
#subset the animals who have duplicate IDS
#take first instance of each ID
animalsf <- animals%>%
  group_by(AnimalID)%>%
  arrange(OutcomeDate)%>%
  slice(1)%>%
  ungroup()
View(animalsf)
#N = 114,589 - matches number of unique IDS

#################################################################################################
#Round 1: Datasets

#Considerations:
#unique IDs only
#complete data in variables of interest
#not using DOB (no age due to small number of cases)

#N = 114,589

#drop unnecessary variables
cols2 <- c("AnimalID", "AnimalType", "IntakeDate", "IntakeType", 
           "PrimaryColor", "Gender", "OutcomeDate", 
           "OutcomeType", "tte", "adopted", "Stray", "dog")

animalsf<- animalsf %>%
  select(cols2)

print(sapply(animalsf,function(x){sum(is.na(x))}))

#get rid of NAs just in case
animalsf<- drop_na(animalsf)

#N = 114,589

#turns out that I'll need ID number to generate the deviance residuals
#get rid of the A at the beginning of each ID
#function that gets rid of A in a string and returns resulting string
idfun <- function(x){
  id <- gsub("A","",x)
  return(id)
}
animalsf$AnimalID<-sapply(animalsf$AnimalID, idfun)
#convert to numeric
animalsf$AnimalID <- as.numeric(animalsf$AnimalID)

#save out the datafile to a csv
write.csv(animalsf,file = "animalsfinal.csv")
#####################################################################################################
#Table 1 Statistics

#subset only cats
cats <- animalsf %>%
  filter(dog == 0)

print("Table 1 Stats")

#stray
print("Stray")
stray_t <- table(cats$Stray)
addmargins(stray_t)
prop.table(stray_t)

#color
print("Color")
color_t <- table(cats$PrimaryColor)
addmargins(color_t)
prop.table(color_t)

#gender - female = 1, male = 0
print("Gender")
gen_t <- table(cats$Gender)
addmargins(gen_t)
prop.table(gen_t)

#outcome type
print("Outcome Type")
otype_t <- table(cats$OutcomeType)
addmargins(otype_t)
prop.table(otype_t)

#adopted
print("Adopted")
adopt_t <- table(cats$adopted)
addmargins(adopt_t)
prop.table(adopt_t)

#time to adoption
#had trouble with processing time using formedian estimates with SAS as in class, 
#so decided to use R for this part since it was Table 1
print("Time to adoption")
print(survfit(Surv(tte, adopted) ~ 1, data = cats), print.rmean = T)
summary(cats$tte)

#visual aid
plot(cats$tte)

