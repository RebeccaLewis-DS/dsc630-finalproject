#Rebecca Lews 
#Course Project Preliminary Analysis

# read in final prepped dataset
library(ggplot2)
library(dplyr)
library(scales)
library(funModeling)
library(Hmisc)
library(corrplot)


# set strings as factors to false
options(stringsAsFactors = FALSE)


#read in data
final_data <- read.csv('FinalDataset.csv', header=TRUE)
dog_data <- final_data %>% filter(Species == 'Dog') %>% droplevels()
cat_data <- final_data %>% filter(Species == 'Cat') %>% droplevels()

profiling_num(dog_data)
profiling_num(cat_data)

str(dog_data)
freq(data=dog_data, input='PrimaryBreed')
freq(data=dog_data, input='Gender')
freq(data=dog_data, input='IntakeDateOnly')
freq(data=dog_data, input='OutcomeDateOnly')
freq(data=dog_data, input='IntakeAsilomar')
freq(data=dog_data, input='IntakeCondition')
freq(data=dog_data, input='IntakeType')
freq(data=dog_data, input='OutcomeAsilomar')
freq(data=dog_data, input='OutcomeSubType')
freq(data=dog_data, input='Size')
freq(data=dog_data, input='IntakeLocation')
freq(data=dog_data, input='IntakeSubLocation')
freq(data=dog_data, input='PreAltered')
freq(data=dog_data, input='OutcomeLocation')
freq(data=dog_data, input='OutcomeSubLocation')

str(cat_data)
freq(data=cat_data, input='PrimaryBreed')
freq(data=cat_data, input='Gender')
freq(data=cat_data, input='IntakeDateOnly')
freq(data=cat_data, input='OutcomeDateOnly')
freq(data=cat_data, input='IntakeAsilomar')
freq(data=cat_data, input='IntakeCondition')
freq(data=cat_data, input='IntakeType')
freq(data=cat_data, input='OutcomeAsilomar')
freq(data=cat_data, input='OutcomeSubType')
freq(data=cat_data, input='Size')
freq(data=cat_data, input='IntakeLocation')
freq(data=cat_data, input='IntakeSubLocation')
freq(data=cat_data, input='PreAltered')
freq(data=cat_data, input='OutcomeLocation')
freq(data=cat_data, input='OutcomeSubLocation')


###########################################################################################################################################
#feature extraction and selection
#before split data into dogs and cats

#drop unneeded date fields - IntakeDate, OutcomeDate, IntakeDateOnly, OutcomeDateOnly, OutcomeYear, OutcomeMonth, Month
#remove intake subtype from overall feature set
#remove intake reason from overall feature set
#exclude length owned and Intake Agendcy Association from overall feature set
#remove found zip from overall feature set

#create month and weekday variables for outcome date and intake date
library(lubridate)

final_data$IntakeMonth <- month(final_data$IntakeDateOnly)
final_data$OutcomeMonth <- month(final_data$OutcomeDateOnly)
final_data$IntakeWeekday <- wday(final_data$IntakeDateOnly)
final_data$OutcomeWeekday <- wday(final_data$OutcomeDateOnly)

#log10 transform intake age in months
#check for 0 values
final_data %>% filter(IntakeAgeMonths == 0)
#none
final_data$Log10AgeInMonths <- log10(final_data$IntakeAgeMonths)

#calculate the length of stay from intake and outcome dates as a target variable
final_data$LengthOfStayDays <- as.numeric( difftime(final_data$OutcomeDateOnly, final_data$IntakeDateOnly, units = "days"))

#derive binary target of adoption in under 7 days
final_data$AdoptedInSevenDays <- ifelse(final_data$LengthOfStayDays <= 7, 1, 0)

#create a binary feature to indicate improvement in asilomar status
final_data$IntakeAsilomar <- as.factor(final_data$IntakeAsilomar)
final_data$OutcomeAsilomar <- as.factor(final_data$OutcomeAsilomar)

levels(final_data$IntakeAsilomar)
levels(final_data$OutcomeAsilomar)

#test improvement logic
final_data %>% filter(as.numeric(final_data$OutcomeAsilomar) < as.numeric(final_data$IntakeAsilomar)) %>%
  select(c('IntakeAsilomar','OutcomeAsilomar'))

final_data$AsilomarImprovement <- ifelse(as.numeric(final_data$OutcomeAsilomar) < as.numeric(final_data$IntakeAsilomar), 1, 0)


#one hot incode intake asilomar
final_data$HealthyAsilomar <- ifelse(final_data$IntakeAsilomar == 'Healthy', 1, 0)
final_data$TreatRehabAsilomar <- ifelse(final_data$IntakeAsilomar == 'Treatable-Rehabilitatable', 1,0)
final_data$TreatManAsilomar <- ifelse(final_data$IntakeAsilomar == 'Treatable-Manageable', 1, 0)
final_data$UnhealthyAsilomar <- ifelse(final_data$IntakeAsilomar == 'Unhealthy/Untreatable', 1, 0)

#derive features from intake condition for emaciated, feral, heartworm positive, combine sick with sick & injured and injured wounds, skin condition
final_data$Emaciated <- ifelse(final_data$IntakeCondition == 'Emaciated/Thin', 1, 0)
final_data$Feral <- ifelse(final_data$IntakeCondition == 'Feral', 1, 0)
final_data$Heartworm <- ifelse(final_data$IntakeCondition == 'Heartworm Positive', 1, 0)
final_data$SickInjured <- ifelse(final_data$IntakeCondition %in% c('Injured/woulds', 'Sick','Sick & Injured'), 1, 0)
final_data$SkinIssue <- ifelse(final_data$IntakeCondition == 'Skin Condition', 1, 0)

#one hot encode intake types
final_data$SurrenderIntake <- ifelse(final_data$IntakeType == 'Owner/Guardian Surrender', 1, 0)
final_data$SeizeCustodyIntake<- ifelse(final_data$IntakeType == 'Seized / Custody', 1, 0)
final_data$StrayIntake<- ifelse(final_data$IntakeType == 'Stray', 1, 0)
final_data$TransferIntake<- ifelse(final_data$IntakeType == 'Transfer In', 1, 0)

#create binary feature for small animals
final_data$SmallAnimal <- ifelse(final_data$Size == 'Small', 1, 0)

#create Prealtered binary variable
final_data$AlteredAtIntake <- ifelse(final_data$PreAltered == 'Y', 1, 0)

#foster and offsite flags
final_data$Foster <- ifelse(final_data$OutcomeSubType == 'Foster Home/ Fast Track', 1, 0)
final_data$Offsite <- ifelse(final_data$OutcomeSubType %in% c('No Fleas Market', 'Special Event') |
                            grepl('Off-Site', final_data$OutcomeSubType) | 
                            grepl('Offsite', final_data$OutcomeSubType), 1, 0)

#create multiclass target for Barn Cat, Foster, Louisiana SPCA Offsite, Veterinary Partner Offsite, Retail Location Offsite, and Onsite. 
final_data$AdoptionChannelGroup <- as.factor(ifelse(final_data$OutcomeSubType %in% c('Off-Site Jefferson Fee', 'Off-Site Petco',
                                                                               'Off-Site Petco Algiers','Off-Site PetSmart',
                                                                               'Off-site PetSmart Manhattan',
                                                                               'Offsite Petco Tchoup', 'Off-Site Petco Mid-City',
                                                                               'Off-Site Petco Harvey', 'Off-Site Petco Kenner'), 
                                                 'Retail Partner Offsite', 
                                                 ifelse(final_data$OutcomeSubType %in% c('Off-Site Cat Practice','Off-Site MidCity Vet.Clinic'),
                                                        'Veterinary Partner Offsite', 
                                                        ifelse(final_data$OutcomeSubType %in% c('No Fleas Market','Off-Site', 'Off-Site Clearview Volunteer',
                                                                                             'Special Event'), 'Louisiana SPCA Offsite', 
                                                               as.character(final_data$OutcomeSubType)))))


#Dog Features
#create a feature for Mix Specified based on values in primary breed.
final_data$DogMixed <- ifelse(grepl('Mix', final_data$PrimaryBreed), 1, 0)

#create a feature for PitBullType based on primary breed
final_data$DogPitBullType <- ifelse(final_data$PrimaryBreed %in% c('Terrier, Pit Bull', 'Terrier, American Pit Bull', 
                                                             'Bulldog, American', 'Terrier, American Staffordshire', 
                                                             'Terrier, Staffordshire Bull','Bullmastiff'), 1, 0)
#create encouragement room feature based on locations
final_data$DogEncouragement <- ifelse(grepl('Encourage', final_data$IntakeLocation) | 
                                     grepl('Encourage', final_data$IntakeSubLocation) | 
                                     grepl('Encourage', final_data$OutcomeLocation) |
                                     grepl('Encourage', final_data$OutcomeSubLocation), 1, 0)

#Cat Features
#create a feature for breed specified based on primary breed
final_data$CatBreedSpecified <- ifelse(grepl('Domestic', final_data$PrimaryBreed), 0,1)

#make ismale variable
final_data$Male <- ifelse(final_data$Gender == 'M', 1, 0)
final_data$Female <- ifelse(final_data$Gender == 'F', 1, 0)
#######################################################################################################
#create prepared dataset
prepared <- c('Species', 'IntakeMonth', 'OutcomeMonth', 'IntakeWeekday','OutcomeWeekday', 'Log10AgeInMonths', 'Male', 'Female',
              'AsilomarImprovement', 'HealthyAsilomar', 'TreatRehabAsilomar', 'TreatManAsilomar', 
              'UnhealthyAsilomar', 'Emaciated', 'Feral', 'Heartworm', 'SickInjured','SkinIssue',
              'SurrenderIntake', 'SeizeCustodyIntake', 'StrayIntake', 'TransferIntake','SmallAnimal',
              'AlteredAtIntake', 'Foster', 'Offsite', 'CatBreedSpecified', 'DogEncouragement', 'DogMixed',
              'DogPitBullType', 'LengthOfStayDays', 'AdoptedInSevenDays','AdoptionChannelGroup')

prepareddata <- final_data[prepared]

prepareddata$AdoptionChannelNum <- as.numeric(prepareddata$AdoptionChannelGroup)

#split between dog and cats
dogdata <- subset(prepareddata %>% filter(Species == 'Dog'), select=-c(CatBreedSpecified, Species, AdoptionChannelGroup))
catdata <- subset(prepareddata %>% filter(Species == 'Cat'), select=-c(DogEncouragement, DogMixed,DogPitBullType, Species, AdoptionChannelGroup))

#drop categorical channel column
#1 - Barn Cat    
#2 -Foster Home/ Fast Track     
#3 -Louisiana SPCA Offsite                     
#4 -Onsite     
#5 -Retail Partner Offsite 
#6 -Veterinary Partner Offsite 


######################################################################################
#plot newly transformed age feature
prepareddata %>% 
  ggplot(aes(x=Log10AgeInMonths)) +
  geom_histogram(aes(y=..density..), fill = 'white', color='black', binwidth = .25) +
  facet_wrap(~Species)+
labs(title='Log 10 Intake Age in Months Distribution', subtitle='For Adopted Animals') 

########################################################################################
#correlation plots
res <-rcorr(as.matrix(dogdata))
corrplot(round(res$r,4), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, title="Dog Feature and Target Correlation Plot", mar=c(0,0,1,0))


#after viewing the correlation plot, I noticed high negative correlation between male and female so I am removing one.
# I also noticed a high correlation between intake month and intake month so outcome month which makes sense because 
# we are only looking at a seven day period
#one hot encoded categorical variables had slight negative and positive correlations with each other but I would
#like to see how each contributes to the model before removing any

dogdata <- subset(dogdata, select=-c(Female, OutcomeMonth))

catres <-rcorr(as.matrix(catdata))

#heartworm created a null value in the correlation data, need to remove before plotting

catdata <- subset(catdata, select=-c(Heartworm))
catres <-rcorr(as.matrix(catdata))
corrplot(round(catres$r,4), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, title="Cat Feature and Target Correlation Plot", mar=c(0,0,1,0))


#making same changes as dogs: removing female and outcomemonth
#interesting note in that the unhealthy asilomar is correlated with feral status

catdata <- subset(catdata, select=-c(Female, OutcomeMonth))

#save catdata and dogdata
write.csv(catdata, 'catdata.csv', row.names = FALSE)
write.csv(dogdata, 'dogdata.csv', row.names = FALSE)

########################################################################################
#view details on target variables
#LEngth of stay days
ggplot(dogdata, aes(LengthOfStayDays)) +
  geom_histogram(aes(y=..density..),fill = 'white', color='black', binwidth = 10) +
  labs(title='Dog - Length of Stay in Days Distribution', subtitle='For Adopted Animals')  

summary(dogdata$LengthOfStayDays)

dim(dogdata %>% filter(LengthOfStayDays > 29))
dim(dogdata)


ggplot(catdata, aes(LengthOfStayDays)) +
  geom_histogram(aes(y=..density..),fill = 'white', color='black', binwidth = 10) +
  labs(title='Cat - Length of Stay in Days Distribution', subtitle='For Adopted Animals')  

summary(catdata$LengthOfStayDays)

#Adopted in seven days
prepareddata %>% group_by(Species, AdoptedInSevenDays) %>% tally() %>% mutate(pct=n/sum(n)) %>%
  ggplot(aes(x=as.factor(AdoptedInSevenDays), y=n, fill=as.factor(AdoptedInSevenDays))) +
  geom_bar(stat='Identity') +
  facet_wrap(~Species) +
  geom_text(aes(label = scales::percent(pct)), position = position_stack(vjust= .5), color="white") +
  labs(title='Adopted in Seven Days', subtitle='For Adopted Animals') +
  scale_x_discrete(labels = c('No', 'Yes')) +
  scale_fill_discrete(name = "Adopted in Seven Days", labels = c("No", "Yes")) +
  theme(axis.title.x = element_blank())


#Target classes are highly imbalanced
summary(as.factor(dogdata$AdoptedInSevenDays))

summary(as.factor(catdata$AdoptedInSevenDays))

#Adoption Channel
freq(prepareddata %>% filter(Species =='Dog') %>% select(AdoptionChannelGroup))

freq(prepareddata %>% filter(Species =='Cat') %>% select(AdoptionChannelGroup))

cross_plot(data = prepareddata, target = 'LengthOfStayDays')


#Adoption Channel
prepareddata %>% group_by(Species, AdoptionChannelGroup) %>% tally() %>% mutate(pct=n/sum(n)) %>%
  ggplot(aes(x=as.factor(AdoptionChannelGroup), y=n, fill=as.factor(AdoptionChannelGroup))) +
  geom_bar(stat='Identity') +
  facet_wrap(~Species) +
  geom_text(aes(label = scales::percent(pct)), position = position_stack(vjust= .5), color="black") +
  labs(title='Adoption Channels by Species', subtitle='For Adopted Animals', fill = 'Adoption Channels') +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
