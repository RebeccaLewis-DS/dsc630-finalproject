#Rebecca Lews 
#Course Project EDA

#############################################################################################################################################
# get overall statistics for presentation
#I want to see number of intakes by year vs number of adoptions by year

library(ggplot2)
library(dplyr)
library(scales)

# set strings as factors to false
options(stringsAsFactors = FALSE)

datapath <- 'C:/Users/rvick/OneDrive/Bellevue/DSC 630/Course Project'

#read in data
AllIntakeFile <- file.path(datapath, 'IntakeData.csv')
AllIntakes <- read.csv(AllIntakeFile, header=TRUE)

AdoptedIntakeFile <- file.path(datapath, 'AllIntakeWithResults.csv')
AdoptedIntakes <- read.csv(AdoptedIntakeFile, header=TRUE)

EuthIntakeFile <- file.path(datapath, 'EuthData.csv')
EuthIntakes <- read.csv(EuthIntakeFile, header=TRUE)

AllIntakes$Adopted <- AllIntakes$Animal.ID %in% AdoptedIntakes$Animal..
AllIntakes$Euthanized <- AllIntakes$Animal.ID %in% EuthIntakes$Animal..

AllIntakes$IntakeYear <- year(as.Date(AllIntakes$Intake.Date.Time))

IntakeStats = AllIntakes[c('Animal.ID', 'IntakeYear', 'Adopted', 'Euthanized')]

IntakeStats$Outcome <- ifelse(IntakeStats$Euthanized == TRUE, 'Euthanized', ifelse(IntakeStats$Adopted == TRUE, 'Adopted', 'Other'))

IntakeStats

ggplot(IntakeStats %>% group_by(IntakeYear) %>% count(Outcome) %>%
         mutate(pct=n/sum(n)), aes(x=IntakeYear, y=n, fill=Outcome)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label = scales::percent(pct)), position = position_stack(vjust= .5), color="white") +
  labs(title = "Animal Intakes for 2015-2019 With Outcomes", 
       caption = "**Other Outcomes include Transfer Out, Return to Owner, Wildlife Release, DOA, etc.") +
  xlab('Intake Year') +
  ylab('Intake Count') +
  scale_fill_manual(name='Outcome', labels=c('Adopted','Euthanized','Other**'), values=c("#008000", "#000080", '#dfaf37'))

ggsave('IntakesThatWereAdopted.png')

#############################################################################################################################################

#Explore individual variables and relationships between variables
options(stringsAsFactors = TRUE)

clean_data <- read.csv(file.path(datapath,'CleanData.csv'), header=TRUE)
clean_data$FoundZip <- as.factor(clean_data$FoundZip)

dim(clean_data)

str(clean_data)

summary(clean_data)

############################################################################################################################################
#further data cleanup
#DONE - remove initial stage - most records are new arrival
#DONE - times adopted can be removed as this was just used to filter the data
#DONE - length owned must be converted to the same units

#first - drop columns that are not needed
newdata <- subset(clean_data, select=-c(TimesAdopted, InitialStage))

#convert length owned to days and drop units column
newdata$LengthOwnedDays <- ifelse(newdata$LengthOwnedUnits == 'Months', newdata$LengthOwned * 30.417, 
                                  ifelse(newdata$LengthOwnedUnits == 'Weeks', newdata$LengthOwned * 7, 
                                         ifelse(newdata$LengthOwnedUnits == 'Years', newdata$LengthOwned * 365, newdata$LengthOwned)))

newdata <- subset(newdata, select=-c(LengthOwned, LengthOwnedUnits))

#view records where age in months at intake is less than 2 months
dim(newdata %>% 
  filter(IntakeAgeMonths < 2))

newdata <- newdata %>% filter(IntakeAgeMonths >= 2)

dim(newdata)

###########################################################################################################################################
#variable relationships

#age group, intake age and outcome age will be highly correlated.  need to decide which to keep
newdata %>%
  ggplot(aes(x=IntakeAgeMonths, y=OutcomeAgeMonths, color=AgeGroup)) +
  geom_point() + geom_jitter() +
  labs(title='Intake Age, Outcome Age, and Age Group Correlation') +
  xlab('Intake Age in Months') + ylab('Outcome Age in Months')

#removing dob, outcomeagemonths and agegroup due to high correlation
newdata <- subset(newdata, select=-c(DOB, OutcomeAgeMonths, AgeGroup))
str(newdata)
#------------------------------------------------------------------------------------------------

#explore correlation between intakeasilomar and intake condition.
newdata %>% 
  ggplot(aes(x=IntakeCondition, fill=IntakeCondition)) +
  geom_bar(stat='Count') +
  facet_wrap(~IntakeAsilomar) +
  labs(title='Intake Condition by Intake Asilomar Status', fill='Intake Condition') +
  theme(axis.text.x = element_blank())

#view more detail about outliers 
dim(newdata %>% filter(!(IntakeAsilomar %in% c('','Unassigned')))) 

#eliminate outliers
newdata <- newdata %>% filter(!(IntakeAsilomar %in% c('','Unassigned'))) %>% droplevels()
dim(newdata)

newdata %>% 
  ggplot(aes(x=IntakeCondition, fill=IntakeCondition)) +
  geom_bar(stat='Count') +
  facet_wrap(~IntakeAsilomar) +
  labs(title='Intake Condition by Intake Asilomar Status', subtitle='For Adopted Animals', fill='Intake Condition') +
  theme(axis.text.x = element_blank())



#----------------------------------------------------------------------------------------------------------------------------
#view asilomar at intake and outcome - see if animals wer rehabilitated and their status changed before outcome
#- may warrant a feature to indicate asilomar improvement or deterioration before adoption

newdata %>% 
  ggplot(aes(x=OutcomeAsilomar, fill=OutcomeAsilomar)) +
  geom_bar(stat='Count') +
  facet_wrap(~IntakeAsilomar) +
  labs(title='Outcome Asilomar Status by Intake Asilomar Status', subtitle='For Adopted Animals', fill='Outcome Asilomar') +
  theme(axis.text.x = element_blank())
#----------------------------------------------------------------------------------------------------------------------------

#view visual of intake type and subtype as the fill for adopted animals - this will need to be one hot encoded if used
newdata %>% 
  ggplot(aes(x=IntakeSubType, fill=IntakeType)) +
  geom_bar(stat='Count') +
  labs(title='Intake SubType Distribution', subtitle='For Adopted Animals', fill='Intake Typee') +
  facet_wrap(~IntakeSubType) +
  theme(axis.text.x = element_blank())

#too much to look at together

#intake Type
newdata %>% 
  ggplot(aes(x=IntakeType)) +
  geom_bar(stat='Count') +
  labs(title='Intake Type', subtitle='For Adopted Animals') 

#there should be no returns in the data - going to exclude returned records
dim(newdata %>% filter(IntakeType == 'Return'))
#only 7 records, removing
newdata <- newdata %>% filter(IntakeType != 'Return')
dim(newdata)

newdata %>% 
  ggplot(aes(x=IntakeType)) +
  geom_bar(stat='Count') +
  labs(title='Intake Type', subtitle='For Adopted Animals') 


#view intake subtype
newdata %>% 
  ggplot(aes(x=IntakeSubType, fill=IntakeSubType)) +
  geom_bar(stat='Count') +
  labs(title='Intake SubType', subtitle='For Adopted Animals')

#view counts for each category
intake_groups <- newdata %>% group_by(IntakeType, IntakeSubType) %>% tally()
intake_groups

intake_groups %>% filter(IntakeType=='Owner/Guardian Surrender') %>%
  ggplot(aes(x=IntakeSubType, y=n, fill=IntakeSubType)) +
  geom_bar(stat="Identity") +
  labs(title='Owner Surrender Subtype Distribution', subtitle='For Adopted Animals', fill='Intake SubType') +
  theme(axis.text.x = element_blank())

intake_groups %>% filter(IntakeType=='Seized / Custody') %>%
  ggplot(aes(x=IntakeSubType, y=n, fill=IntakeSubType)) +
  geom_bar(stat="Identity") +
  labs(title='Seized/Custody Subtype Distribution', subtitle='For Adopted Animals', fill='Intake SubType') +
  theme(axis.text.x = element_blank())

intake_groups %>% filter(IntakeType=='Stray') %>%
  ggplot(aes(x=IntakeSubType, y=n, fill=IntakeSubType)) +
  geom_bar(stat="Identity") +
  labs(title='Stray Subtype Distribution', subtitle='For Adopted Animals', fill='Intake SubType') +
  theme(axis.text.x = element_blank())

intake_groups %>% filter(IntakeType=='Transfer In') %>%
  ggplot(aes(x=IntakeSubType, y=n, fill=IntakeSubType)) +
  geom_bar(stat="Identity") +
  labs(title='Transfer In Subtype Distribution', subtitle='For Adopted Animals', fill='Intake SubType') +
  theme(axis.text.x = element_blank())


newdata %>% 
  ggplot(aes(x=IntakeType, fill=IntakeType)) +
  geom_bar(stat='Count') +
  labs(title='Intake Type', subtitle='For Adopted Animals', fill='Intake Type')  +
  theme(axis.text.x = element_text(angle = 45, vjust = .75))

#--------------------------------------------------------------------------------------------------------------------------------------
#explore intake reason
#view visual of intake reason with intake type as the fill

newdata %>% filter(IntakeReason != '') %>%
  ggplot(aes(x=IntakeReason, fill=IntakeReason)) +
  geom_bar(stat='Count') +
  facet_wrap(~IntakeType) +
  labs(title='Intake Reason', subtitle='For Adopted Animals', fill='Intake Reason')  +
  theme(axis.text.x = element_blank())

#--------------------------------------------------------------------------------------------------------------------------------------


#view visual of IntakeAgencyAssociation with intake type as the fill
newdata %>% filter(IntakeAgencyAssociation != '') %>%
  ggplot(aes(x=IntakeAgencyAssociation, fill=IntakeAgencyAssociation)) +
  geom_bar(stat='Count') +
  facet_wrap(~IntakeType) +
  labs(title='Intake Agency Association', subtitle='For Adopted Animals', fill='Intake Agency')  +
  theme(axis.text.x = element_blank())


#there is an odd spike for seized/custody this coud possibly be a hoarding case or coudl be the Louisiana SPCA
newdata %>% filter(IntakeType == 'Seized / Custody') %>% group_by(IntakeAgencyAssociation) %>% tally()

#they are all the louisiana spca for seized.  So this field is only relevant for transfer in.

newdata %>% filter((IntakeAgencyAssociation != '') & (IntakeType == 'Transfer In')) %>%
  ggplot(aes(x=IntakeAgencyAssociation, fill=IntakeAgencyAssociation)) +
  geom_bar(stat='Count') +
  facet_wrap(~IntakeType) +
  labs(title='Intake Agency Association', subtitle='For Adopted Animals', fill='Intake Agency')  +
  theme(axis.text.x = element_blank())

#--------------------------------------------------------------------------------------------------------

#View outcome subtype with outcome asilomar as a fill to see if certain types are correlated with ratings

newdata %>% 
  ggplot(aes(x=OutcomeSubType, fill=OutcomeSubType)) +
  geom_bar(stat='Count') +
  labs(title='Outcome SubType', subtitle='For Adopted Animals', fill='Outcome Subtype')  +
  theme(axis.text.x = element_blank())

#this field tells us some more information on how the animal was adopted
#i don't think one hot encoding would be helpful however, the values can be
#binned into a few different features.  
#  1. Offsite - indicates whether the animal was adopted from an offsite location.
#     All offsite locations as well as no fleas, and special event will be grouped here
#  2. Barn Cat - the fact an cat is a barn cat should contribute to length of stay in
#     the shelter because it is likely feral.  There may be other fields that can tell us if
#     an animal is a barn cat besides this, such as initial stage
#  3. Foster Home/Fast Track - create a flag to indicate if the animal was adopted from a foster home
#     I anticipate these values to have longer length of styas because they tend to be our harder to place animals.

#look at asilomar status and outcome subtype
newdata %>%
  ggplot(aes(x=OutcomeAsilomar, fill=OutcomeSubType)) +
  geom_bar(stat='Count') +
  labs(title='Outcome Asilomar by Outcome SubType', subtitle='For Adopted Animals', fill='Outcome Subtype')  +
  theme(axis.text.x = element_text(angle=90, vjust=.5))

newdata$OutcomeGroup <- ifelse(!(newdata$OutcomeSubType %in% c('Barn Cat', 'Foster Home/ Fast Track', 'Onsite')), 'OffSite', as.character(newdata$OutcomeSubType))
newdata %>%
  ggplot(aes(x=OutcomeAsilomar, fill=OutcomeGroup)) +
  geom_bar(stat='Count') +
  labs(title='Outcome Asilomar by Outcome Group', subtitle='For Adopted Animals', fill='Outcome Group')  +
  theme(axis.text.x = element_text(angle=90, vjust=.5))

#------VIew primary and secondary breed for dogs and cats separately-----------------------------------------------------------------------

dogdata <- newdata %>% filter(Species == 'Dog') %>% droplevels()
catdata <- newdata %>% filter(Species == 'Cat') %>% droplevels()

mixdogs <- dogdata %>% filter(grepl('Mix', PrimaryBreed)) %>% droplevels()

# evaluate primary breed
summary(mixdogs$PrimaryBreed)

summary(catdata$PrimaryBreed)

str(dogdata)
summary(dogdata$PrimaryBreed)

#evaluate secondarybreed
summary(dogdata$SecondaryBreed)

summary(catdata$SecondaryBreed)

domcats <- catdata %>% filter(grepl('domestic', SecondaryBreed)) %>% droplevels()

summary(domcats$SecondaryBreed)
str(catdata)

#----------Gender Distributions------------------------------------------------------------------------------------------------------------

newdata %>% 
  ggplot(aes(x=Gender, fill=Species)) +
  geom_bar(stat='Count') +
  labs(title='Gender by Species', subtitle='For Adopted Animals', fill='Species')  

summary(dogdata$Gender)

summary(catdata$Gender)

# one unknown gender in cat observations - removing
newdata <- newdata %>% filter(Gender != 'U')

newdata %>% 
  ggplot(aes(x=IntakeAgeMonths)) +
  geom_histogram(aes(y=..density..), fill = 'white', color='black', binwidth = 10) +
  facet_wrap(~Species) +
  labs(title='Intake Age in Months Distribution', subtitle='For Adopted Animals')  



#----INtake Date and Outcome Date ------------------------------------------------------------------------------------------------------#

library(lubridate)
  
newdata$IntakeDateOnly <- as.Date(newdata$IntakeDateOnly, '%Y-%m-%d')
newdata$OutcomeDateOnly <- as.Date(newdata$OutcomeDateOnly, '%Y-%m-%d')

#need to create month and year values for dates for better evaluation
newdata$IntakeYear = year(newdata$IntakeDateOnly)
newdata$IntakeMonth = month(newdata$IntakeDateOnly)
newdata$OutcomeYear = year(newdata$OutcomeDateOnly)
newdata$OutcomeMonth = month(newdata$OutcomeDateOnly)

IntakeTimeData <- newdata %>%
  group_by(IntakeYear, IntakeMonth) %>%
  tally(name='count') 

head(IntakeTimeData, 50)


ggplot(newdata, aes(x=IntakeMonth)) + 
     geom_line(stat="count") + 
     facet_wrap(~IntakeYear) +
     scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12), limits = c(1,12)) +
     labs(title = 'Intakes by Month', 'For Adopted Animals')

IntakeTimeData$IntakeYear <- factor(IntakeTimeData$IntakeYear)


ggplot(data = IntakeTimeData, aes(x=IntakeMonth, y=count, color=IntakeYear, group=IntakeYear)) + 
  geom_line() + 
  geom_point() +
  #facet_wrap(~IntakeYear) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12), limits = c(1,12)) +
  labs(title = 'Intakes by Month by Year', subtitle= 'For Adopted Animals', color='Intake Year', x='Intake Month')

newdata %>%
  ggplot(aes(x=IntakeMonth, fill=factor(IntakeYear))) +
  geom_bar(stat='count') +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12), limits = c(0,13)) +
  labs(title = 'Intakes by Month All Years Combined', subtitle= 'For Adopted Animals', fill='Intake Year', x='Intake Month')


#-----Adoption Dates/Outcome date-------------------------------------------------------------------------

OutcomeTimeData <- newdata %>%
  group_by(OutcomeYear, OutcomeMonth) %>%
  tally(name='count') 

OutcomeTimeData


ggplot(newdata %>% filter(OutcomeYear !='2020'), aes(x=OutcomeMonth)) + 
  geom_line(stat="count", color='blue') + 
  geom_line(aes(x=IntakeMonth), stat='Count', color='red') +
  facet_wrap(~OutcomeYear) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12), limits = c(1,12)) +
  labs(title = 'Adoptions vs. Intake by Month', subtitle='Blue = Adoptions; Red = Intake', legend=c('Adoptions', 'Intake'))

IntakeTimeData$IntakeYear <- factor(IntakeTimeData$IntakeYear)


ggplot(data = IntakeTimeData, aes(x=IntakeMonth, y=count, color=IntakeYear, group=IntakeYear)) + 
  geom_line() + 
  geom_point() +
  #facet_wrap(~IntakeYear) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12), limits = c(1,12)) +
  labs(title = 'Intakes by Month by Year', subtitle= 'For Adopted Animals', color='Intake Year', x='Intake Month')

newdata %>%
  ggplot(aes(x=IntakeMonth, fill=factor(IntakeYear))) +
  geom_bar(stat='count') +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12), limits = c(0,13)) +
  labs(title = 'Intakes by Month All Years Combined', subtitle= 'For Adopted Animals', fill='Intake Year', x='Intake Month')

#--View intake condition by cats and dogs overall-----------------------------------------------------------

newdata %>% 
  ggplot(aes(x=IntakeCondition, fill=Species)) +
  geom_bar(stat='Count') +
  labs(title='Intake Condition by Species', subtitle='For Adopted Animals', fill='Species') +
  theme(axis.text.x = element_text(angle = 45, vjust = .75))

#------View intake type by species--------------------------------------------------------------------

newdata %>% 
  ggplot(aes(x=IntakeType, fill=IntakeType)) +
  geom_bar(stat='Count') +
  labs(title='Intake Type Distribution by Species', subtitle='For Adopted Animals', fill='Intake Typee') +
  facet_wrap(~Species) +
  theme(axis.text.x = element_blank())

#---------------View unique zip code values------------------------------------------------------------
summary(newdata$FoundZip)

#see how many animals are found in new orleans zips
str(newdata %>% filter(substr(as.character(FoundZip), 1, 3) =='701'))

str(newdata %>% filter(substr(as.character(FoundZip), 1, 3) !='701'))

#----Size distribution---------------------------------------------------------------------------------

newdata %>% 
  ggplot(aes(x=Size, fill=Size)) +
  geom_bar(stat='Count') +
  labs(title='Size by Species', subtitle='For Adopted Animals', fill='Size') +
  facet_wrap(~Species) +
  theme(axis.text.x = element_blank())

#-------Prealtered Distribution------------------------------------------------------------------------

newdata %>% 
  ggplot(aes(x=PreAltered, fill=PreAltered)) +
  geom_bar(stat='Count') +
  labs(title='PreAltered by Species', subtitle='For Adopted Animals', fill='PreAltered') +
  facet_wrap(~Species) +
  theme(axis.text.x = element_blank())

#count number of unknown
str(newdata %>% filter(PreAltered == 'U'))

#----Outcome Subtype-----------------------------------------------------------------------------------

newdata %>% 
  ggplot(aes(x=OutcomeSubType, fill=OutcomeSubType)) +
  geom_bar(stat='Count') +
  labs(title='OutcomeSubType by Species', subtitle='For Adopted Animals', fill='OutcomeSubType') +
  facet_wrap(~Species) +
  theme(axis.text.x = element_blank())

#----Outcome Location-----------------------------------------------------------------------------------
newdata %>% 
  ggplot(aes(x=OutcomeLocation, fill=OutcomeLocation)) +
  geom_bar(stat='Count') +
  labs(title='Outcome Location by Species', subtitle='For Adopted Animals', fill='Outcome Location') +
  facet_wrap(~Species) +
  theme(axis.text.x = element_blank())

newdata %>% 
  ggplot(aes(x=OutcomeSubLocation, fill=OutcomeSubLocation)) +
  geom_bar(stat='Count') +
  labs(title='Outcome Sub Location by Species', subtitle='For Adopted Animals', fill='Outcome Sub Location') +
  facet_wrap(~Species) +
  theme(axis.text.x = element_blank())

newdata %>% 
  ggplot(aes(x=IntakeLocation, fill=IntakeLocation)) +
  geom_bar(stat='Count') +
  labs(title='Intake Location by Species', subtitle='For Adopted Animals', fill='Intake Location') +
  facet_wrap(~Species) +
  theme(axis.text.x = element_blank())

newdata %>% 
  ggplot(aes(x=IntakeSubLocation, fill=IntakeSubLocation)) +
  geom_bar(stat='Count') +
  labs(title='Intake Sub Location by Species', subtitle='For Adopted Animals', fill='Intake Sub Location') +
  facet_wrap(~Species) +
  theme(axis.text.x = element_blank())

#see if cats go to the encouragement room
dim(newdata %>% filter(Species == 'Cat' & (grepl('Encourage', IntakeLocation) | 
                                                   grepl('Encourage', IntakeSubLocation) | 
                                                   grepl('Encourage', OutcomeLocation) |
                                                   grepl('Encourage', OutcomeSubLocation))))


##########################################################################################################################################
#refine working dataset and save

final_columns <-c('AnimalID','Species','PrimaryBreed','Gender', 'IntakeDateOnly', 'OutcomeDateOnly', 
                    'IntakeAgeMonths', 'IntakeAsilomar', 'IntakeCondition', 'IntakeType', 'OutcomeAsilomar', 
                    'OutcomeSubType', 'Size', 'IntakeLocation','IntakeSubLocation', 'PreAltered', 'OutcomeLocation', 
                    'OutcomeSubLocation','OutcomeGroup')

final_data <- newdata[final_columns]

write.csv(final_data, "FinalDataset.csv", row.names = FALSE)
