# Load required packages
library(ggplot2)
library(RODBC)
library(stringr)
library(data.table)
library(plyr)
library(stringdist)
library(dplyr)
library(devtools)
library(htmlwidgets)
library(tidyr)
library(stringi)
library(googlesheets)
library(xlsx)


# Authenticate google
gs_auth(new_user = TRUE)

# Check available sheets
gs_ls()
available <- gs_ls()

# Bring in dataset
hl <- gs_title("Homeless Census Table")
hl <- gs_read(hl)

# Convert all "no answer" to NA
hl$Verification_Number[1:71] <- NA
hl[ hl == "no answer" | hl == "No answer" | hl == "no asnwer" | hl == "No asnwer" | hl == "No time" | hl == "no time" | hl == "No name"] <- NA
paper$Otherrace <- NA

# Change all interview to be spelled out
hl$O_or_I[ hl$O_or_I == "I"] <- "Interview"
hl$O_or_I[ hl$O_or_I == "O"] <- "Observation"

# Change all R to Refused
hl [ hl == "R" | hl == "refused"] <- "Refused"

# Change all N to No
hl [ hl == "N" | hl == "no" | hl == "n" | hl == "NO"] <- "No"

# Change all Y to Yes
hl [ hl == "Y" | hl == "yes"] <- "Yes"

# Change all M and F to Male/Female
hl [ hl == "M"] <- "Male"
hl [ hl == "F"] <- "Female"

# Change all DK to Don't know
hl [ hl == "DK" | hl == "don't know" | hl == "dont know" | hl == "Dont know" | hl == "Don't Know"] <- "Don't know"

# Fix ages to be <18, 18-24, 24-62, 62+
hl [ hl == "24+" | hl == "2462"] <- "24-62"
hl [ hl == "1824" ] <- "18-24"
hl [ hl == "62plus" | hl == "62+ with ?"] <- "62+"

hl$PhysicalDisability [ hl$PhysicalDisability == "Hypertension" |
                          hl$PhysicalDisability == "Extreme vision anxiety" |
                          hl$PhysicalDisability == "Yes, broken feet"] <- "Yes"

# Change others to all be the same
hl [ hl == "Other"] <- "other"

# Fix the sleep tonight to all be the same
hl [ hl == "choice0" | hl == "house or apartment" | hl == "hourse or apartment"] <- "House or Apartment"
hl [ hl == "choice1" | hl == "transitional housing"] <- "Transitional Housing"
hl [ hl == "choice2" | hl == "with friends or family" |
       hl == "with friends" | hl == "with friends and family" | hl == "With friends/family"] <- "With Friends or Family"
hl [ hl == "choice3" | hl == "hospital / treatment facility"] <- "Hospital / Treatment Facility"
hl [ hl == "choice4" | hl == "emergency shelter"] <- "Emergency Shelter"
hl [ hl == "choice5" | hl == "bus or train station"] <- "Bus or Train Station"
hl [ hl == "choice6" | hl == "motel / hotel"] <- "Motel / Hotel"
hl [ hl == "choice7" | hl == "street or sidewalk" | hl == "Street or sidewalk" | hl == "Street"] <- "Street or Sidewalk"
hl [ hl == "choice8" | hl == "park"] <- "Park"
hl [ hl == "choice9" | hl == "vehicle"] <- "Vehicle"
hl [ hl == "choice10" | hl == "under bridge / underpass"| hl == "under bridge/underpass"] <- "Under Bridge / Underpass"
hl [ hl == "choice11" | hl == "woods / encampment"] <- "Woods / Encampment"
hl [ hl == "choice12" | hl == "abandoned building"] <- "Abandoned Building"
# unique cases
hl$Sleep_Tonight [ hl$Sleep_Tonight == "No response, called 311" | hl$Sleep_Tonight == "didn't know yet"] <- NA
hl$Sleep_Tonight [ hl$Sleep_Tonight == "downtown crossing train station" | hl$Sleep_Tonight == "bus or transportation" |
                     hl$Sleep_Tonight == "bus or train station waiting for bus" | hl$Sleep_Tonight == "South Station" |
                     hl$Sleep_Tonight == "South Station Terminal"] <- "Bus or Train Station"
hl$Sleep_Tonight [ hl$Sleep_Tonight == "woods, encampment, park in tent" | hl$Sleep_Tonight == "encampment" |
                     hl$Sleep_Tonight == "Woods, encampment" | hl$Sleep_Tonight == "Woods, encampment (tent)"] <- "Woods / Encampment"
hl$Sleep_Tonight [ hl$Sleep_Tonight == "street or sidewalk at library" |
                     hl$Sleep_Tonight == "street or sidewalk they have not decided, with male friend"] <- "Street or Sidewalk"
hl$Sleep_Tonight [ hl$Sleep_Tonight == "under bridge/underpass, didn't want to speak to group"] <- "Under Bridge / Underpass"
hl$Sleep_Tonight [ hl$Sleep_Tonight == "Doesn't know" | hl$Sleep_Tonight == "where to go emergency shelter/bus or train station" |
                     hl$Sleep_Tonight == "it was unclear, options included street or sidewalk, emergency shelter in Kingston, or with friends and family (possible story with female friend)"|
                     hl$Sleep_Tonight == "No"] <- "Don't know"
hl$Sleep_Tonight [ hl$Sleep_Tonight == "transported with Pine Street van" | hl$Sleep_Tonight == "Transported to 112 Southampton Street" |
                     hl$Sleep_Tonight == "street or sidewalk (Called van to transport to 112 Southampton Street)" |
                     hl$Sleep_Tonight == "Transported to Pine Street"] <- "Emergency Shelter"
hl$Sleep_Tonight [ hl$Sleep_Tonight == "Other (hallway)"] <- "Abandoned Building"
hl$Sleep_Tonight [ hl$Sleep_Tonight == "Hospital (ER)" | hl$Sleep_Tonight == "Hospital (ED)"] <- "Hospital / Treatment Facility"

# Change team names to just be the number
hl$Team_Number <- gsub('Team', '', hl$Team_Number)

# Ride to shelter clean
hl$Ride_to_Shelter [ hl$Ride_to_Shelter == "no response" | hl$Ride_to_Shelter == "No, don't like shelters" |
                       hl$Ride_to_Shelter == "street or sidewalk, had not decided -- told by female companion"] <- "No"
hl$Ride_to_Shelter [ hl$Ride_to_Shelter == "Yes willing to go to emergency shelter" | hl$Ride_to_Shelter == "Yes (EMS transport)"] <- "Yes"

# Race cleanup
hl$Race [ hl$Race == "white" | hl$Race == "White,Refused"] <- "White"
hl$Race [ hl$Race == "African American" | hl$Race == "Black_/African_American,Don't_know" |
            hl$Race == "Black_/African_American"] <- "Black or African American"
hl$Race [ hl$Race == "Don't_know" | hl$Race == "?"] <- "Don't know"
hl$Race [ hl$Race == "American_Indian_/Alaskan_Native"] <- "American Indian / Alaskan Native"
hl$Race [ hl$Race == "American Indian or Alaskan Native or Black or African American"] <- "American Indian / Alaskan Native, Black or African American"
hl$Race [ hl$Race == "American_Indian_/Alaskan_Native,White"] <- "American Indian / Alaskan Native, White"
hl$Race [ hl$Race == "White and American Indian and Alaska Native"] <- "American Indian / Alaskan Native, White"

# move over the birthday
hl$Born <- ifelse(hl$Born == "other", hl$Date2, hl$Born)

# move over the where homeless
hl$Where_Homeless <- ifelse(hl$Where_Homeless == "other", hl$WhereHomeless, hl$Where_Homeless)

# move over permanent home
hl$LastPermanentHome <- ifelse(hl$LastPermanentHome == "other",
                               hl$DatePermanentHome, hl$LastPermanentHome)

# move over where living then
hl$WhereLivingThen <- ifelse(hl$WhereLivingThen == "other",
                             hl$wherelivingthen2, hl$WhereLivingThen)

# Create initial review code field
hl$initial_review_code <- NA

hl$initial_review_code [ hl$Interviewed_Already == "Yes"] <- 1
hl$initial_review_code [ hl$Sleep_Tonight == "With Friends or Family"] <- 3
hl$initial_review_code [ hl$Sleep_Tonight == "Emergency Shelter"] <- 2
hl$initial_review_code [ hl$Ride_to_Shelter == "Yes"] <- 5 
hl$initial_review_code [ hl$Sleep_Tonight == "House or Apartment"] <- 8

# Add in additional fields present in last year's version
hl$confirmedhomeless <- NA
hl$second_review <- NA
hl$notes <- NA
hl$meetscondition <- NA

# Create App review code
hl$app <- NA
hl$app [ !(is.na(hl$Verification_Number)) ] <- "Yes"
hl$app [ is.na(hl$Verification_Number) ] <- "No"


# Begin organizing table for export to google sheet
homeless.2019 <- data.table(hl$confirmedhomeless, 
                            hl$initial_review_code, 
                            hl$second_review,
                            hl$notes, 
                            hl$app, 
                            hl$Team_Number, 
                            hl$Time_,
                            hl$O_or_I,
                            hl$Location,
                            hl$Interviewed_Already,
                            hl$Sleep_Tonight,
                            hl$Ride_to_Shelter,
                            hl$Characteristics,
                            hl$Name,
                            hl$Name2,
                            hl$Born,
                            hl$Gender,
                            hl$othergender,
                            hl$age,
                            hl$Hispanic,
                            hl$Race,
                            hl$Otherrace,
                            hl$Where_Homeless,
                            hl$LastPermanentHome,
                            hl$DatePermanentHome,
                            hl$IdentifyAs,
                            hl$AdultNum,
                            hl$ChildNum,
                            hl$Initials,
                            hl$TimesonStreet,
                            hl$MonthsTotalHomeless,
                            hl$Military,
                            hl$Benefits,
                            hl$Alcohol,
                            hl$Drugs,
                            hl$HealthProblems,
                            hl$EmotionalMentalHealth,
                            hl$PhysicalDisability,
                            hl$DeveopmentalDisability,
                            hl$AIDS,
                            hl$Pregnant,
                            hl$lat,
                            hl$long)

colnames(homeless.2019) <- c('Confirmed Homeles (y/n)','Initial Review Code (see key)','Second Review (y/n/m)',
                             'Notes','App','Team Number','Time','Observation or Interview','Location',
                             'Interviewed Already','Sleeping Tonight',
                             'Ride to Shelter Requested','Identifying Characteristics',
                             'Name','Name2','Gender','Other Gender','Age','Hispanic','Race','Otherrace',
                             'Where did you become homeless?','')




#












