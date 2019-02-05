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
hl$Verification_Number <- NA
hl[ hl == "no answer" | hl == "No answer" | hl == "no asnwer" | hl == "No asnwer" | hl == "No time" | hl == "no time"] <- NA
paper$Otherrace <- NA

# Change all interview to be spelled out
hl$O_or_I[ hl$O_or_I == "I"] <- "Interview"
hl$O_or_I[ hl$O_or_I == "O"] <- "Observation"

# Change all R to Refused
hl [ hl == "R" | hl == "refused"] <- "Refused"

# Change all N to No
hl [ hl == "N" | hl == "no"] <- "No"

# Change all Y to Yes
hl [ hl == "Y" | hl == "yes"] <- "Yes"

# Change all M and F to Male/Female
hl [ hl == "M"] <- "Male"
hl [ hl == "F"] <- "Female"

# Change all DK to Don't know
hl [ hl == "DK" | hl == "don't know" | hl == "dont know" | hl == "Dont know" | hl == "Don't Know"]

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
       hl == "with friends"] <- "With Friends or Family"
hl [ hl == "choice3" | hl == "hospital / treatment facility"] <- "Hospital / Treatment Facility"
hl [ hl == "choice4" | hl == "emergency shelter"] <- "Emergency Shelter"
hl [ hl == "choice5" | hl == "bus or train station"] <- "Bus or Train Station"
hl [ hl == "choice6" | hl == "motel / hotel"] <- "Motel / Hotel"
hl [ hl == "choice7" | hl == "street or sidewalk" | hl == "Street or sidewalk"] <- "Street or Sidewalk"
hl [ hl == "choice8" | hl == "park"] <- "Park"
hl [ hl == "choice9" | hl == "vehicle"] <- "Vehicle"
hl [ hl == "choice10" | hl == "under bridge / underpass"| hl == "under bridge/underpass"] <- "Under Bridge / Underpass"
hl [ hl == "choice11" | hl == "woods / encampment"] <- "Woods / Encampment"
hl [ hl == "choice12" | hl == "abandoned building"] <- "Abandoned Building"















# Separate out the paper and app versions
paper.orig <- hl[1:71,]
paper <- paper.orig
app <- hl[73:211,]
app.orig <- app









# Coerce all "no answer" to one specific string through all columns
paper$Verification_Number <- NA
paper[ paper == "no answer" | paper == "No answer" | paper == "no asnwer" | paper == "No asnwer"] <- NA
paper$Otherrace <- NA
paper$O_or_I[ paper$O_or_I == "I"] <- "Interview"
paper$O_or_I[ paper$O_or_I == "O"] <- "Observation"
paper$Interviewed_Already[ paper$Interviewed_Already == "No" | paper$Interviewed_Already == "no"] <- "No"

paper$Military[ paper$Military == "no" | paper$Military == "No"] <- "No"
paper$Military[ paper$Military == "yes" | paper$Military == "Yes" | paper$Military == "Yes, Vietnam"] <- "Yes"
paper$Military[ paper$Military == "refused" | paper$Military == "Refused"] <- "Refused"
paper$Military[ paper$Military == "don't know" | paper$Military == "Don't know"] <- "No"

paper$Benefits[ paper$Benefits == "no" | paper$Benefits == "No"] <- "No"
paper$Benefits[ paper$Benefits == "yes" | paper$Benefits == "Yes" | paper$Benefits == "yes (none circled)" | paper$Benefits == "Yes (none circled)"] <- "Yes"
paper$Benefits[ paper$Benefits == "refused" | paper$Benefits == "Refused"] <- "Refused"
paper$Benefits[ paper$Benefits == "don't know" | paper$Benefits == "Don't know"] <- "No"








paper$Team_Number <- amatch(paper$Team_Number, "no time", maxDist = 20)




paper <- lapply(paper, amatch(paper,"no anser", maxDist = 20))
















