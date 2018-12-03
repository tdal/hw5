
library(tidyverse)
library(ROCR)
library(lubridate)
library(randomForest)
library(doParallel)
library(foreach)
library(dplyr)

 

##Importing the data
restaurants.data <- read_csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv")


##i. Drop the following columns: BUILDING, STREET, PHONE, DBA,
##ZIPCODE, RECORD DATE, VIOLATION DESCRIPTION, GRADE DATE

restaurants.data$BUILDING <- NULL
restaurants.data$STREET <- NULL
restaurants.data$PHONE <- NULL
restaurants.data$DBA <- NULL
restaurants.data$ZIPCODE <- NULL
restaurants.data$`RECORD DATE` <- NULL
restaurants.data$`VIOLATION DESCRIPTION` <- NULL
restaurants.data$`GRADE DATE` <- NULL


##ii. Make INSPECTION DATE a date object called inspection_date, and
##extract the year as inspection_year

restaurants.data$`INSPECTION DATE` <- format(as.Date(restaurants.data$`INSPECTION DATE`, format="%m/%d/%Y"))
restaurants.data$`INSPECTION YEAR` <- format(as.Date(restaurants.data$`INSPECTION DATE`, format="%Y-%m-%d"), "%Y")

##iii. Rename the appropriate columns ‘id’, ‘borough’, ‘cuisine’, ‘action’, ‘code’,
##‘critical’, ‘score’, ‘grade’, ‘inspection_type

names(restaurants.data)[names(restaurants.data)=="CAMIS"] <- "id"
names(restaurants.data)[names(restaurants.data)=="BORO"] <- "borough"
names(restaurants.data)[names(restaurants.data)=="CUISINE DESCRIPTION"] <- "cuisine"
names(restaurants.data)[names(restaurants.data)=="ACTION"] <- "action"
names(restaurants.data)[names(restaurants.data)=="VIOLATION CODE"] <- "code"
names(restaurants.data)[names(restaurants.data)=="CRITICAL FLAG"] <- "critical"
names(restaurants.data)[names(restaurants.data)=="SCORE"] <- "score"
names(restaurants.data)[names(restaurants.data)=="GRADE"] <- "grade"
names(restaurants.data)[names(restaurants.data)=="INSPECTION TYPE"] <- "inspection_type"
names(restaurants.data)[names(restaurants.data)=="INSPECTION DATE"] <- "inspection_date"
names(restaurants.data)[names(restaurants.data)=="INSPECTION YEAR"] <- "inspection_year"


##rename the values in the
##‘action’ and ‘inspection_type’ column with shorter, simpler values.

restaurants.data$action[restaurants.data$action == "Violations were cited in the following area(s)."] <- "violations found"
restaurants.data$action[restaurants.data$action == "No violations were recorded at the time of this inspection."] <- "no violations"
restaurants.data$action[restaurants.data$action == "Establishment re-closed by DOHMH"] <- "re-closed"
restaurants.data$action[restaurants.data$action ==  "Establishment re-opened by DOHMH"] <- "re-opened"
restaurants.data$action[restaurants.data$action ==  "Establishment Closed by DOHMH.  Violations were cited in the following area(s) and those requiring immediate action were addressed."] <- "closed"

restaurants.data$inspection_type[restaurants.data$inspection_type == "Cycle Inspection / Initial Inspection"] <- "Cycle / Initial"
restaurants.data$inspection_type[restaurants.data$inspection_type == "Cycle Inspection / Re-inspection"] <- "Cycle / Re"
restaurants.data$inspection_type[restaurants.data$inspection_type == "Cycle Inspection / Reopening Inspection"] <- "Cycle / Reopening"
restaurants.data$inspection_type[restaurants.data$inspection_type == "Cycle Inspection / Compliance Inspection"] <- "Cycle / Comp"

restaurants.data$inspection_type[restaurants.data$inspection_type == "Pre-permit (Operational) / Initial Inspection"] <- "Pre Op / Initial"
restaurants.data$inspection_type[restaurants.data$inspection_type == "Pre-permit (Operational) / Re-inspection"] <- "Pre Op / Re"
restaurants.data$inspection_type[restaurants.data$inspection_type == "Pre-permit (Operational) / Reopening Inspection"] <- "Pre Op / Reopening"
restaurants.data$inspection_type[restaurants.data$inspection_type == "Pre-permit (Operational) / Compliance Inspection"] <- "Pre Op / Comp"


restaurants.data$inspection_type[restaurants.data$inspection_type == "Pre-permit (Non-operational) / Initial Inspection"] <- "Pre non-Op / Initial"
restaurants.data$inspection_type[restaurants.data$inspection_type == "Pre-permit (Non-operational) / Re-inspection"] <- "Pre non-Op / Re"
restaurants.data$inspection_type[restaurants.data$inspection_type == "Pre-permit (Non-operational) / Reopening Inspection"] <- "Pre non-Op / Reopening"
restaurants.data$inspection_type[restaurants.data$inspection_type == "Pre-permit (Non-operational) / Compliance Inspection"] <- "Pre non-Op / Comp"


##Deal with ‘Missing’ borough information, remove restaurants that haven’t been inspected, remove rows without a score or with a negative score, and remove
## any of the the following six inspection types:
##i. 'Calorie Posting / Re-inspection',
##ii. 'Inter-Agency Task Force / Re-inspection',
##iii. 'Smoke-Free Air Act / Re-inspection',
##iv. 'Administrative Miscellaneous / Re-inspection',
##v. 'Trans Fat / Re-inspection',
##vi. 'Inter-Agency Task Force / Initial Inspection'

restaurants.data <- restaurants.data[restaurants.data$inspection_date!="1900-01-01",]
restaurants.data <- restaurants.data[!is.na(restaurants.data["score"]),]
restaurants.data <- restaurants.data[restaurants.data$score >= 0, ]


restaurants.data <- restaurants.data[restaurants.data[["inspection_type"]] !="Calorie Posting / Re-inspection",]
restaurants.data <- restaurants.data[restaurants.data$inspection_type!="Inter-Agency Task Force / Re-inspection",]
restaurants.data <- restaurants.data[restaurants.data$inspection_type!="Smoke-Free Air Act / Re-inspection",]
restaurants.data <- restaurants.data[restaurants.data$inspection_type!="Administrative Miscellaneous / Re-inspection",]
restaurants.data <- restaurants.data[restaurants.data$inspection_type!="Trans Fat / Re-inspection",]
restaurants.data <- restaurants.data[restaurants.data$inspection_type!="Inter-Agency Task Force / Initial Inspection",]


##Some restaurants received different scores for the same inspection on the same
##day; replace all scores for any inspection for a given restaurant on a given day by
##the maximum score.

  
  


##Create the sample of data that you will use for prediction as a tibble called
##restaurant_data. We will restrict our attention to all initial cycle inspections that took
##place in 2015, 2016, or 2017 

restaurant_data <- restaurants.data
restaurant_data <- restaurant_data[restaurant_data$inspection_year %in% c("2015","2016","2017"), ]
restaurant_data <- restaurant_data[restaurant_data$inspection_type=="Cycle / Initial", ]


##a. Create a binary outcome variable called ‘outcome’, defined by whether the score
##for that initial cycle inspection was 28 or higher, or not.

restaurant_data$outcome <- ifelse(restaurant_data$score>28, 1, 0)



##b. For each initial cycle inspection, just keep the following features: borough,
##cuisine, outcome, and inspection_year.


cycle_inspections <- subset(restaurant_data, select = c("borough","cuisine","outcome","inspection_year") )



##Add weekday 

restaurant_data <- restaurant_data %>%
  mutate(weekday = wday(inspection_date, label=TRUE, abbr = FALSE)) %>%
  mutate(month = month(inspection_date, label=TRUE, abbr = FALSE)) 




##Perform some feature engineering. We will only create features that could be
##known before a given initial cycle inspection takes place

all_data <- restaurants.data

all_data <- all_data %>% select(c(id, score, action, inspection_date))

restaurant_data <- merge(restaurant_data,all_data, by=c("id"),all.x = TRUE, allow.cartesian=TRUE) 
restaurant_data <- restaurant_data %>% 
  filter(joint_data, inspection_date.y < inspection_date.x) %>%
  group_by_("id","inspection_date.x") %>% 
  mutate(num_previous_low_inspections = score.y < 14,
         num_previous_med_inspections = score.y >= 14 & score.y < 28,
         num_previous_high_inspections = score.y >= 28)

restaurant_data$num_previous_closings <- grepl("closed", restaurant_data$action.y) == TRUE

restaurant_data$num_previous_low_inspections[is.na(restaurant_data$num_previous_low_inspections)] <- 0
restaurant_data$num_previous_med_inspections[is.na(restaurant_data$num_previous_med_inspections)] <- 0
restaurant_data$num_previous_high_inspections[is.na(restaurant_data$num_previous_high_inspections)] <- 0
restaurant_data$num_previous_closings[is.na(restaurant_data$num_previous_closings)] <- 0

top50 <- restaurant_data %>%
  group_by(cuisine)%>%
  arrange(desc(n))

top50 <- top50[1:50,]

restaurant_data <- restaurant_data %>%
  filter (cuisine %in% top50)

