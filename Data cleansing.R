#### Week 3 Lecture Slide 6 (Example 1) ####

library(tidyverse)

test_results=read_csv(file="2018_19_GT_Results.csv") 
#load the data
str(test_results) #show the summary data of tibble
head(test_results) #show the first 6 rows of the data 

#The dates are in American format. 
#Say the first number is the month,
#the second number is the date, 
#and the third number is year.
#By looking at the csv file, the first entry "3/27/18" 
#and the last entry "3/27/2018" are both valid.
#However, the two entries have been entered with different formats,
#and so R treats the two entries as two different dates.

#Time stamp column

unique(test_results$Timestamp) #see unique timestamps
#"3/27/2017" (A11 in the csv file) and "Mar-27" 
#(A25 in the csv file)both probably refer to "3/27/2018", 
#and so let us update these.

test_results <- test_results %>%
mutate(Timestamp = replace(Timestamp,Timestamp == "3/27/2017"
                           ,"3/27/2018")) %>%
mutate(Timestamp = replace(Timestamp,Timestamp == "Mar-27",
                           "3/27/2018")) %>%
mutate(Timestamp = replace(Timestamp,Timestamp == "3/27/18",
                           "3/27/2018")) %>%
mutate(Timestamp = replace(Timestamp,Timestamp == "Mar-18",
                           "3/18/2018"))

unique(test_results$Timestamp) 

#see unique timestamps again

#Entering Grade Level column
unique(test_results$`Entering Grade Level`) 
#see unique entering grade
#there is a natural progression from kindergarten to Grade 3,
#and so it may make sense to replace K with 0.
#we shall replace K with 0 and convert to integers 
#Please note that a backtick ` is used after the $ sign. 
#Why?
#The backticks `` are used for the variable 
#"Entering Grade Level" which contains spaces. 
#An error will result if `` are not used.

test_results <- test_results %>%
mutate(`Entering Grade Level` 
       = replace(`Entering Grade Level`, `Entering Grade Level` 
                 == "K","0")) %>% 
#replace "K" with "0"
mutate(`Entering Grade Level` = as.integer(`Entering Grade Level`)) 
#convert everything in the column `Entering Grade Level` to integer

unique(test_results$`Entering Grade Level`) 
#see unique entering grade

test_results <- test_results %>%
rename(Grade = `Entering Grade Level`)
#for convenience, the variable "Entering Grade Level" 
#is renamed as "Grade".

unique(test_results$Grade) 
#verify that the variable has been renamed

#District column

unique(test_results$District) #see unique districts
#we can see that there is at least one missing value or NA.
is.na(test_results$District) 
#this will test the cells for missing values
#the missing value (or TRUE) is the entry 33.
sum(is.na(test_results$District)) 
#count the number of NA's
#there is only one missing value.
which(is.na(test_results$District)) 
#return the location/index of the NA value

#since the data set does not contain other information 
#about District, we cannot do anything further for the NA. 

#please note that District is a categorical variable,
#and so we change this column to characters to reflect this.
test_results$District=as.character(test_results$District) 
#convert to character to make sure it is treated as categorical
test_results$District #print the result


#Birth month column

test_results <- test_results %>%
  rename(Birth_month = `Birth Month`)
#for convenience, the variable "Birth Month" 
#is renamed as "Birth_month".

unique(test_results$Birth_month) 
#check unique birth month
#why are there 14 months? 
#"Febrauary" is misspelled 
#and "september" starts with a lower case.

#capitalize the first letter of each month
#and correct the misspelling "Febrauary"

test_results <- test_results %>%
  mutate(Birth_month = str_to_title(Birth_month)) %>% 
  #str_to_title() automatically forces the first letter 
  #to be Upper Case and the other letters to be Lower Case. 
  mutate(Birth_month = replace(Birth_month, Birth_month 
                                 == "Febrauary", "February"))
  #correct the misspelling "Febrauary"

unique(test_results$Birth_month) 
#check unique birth month again

#OLSAT Verbal Score column

test_results <- test_results %>%
  rename(OLSAT_VS = `OLSAT Verbal Score`)
#for convenience, the variable "OLSAT Verbal Score" 
#is renamed as "OLSAT_VS".

unique(test_results$OLSAT_VS) 
#check unique OLSAT Verbal Score
#there is at least one missing value or NA
sum(is.na(test_results$OLSAT_VS)) 
#count the number of missing values
#there is only one missing value. 
which(is.na(test_results$OLSAT_VS)) 
#return the location/index of the missing value
#the missing value is in row 6.

#Replace missing value with the mean of other scores
#in the same column 
#please note that the option na.rm=TRUE is used to remove the NA
#when evaluating the mean score.
test_results <- test_results %>%
  mutate(OLSAT_VS = replace(OLSAT_VS,is.na(OLSAT_VS), 
                            mean(OLSAT_VS,na.rm=TRUE)))
#the logical statement is.na(OLSAT_VS) finds all the NAs
#in the column OLSAT_VS. 
#the option na.rm=TRUE is to remove the NAs when evaluating
#the mean score of the column OLSAT_VS. 

unique(test_results$OLSAT_VS) 
#check unique OLSAT Verbal Score again
#the missing value NA is replaced by the mean score 24.62222.

#OLSAT Verbal percentile column

test_results <- test_results %>%
  rename(OLSAT_VP = `OLSAT Verbal Percentile`)
#for convenience, the variable "OLSAT Verbal Percentile" 
#is renamed as "OLSAT_VP".

unique(test_results$OLSAT_VP) 
#check unique OLSAT Verbal percentile
#there is at least one missing value or NA
sum(is.na(test_results$OLSAT_VP)) 
#count the number of missing values
#there are two missing values
which(is.na(test_results$OLSAT_VP)) 
#return the locations/indices of the NA values
#the indices of the NA values are 6 and 18
#say student 6 and student 18, respectively

#let us replace student 6's OLSAT Verbal percentile NA 
#with the median IN THE SAME DISTRICT

#find out the student 6's district 
student_6_district <- test_results %>%
  slice(6) #pull out row 6
  
student_6_district 
#Row 6 is pulled out
#District "2" is student 6's district.

student_6_district <- test_results %>%
  slice(6) %>% #pull out row 6
  pull(District) 
#extract the district as a value and not a tibble

student_6_district
#the character "2" for student 6's district is extracted. 

#Extract the OLSAT Verbal Percentiles 
#for all students in District "2"
student_6_district_OVP <- test_results %>%
  filter(District == student_6_district) %>% 
  #filter for all rows with District "2"
  pull(OLSAT_VP) 
  #extract OLSAT Verbal Percentile
student_6_district_OVP
#print the OLSAT Verbal Percentiles 
#for all students in District "2"
#there is one NA.

district_median <- median(student_6_district_OVP,na.rm=TRUE) 
#calculate median for District "2"
#the option na.rm=TRUE removes the NA when evaluating the median
district_median 
#print the median for District "2" 

test_results <- test_results %>%
  mutate(OLSAT_VP = replace(OLSAT_VP, 
  District == student_6_district & is.na(OLSAT_VP),district_median))
#use the median of District "2" to replace NA in row 6

test_results$OLSAT_VP 
#print the result
#the NA in row 6 is replaced with 
#the median of District "2".
#there is another NA in row 18.

#replace all the NAs in the OLSAT_VP column 
#with the median of District "2"
test_results <- test_results %>%
  mutate(OLSAT_VP = replace(OLSAT_VP, is.na(OLSAT_VP),
                            district_median))

test_results$OLSAT_VP
#print the result
#the other NA in row 18 is replaced with
#the median of District "2".

#Columns 7, 8, 9 are left to you as an exercise.

# NNAT Non Verbal Raw Score

test_results = test_results %>%
  rename(NNAT_Non_Verbal_Raw_Score = `NNAT Non Verbal Raw Score`)


unique(test_results$NNAT_Non_Verbal_Raw_Score) 
sum(is.na(test_results$NNAT_Non_Verbal_Raw_Score)) 
which(is.na(test_results$NNAT_Non_Verbal_Raw_Score))


test_results <- test_results %>%
  mutate(NNAT_Non_Verbal_Raw_Score = replace(NNAT_Non_Verbal_Raw_Score,is.na(NNAT_Non_Verbal_Raw_Score), 
                            mean(NNAT_Non_Verbal_Raw_Score,na.rm=TRUE)))


unique(test_results$NNAT_Non_Verbal_Raw_Score) 

# NNAT Non Verbal Percentile


test_results = test_results %>%
  rename(NNAT_Non_Verbal_Percentile = `NNAT Non Verbal Percentile`)


unique(test_results$NNAT_Non_Verbal_Percentile) 
sum(is.na(test_results$NNAT_Non_Verbal_Percentile)) 
which(is.na(test_results$NNAT_Non_Verbal_Percentile))


student_18_district <- test_results %>%
  slice(18) %>% 
  pull(District) 

student_18_district_NNATNVP <- test_results %>%
  filter(District == student_18_district) %>% 
  pull(NNAT_Non_Verbal_Percentile)


district_18_median <- median(student_18_district_NNATNVP,na.rm=TRUE) 


test_results <- test_results %>%
  mutate(NNAT_Non_Verbal_Percentile = replace(NNAT_Non_Verbal_Percentile, 
                            District == student_18_district & is.na(NNAT_Non_Verbal_Percentile),district_18_median))

unique(test_results$NNAT_Non_Verbal_Percentile) 

# Overall Score

test_results = test_results %>%
  rename(Overall_Score = `Overall Score`)


unique(test_results$Overall_Score) 
sum(is.na(test_results$Overall_Score)) 
which(is.na(test_results$Overall_Score))


#X10 column

#if you look up metadata online, 
#this column is "School preference".

#give a name to this column
test_results = test_results %>% 
  rename(School_Preference = ...10)

unique(test_results$School_Preference) 
#check the unique entries for School_Preference

#capitalise first letter
test_results = test_results%>%
  mutate(School_Preference = str_to_title(School_Preference))

unique(test_results$School_Preference) 
#check the unique entries for School_Preference again

sum(is.na(test_results$School_Preference)) 
#count the number of NA's

#There are quite a few things that should be NA
test_results = test_results %>%
  mutate(School_Preference = replace(School_Preference,
  substr(School_Preference,1,2)=="No",NA)) %>%
  #replace all things starting with "No" by NA
  mutate(School_Preference = replace(School_Preference,
  substr(School_Preference,1,3)=="N/A",NA)) %>%
  #replace all things starting with "N/A" by NA
  mutate(School_Preference = replace(School_Preference,
  substr(School_Preference,1,1)==":",NA)) %>%
  #fix the emoticon :-( by replacing it with NA
  mutate(School_Preference = replace(School_Preference,
  School_Preference=="Anderson?","Anderson"))
  #replace "Anderson?" with "Anderson"
  
unique(test_results$School_Preference) 
#check the unique entries for School_Preference again

#X11 and 'School Assigned' columns

unique(test_results$...11) 
#verify these columns are empty
unique(test_results$'School Assigned') 
#verify these columns are empty

test_results = test_results %>%
  select(-'...11',-'School Assigned') 
#drop these empty columns

str(test_results) 
#verify there are now 13 columns instead of 15

#Will you enroll there? column 

unique(test_results$'Will you enroll there?') 
#see the unique entries in 'Will you enroll there?'

#we can do similar edits as School_Preference, rename first

test_results = test_results %>%
  rename(Enrol = 'Will you enroll there?') 
#rename to something without spaces

test_results = test_results %>%
  mutate(Enrol = str_to_title(Enrol)) %>%
#capitalise the first letter of each entry in the Enrol column
  mutate(Enrol = replace(Enrol,substr(Enrol,1,1)=="Y","Yes"))
#change anything starting with "Y" to "Yes"

unique(test_results$Enrol) 
#see the unique entries in 'Will you enroll there?' again
#the rest are left as an exercise

#Test prep column

test_results = test_results %>%
  rename(Test_Prep = 'Test Prep?') 
#rename to something without spaces or punctuation

unique(test_results$Test_Prep)

#do somethings similar to the previous column

test_results = test_results %>%
  mutate(Test_Prep = str_to_title(Test_Prep)) %>% 
  #capitalise the first letter of each entry
  mutate(Test_Prep = replace(Test_Prep, 
  substr(Test_Prep,1,1) == "Y","Yes")) %>%
  #change all entries starting with "Y" to "Yes"
  mutate(Test_Prep = replace(Test_Prep, 
  substr(Test_Prep,1,1) == "A","Yes")) %>%
  #change all entries starting with "A" to "Yes"
  mutate(Test_Prep = replace(Test_Prep, 
  substr(Test_Prep,1,1) == "N","No")) %>%
  #change all entries starting with "N" to "No"
  mutate(Test_Prep = replace(Test_Prep, 
  substr(Test_Prep,1,1) == "M",NA)) %>%
  #change all entries starting with "M" to NA 
  mutate(Test_Prep = replace(Test_Prep, 
  substr(Test_Prep,1,1) == "P","Yes"))
  #change all entries starting with "P" to "Yes" 
  
unique(test_results$Test_Prep) 
#check unique responses again

#X15 column

unique(test_results$...15) 
#see unique responses
#"Yes at home with mom" and "mom" are the unique elements 
#in the non-empty entries.
#quite clearly these responses are for the Test_Prep column. 
#Check first before replacing
#say see the entries in Test_Prep corresponding to 
#the non-empty entries "Yes at home with mom" 
#and "mom" in X15

test_results %>%
  select(Test_Prep,...15) %>% 
  #see only these 2 columns
  filter(!is.na(...15)) 
  #filter shows only non-empty entries in X15
  #the !is.na function gives "TRUE" to non-empty entries
  #this is the opposite of the is.na function. 
  #the logical statement !is.na(X15) selects only 
  #the non-empty entries in X15

#change NA in Test_Prep to "Yes" which corresponds to 
#"Yes at home with mon" in X15 
#It may be convenient to change the two entries in 
#Test_Prep corresponding to the non-empty entries
#in X15 to "Yes" at the same time. 

test_results = test_results %>%
  mutate(Test_Prep = replace(Test_Prep,!is.na(...15),"Yes")) %>% 
  #the logical statement !is.na(X15) selects only 
  #the non-empty entries in X15
  #"Yes" to Test_Prep if they entered something for X15
  #say the two entries with "Yes at home with mom" and "mom"
  select(-...15) 
  #drop X15 since we have used up the information contained in X15
 
str(test_results)
#check that the above changes have been made

#the cleansing has been made. 

#now to save our changes
write_csv(test_results,"Clean_Results.csv")