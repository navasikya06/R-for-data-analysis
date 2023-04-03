#The tutorial in Week 4 will consider data cleansing and
#visualizations in R. 

library(tidyverse)

test_results=read_csv(file="2017_18_GT_Results.csv")
str(test_results) #show summary data of tibble
head(test_results) #show the first 6 entries

#Time stamp column

unique(test_results$Timestamp) 
#see unique timestamps
any(is.na(test_results$Timestamp)) 
#check if there are any missing values or NAs
colSums(is.na(test_results))

#remove time stamps (e.g. 10:40:45) first: 
#separate using the space

word("ABC DEF") 

#this function pulls the first word in a string sentence

test_results_2 = test_results %>%
  mutate(Timestamp = word(Timestamp)) %>%
  mutate(Timestamp = replace(Timestamp,
  Timestamp == "1/11/2019","1/11/2017"))

unique(test_results_2$Timestamp) 
#see unique timestamps again

#Grade Entering Level column

test_results_2 = test_results_2 %>% 
  #make the name easier
  rename(Grade = `Entering Grade Level`)

unique(test_results_2$Grade) 
#see unique entering grade
#we can replace K with 0 and convert to integers

test_results_3 = test_results_2 %>%
  mutate(Grade = replace(Grade,Grade == "K","0")) %>% 
  #replace "K" with "0"
  mutate(Grade = as.integer(Grade)) 
#convert everything to integer

unique(test_results_3$Grade) 
#see unique entering grade again

#District column

unique(test_results_3$District) 
#see unique districts

is.na(test_results_3$District) 
#this will test the cells for missing values
sum(is.na(test_results_3$District)) 
#count NA's
which(is.na(test_results_3$District)) 
#returns the indices of NA values
test_results_3$District=
  as.character(test_results_3$District) 
#convert to character to make sure 
#it is treated as a categorical variable
#we are not given any information about
#the NAs in the District column, and so
#we cannot do anything further for those NAs.

#Birth month column

unique(test_results_3$`Birth Month`) 
#check unique birth month
#this one seems fine

#OLSAT Verbal Score column

sum(is.na(test_results_3$`OLSAT Verbal Score`)) 
#count the number of missing values
unique(test_results_3$`OLSAT Verbal Score`) 
#check unique OLSAT Verbal Score
#we see there are some issues here

word("1/2",sep = "/") 
word("1/2", 1, sep = "/") 
word("1/2", 2, sep = "/") 
word("1/2", 1, 2, sep = "/") 
word("1/2", 1, -1, sep = "/") 
#an example of changing the separator

test_results_4 = test_results_3 %>%
  mutate(`OLSAT Verbal Score` = 
           word(`OLSAT Verbal Score`,sep = "/")) %>%
  mutate(`OLSAT Verbal Score` = 
           replace(`OLSAT Verbal Score`,
                   word(`OLSAT Verbal Score`) == 
                     "Fill",NA)) %>%
  #recall that the word function pulls out 
  #the first word in a string sentence
  mutate(`OLSAT Verbal Score` = 
           replace(`OLSAT Verbal Score`,
                   `OLSAT Verbal Score` == "**",NA)) %>%
  mutate(`OLSAT Verbal Score` 
         = replace(`OLSAT Verbal Score`,
                   `OLSAT Verbal Score` == "-",NA))

unique(test_results_4$`OLSAT Verbal Score`)

#The other score columns are left to you as an exercise.

#School Preferences column

#rename this column
test_results_4 = test_results_4 %>% 
  rename(School_Preference = `School Preferences`)

unique(test_results_4$School_Preference) 
#check the unique entries for School_Preference

sum(is.na(test_results_4$School_Preference)) 
#count NA's

#we see one of the responses is "na", replace with NA
test_results_5 = test_results_4 %>%
  mutate(School_Preference = 
           replace(School_Preference,
                   School_Preference == "na",NA))

#there are no other obvious things to do here, 
#we might leave this alone.

#School Assigned column

test_results_5 = test_results_5 %>% 
  #rename to this column
  rename(School_Assigned = `School Assigned`)

unique(test_results_5$School_Assigned) 
#see unique entries

test_results_6 = test_results_5 %>%
  mutate(School_Assigned = 
           replace(School_Assigned,
                   word(School_Assigned) == "Currently", 
                   "Brooklyn dual language")) %>%
  mutate(School_Assigned 
         = str_to_title(School_Assigned)) %>%
  mutate(School_Assigned = replace(School_Assigned,
                    School_Assigned == "None",NA)) %>%
  mutate(School_Assigned = replace(School_Assigned,
                    School_Assigned == "N/A",NA))

unique(test_results_6$School_Assigned) 
#see unique entries again

#Will you enroll there? column

test_results_6 = test_results_6 %>% 
  #rename to this column
  rename(Will_Enroll = `Will you enroll there?`)

unique(test_results_6$Will_Enroll) 
#see the unique entries in 'Will you enroll there?'

test_results_7 = test_results_6 %>%
  mutate(Will_Enroll = str_to_title(Will_Enroll)) %>%
  mutate(Will_Enroll = replace(Will_Enroll,
                        Will_Enroll == "Maybe",NA))

unique(test_results_7$Will_Enroll) 
#check unique entries again

#How many students are there in each grade?

test_results_7 %>%
  filter(Grade==0)
#read the output to see 77 students in Kindergarten

#alternative method:
test_results_7 %>%
  group_by(Grade) %>%
  tally()  # Count observations by group
  # count()  # same effect

#Is there any relationship between grade 
#and overall score?

ggplot(test_results_7) +
  geom_point(aes(x=Grade,y=`Overall Score`),alpha=0.2)
#note that Grade is a categorical variable. 

#Is there any relationship between birth month 
#and overall score?

ggplot(test_results_7) +
  geom_point(aes(x=`Birth Month`,y=`Overall Score`),
             alpha=0.2)
#again Birth Month is a categorical variable. 

#Is there any relationship between OLSAT percentile 
#and NNAT percentile?

ggplot(test_results_7) +
  geom_point(aes(x=`OLSAT Verbal Percentile`,
                 y=`NNAT Non Verbal Percentile`),
             alpha=0.2)
# darker color is due to more observations concentrating there


# ------------------- EXERCISE ------------------
# use test_results_7 to do the following exercise

# 1. replace NA in column 'OLSAT Verbal Score'
# with the mean verbal score of this column


test_results_7 = test_results_7 %>% 
  rename(OLSAT_VS = `OLSAT Verbal Score`)

summary(test_results_7)

unique(test_results_7$OLSAT_VS) 
#check unique OLSAT Verbal Score
#there is at least one missing value or NA
sum(is.na(test_results_7$OLSAT_VS)) 
#count the number of missing values
#there is only one missing value. 
which(is.na(test_results_7$OLSAT_VS)) 
#return the location/index of the missing val

test_results_7 <- test_results_7 %>%
  rename(OLSAT_VS = `OLSAT Verbal Score`) %>%
  mutate(OLSAT_VS = as.numeric(OLSAT_VS)) %>%
  mutate(OLSAT_VS = replace(OLSAT_VS,is.na(OLSAT_VS), 
                            mean(OLSAT_VS,na.rm=TRUE)))


# **************************************


# 2. remove special characters in column 'OLSAT Verbal Percentile'



test_results_7 = test_results_7 %>% 
  rename(OLSAT_VP = `OLSAT Verbal Percentile`)


unique(test_results_7$OLSAT_VP) 


summary(test_results_7)

test_results_7 <- test_results_7 %>%
  mutate(OLSAT_VP = replace(OLSAT_VP,OLSAT_VP == '~70',  
                            '70'))



# **************************************


# 3. clean up column 'NNAT Non Verbal Raw Score'
# - remove special characters
# - remove any non-sense value
# - make every value in this column meaningful, 
#   for any missing value, replace with the median score in the same grade



test_results_7 = test_results_7 %>% 
  rename(NNAT_NVS = `NNAT Non Verbal Raw Score`)

unique(test_results_7$NNAT_NVS)

mean(test_results_7$NNAT_NVS, na.rm = TRUE)

test_results_7 = test_results_7 %>% 
  mutate(NNAT_NVS = 
           replace(NNAT_NVS,
                   NNAT_NVS == 
                     "Fill out later.",NA)) %>%
  mutate(NNAT_NVS = 
           replace(NNAT_NVS,
                   NNAT_NVS == "**",NA)) %>%
  mutate(NNAT_NVS 
         = replace(NNAT_NVS,
                   NNAT_NVS == "-",NA)) %>%
  mutate(NNAT_NVS = as.numeric(NNAT_NVS)) %>%
  mutate(NNAT_NVS 
         = replace(NNAT_NVS,
                   NNAT_NVS == "39/48",mean(NNAT_NVS, na.rm = TRUE))) %>%
  mutate(NNAT_NVS 
         = replace(NNAT_NVS,
                   NNAT_NVS == "40/48",mean(NNAT_NVS, na.rm = TRUE))) %>%
  mutate(NNAT_NVS 
         = replace(NNAT_NVS,
                   NNAT_NVS == "41/48",mean(NNAT_NVS, na.rm = TRUE))) %>%
  mutate(NNAT_NVS 
         = replace(NNAT_NVS,
                   NNAT_NVS == "36/48",mean(NNAT_NVS, na.rm = TRUE)))
  


# *****************************************


# 4. replicate the given plot with following steps
# - group students by their birth month, calculate group size
# - group students by their birth month, 
#   calculate the mean of `Overall Score` column
# - combine the calculated group size and mean score into one table
# - use the combined table to plot the given chart

count_month = test_results_7 %>%
  group_by('Birth month') %>%
  tally()  

summary(test_results_7)

mean_month = test_results_7 %>%
  rename(OS = `Overall Score`) %>%
  mutate(OS = as.numeric(OS)) %>%
  group_by('Birth month') %>%
  summarize(mean = mean(OS))

new <- bind_cols(count_month, mean_month)

ggplot(new)+
  geom_point(aes(x=n))
  

# ***********************************
# -----------------------------------------------


