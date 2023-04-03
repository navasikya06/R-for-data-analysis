#The tutorial in Week 3 "Manipulating data with R" 
#is based on the following:
#https://paldhous.github.io/ucb/2016/dataviz/week7.html

#### Install and load R packages ####

install.packages("tidyverse")
#Install tidyverse

library(readr)
library(dplyr)
#load the readr and dplyr packages to read, 
#write and manipulate data

#### Load and view data #### 

###### Load data ####

pfizer <- read_csv("pfizer.csv")
fda <- read_csv("fda.csv") 
#load the two datasets, pfizer payments to doctors
#and warning letters sent by food and drug administration


###### View the data ####

#see that the environment contains two objects 
#of type tbl_df, say a dataframe holding data tables
#the value of a dataframe contains 
#the number of observations (rows)
#and the number of variables (columns)

str(pfizer)
#display the structure of the data "pfizer"
str(fda)
#display the structure of the data "fda"

pfizer$total
#print the values in the total column of the data "pfizer"

#Different functions to change the data types:
#as.character(): convert to characters (or a text string)
#as.numeric(): convert to a number
#as.factor(): convert to a categorical variable
#as.integer(): convert to an integer
#as.Date(): convert to a date
#as.POSIXct(): convert to a full date and time

pfizer$total <- as.numeric(pfizer$total)
#convert total to numeric variable

str(pfizer)
#see the structure of the updated data "pfizer"

summary(pfizer)
#calculate some summary statistics of the data
#"pfizer" such as minimum, 1st quarter, mean,
#median, 3rd quarter and maximum 

#### Manipulate and analyze data #### 

#use the dplyr package (a sub-package of tidyverse) 
#to manipulate the data

###### Filter and sort data ####

ca_expert_10000 <- pfizer %>%
  filter(state == "CA" & total >= 10000 
  & category == "Expert-Led Forums")
#find doctors in California who were paid $10,000 
#or more by Pfizer to run "Expert-Led Forums."
#== means "is equal to" in the logical statement.
#>= means "is greater than or equal to". 
#the Boolean operator & means "and". 

ca_expert_10000 <- pfizer %>%
  filter(state == "CA" & total >= 10000 
  & category == "Expert-Led Forums") %>%
  arrange(desc(total))
#list the doctors in descending order 
#by the payments received
#by default, the arrange() function will sort the data 
#in ascending order 
#(i.e., from the smallest to the largest) 

ca_ny_expert_10000 <- pfizer %>%
  filter((state == "CA" | state == "NY") 
  & total >= 10000 
  & category == "Expert-Led Forums") %>%
  arrange(desc(total))
#find doctors in California or New York 
#who were paid $10,000 or more 
#by Pfizer to run "Expert-Led Forums."
#and list the doctors in descending order 
#by the payments received
#the Boolean operator | means "or".

not_ca_expert_10000 <- pfizer %>%
  filter(state != "CA" & total >= 10000 
  & category=="Expert-Led Forums") %>%
  arrange(desc(total))
#find doctors in states other than California 
#who were paid $10,000 or more 
#by Pfizer to run "Expert-Led Forums."
#the operator != means "is not equal to"
#in the logical statement.

ca_ny_tx_fl_prof_top20 <- pfizer %>%
  filter((state=="CA" | state == "NY" 
  | state == "TX" | state == "FL") 
  & category == "Professional Advising") %>%
  arrange(desc(total)) %>%
  head(20)
#find the 20 doctors across the four largest states 
#(CA, TX, FL, NY) who were paid 
#the most for professional advice
#head(20) displays the top 20. 

expert_advice <- pfizer %>%
  filter(category == "Expert-Led Forums" 
  | category == "Professional Advising") %>%
  arrange(last_name, first_name)
#filter the data for all payments for running 
#Expert-Led Forums or for Professional Advising, 
#and arrange alphabetically by doctor 
#(last name, then first name)

expert_advice <- pfizer %>%
  filter(grepl("Expert|Professional", category)) %>%
  arrange(last_name, first_name)
#use pattern matching to filter text
#the grepl function finds values containing 
#a particular string of text.
#simplify the code for filtering based in text

not_expert_advice <- pfizer %>%
  filter(!grepl("Expert|Professional", category)) %>%
  arrange(last_name, first_name)
#differs only by the Boolean operator ! 
#meaning "is not equal to"

###### Append one data frame to another ####

pfizer2 <- bind_rows(expert_advice, not_expert_advice)
#merge data frames using the bind_rows function

###### Write data to a CSV file #### 

#use the package readr (a sub-package of tidyverse)
#to write data to a csv file

write_csv(expert_advice, "expert_advice.csv", na="")
#write expert_advice data to a csv file
#the option na="" makes sure that any missing values
#or NAs in the data frame are saved as blanks.
#any missing values will be displayed as NA 
#in the saved file.

###### Group and summarize data ####

state_sum <- pfizer %>%
  group_by(state) %>%
  summarize(sum = sum(total)) %>%
  arrange(desc(sum))
#compute total payments by state
#the use of the group_by function followed by
#the summarize function to group and
#summarize data 
#calculate the total payments by the sum function

state_summary <- pfizer %>%
  group_by(state) %>%
  summarize(sum = sum(total), median = median(total), 
            count = n()) %>%
  arrange(desc(sum))
#calculate total payments, the median payment,
#and the number of payments by state
#compute total payments by the sum function,
#the median payment by the median function
#no specification of a variable for the n() function
#since it counts the number of rows in the data

state_category_summary <- pfizer %>%
  group_by(state, category) %>%
  summarize(sum = sum(total), median = median(total), 
            count = n()) %>%
  arrange(state, category)
#calculate total payments, the median payment,
#and the number of payments by state and category
#include multiple variables separated by commas 
#in arrange

###### Working with dates ####

post2005 <- fda %>%
  filter(issued >= "2005-01-01") %>%
  arrange(issued)
#filter the data for FDA warning letters 
#sent from the start of 2005 onwards
#>= can be used for dates and for numbers.

letters_year <- fda %>%
  mutate(year = format(issued, "%Y")) %>%
  group_by(year) %>%
  summarize(letters=n())
#count the number of letters issued by year
#create a new column year using the mutate function
#extract from the issued dates using the format function
#%Y specifies the new variable year as the four-digit year
#group_by groups the data by year
#n() counts the number of letters for each year 

fda <- fda %>%
  mutate(days_elapsed = Sys.Date() - issued,
          weeks_elapsed = 
  difftime(Sys.Date(), issued, units = "weeks"))
#add new columns giving the number of days and weeks 
#that have elapsed since each letter was sent
#the function Sys.Date returns the current date.
#Sys.Date() - issued means subtract the current
#date by the issued date and calculates the difference
#in days.
#Use the difftime function to calculate date and time
#differences using other units, say weeks.

###### Join data from two data frames ####

#join to identify doctors paid to run Expert-led forums 
#who also received a warning letter

expert_warned_inner <- inner_join(pfizer, fda, 
by=c("first_name" = "name_first", 
"last_name" = "name_last")) %>%
  filter(category=="Expert-Led Forums")
#inner_join() returns values from both tables 
#only where there is a match.
#by=c() specifies how the join should be made. 

#expert_warned_inner contains all of the columns 
#from both data frames. 

expert_warned_semi <- semi_join(pfizer, fda, 
by=c("first_name" = "name_first", 
"last_name" = "name_last")) %>%
  filter(category=="Expert-Led Forums")
#semi_join() filters the first-mentioned table to 
#include only values that have matches in the second table.

#expert_warned_semi contains only columns 
#from the pfizer data frame.

#Use inner_join and select to select the columns 

expert_warned <- inner_join(pfizer, fda, 
by=c("first_name" = "name_first", 
"last_name" = "name_last")) %>%
  filter(category=="Expert-Led Forums") %>%
  select(first_plus, last_name, city, state, total, issued)
#select by columns' names

expert_warned <- inner_join(pfizer, fda, 
by=c("first_name" = "name_first", 
"last_name" = "name_last")) %>%
  filter(category=="Expert-Led Forums") %>%
  select(2:5,10,12)
#select by columns' positions/numbers 
