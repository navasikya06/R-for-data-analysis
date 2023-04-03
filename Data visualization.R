#### Week 3 Lecture Slide 12 (Example 2) ####
library(tidyverse)
library(zoo)
library(dplyr)

temperature_table=read.csv(file="Climate_data.csv")

head(temperature_table) 
#inspect the data by printing the first few rows

#the first two and last columns will not be used here, drop them

temperature_table = temperature_table %>%
  select(-'Product code',-'Bureau of Meteorology station number'
         ,-Quality) #drop the three columns

head(temperature_table) 
#the three columns are removed.

#combine year and month to make a date column

temperature_table = temperature_table %>%
  mutate(Date = as.yearmon(paste(temperature_table$Year,
  temperature_table$Month,sep="-")))
#mutate is used to create a new column date.
#note that the function as.yearmon is from the zoo package.
#it is used to combine year and month into date. 

head(temperature_table) 
#the new column date is created. 

#Rename temperature column for convenience

temperature_table = temperature_table %>%
  rename(Temp = `Mean.maximum.temperature..Â.C.`)

head(temperature_table) 
#inspect data again
#the temperature column is renamed.

#Let's start with a scatterplot of the temperatures against year
#use the ggplot2 package which is a part of the tidyverse package

ggplot(temperature_table) + 
  #enter our data into ggplot, + means we're adding more things
  #+ acts like a Pipe (or a Pipe operator), 
  #but we use + instead of %>%
  geom_point(aes(x=Year, y=Temp))+ 
  #geom_point gives a scatterplot
  #the aesthetic specification "aes" is used to specify 
  #the x-axis and the y-axis.
  xlab("Year") + #label the x-axis
  ylab("Temperature") + #label the y-axis
  ggtitle("Average max temperatures by year") 
  #give a title to the graph

#notice the title is to the left. 
#we can run the following code to make it centered. 
theme_update(plot.title = element_text(hjust = 0.5))

#now let's make a histogram of the temperatures observed.

ggplot(temperature_table) +
  geom_histogram(aes(x=Temp,fill=..count..),
  stat="bin",binwidth=0.5) + 
  #geom_histogram plots a histogram.
  #note that in the histogram, the x-axis is the temperature value
  #and the y-axis is the respective frequency. 
  #fill=..count.. fills color depending on the number of occurrence
  #(or the frequency) of a particular temperature value.  
  #geom_bar gives bar graph, and the fill colour depends on count
  #stat="bin" groups the temperature data into bins of the histogram.
  #note that the bins are of equal length.
  #binwidth=0.5 specifies the width of each bin. 
  xlab("Temperature") + #label the x-axis
  ggtitle("Histogram of observed temperatures") + 
  #give the histogram a name
  theme_light() + 
  #change background colour
  scale_fill_continuous() 
  #choose the colour scheme

#we now make some a boxplot by month

ggplot(temperature_table) +
  geom_boxplot(aes(x=Month, y=Temp)) + 
  #force R to read months as a categorical variable
  xlab("Month") +
  #label the x-axis
  ylab("Temperature") +
  #label the y-axis 
  ggtitle("Temperatures by month")
  #give the title of the boxplot 

#Finally, let's make a line plot.
#Let's calculate the average temp in each year, 
#and plot that against the year. 

annual_mean_temp <- group_by(temperature_table,Year) %>% 
  #group the temperature data based on year
  summarise(mean_temp=mean(Temp)) 
  #calculate mean temp within each group (year)

head(annual_mean_temp) 
#inspect new tibble and see that the group means 
#(or the annual means) have been calculated

ggplot(annual_mean_temp) +
  geom_line(aes(x=annual_mean_temp$Year,
  y=annual_mean_temp$mean_temp),color="blue") + 
  #geom_line gives the line plot
  xlab("Year") +
  #label the x-axis
  ylab("Temperature") +
  #label the y-axis 
  ggtitle("Annual average daily max temperature")
  #give the line plot a title 