####Introduction####
#In this tutorial, we shall discuss the use of R to compute 
#some descriptive statistics for real data sets 
#on Export Price Index and Import Price Index in Australia.

#Source: Australian Bureau of Statistics, 
#International Trade Price Indexes, 
#Australia December 2021

#The data set on Export Price Index contains 
#quarterly percentage changes in the Export Price Index 
#for all groups from December 2017 to December 2021

#The data set on Import Price Index contains 
#quarterly percentage changes in the Import Price Index 
#for all groups from December 2017 to December 2021

####Preparation####
#Install and load the package "tidyverse"
install.packages("tidyverse")
library(tidyverse)

#Import the data sets 
EPI<- read_csv("Export price index.csv")
IPI<- read_csv("Import price index.csv")

#Inspect the structure of the data sets
str(EPI)
str(IPI)

#Extract the column "% Change"
EPIChange<- EPI$`% change`
IPIChange<- IPI$`% change'

####Measures of Location####
#Compute the means of the % changes 
MeanEPI<- mean(EPIChange)
MeanIPI<- mean(IPIChange)
#Print the means
MeanEPI
MeanIPI

#Compute the medians of the % changes
MedianEPI<- median(EPIChange)
MedianIPI<- median(IPIChange)
#Print the means
MedianEPI
MedianIPI

#Compute the modes of the % changes
ModeEPI<- mode(EPIChange)
ModeIPI<- mode(IPIChange)
#Print the modes
ModeEPI
ModeIPI

####Measures of Dispersion or Variation####
#Compute the sample variances of the % changes 
VarEPI<- var(EPIChange)
VarIPI<- var(IPIChange)
#Note that the "var" function in R gives the sample variance.
#Print the sample variances
VarEPI
VarIPI

#Compute the sample standard deviations of the % changes 
SDEPI<- sd(EPIChange)
SDIPI<- sd(IPIChange)
#Note that the "sd" function in R gives the sample standard deviation.
#Print the sample standard deviations
SDEPI
SDIPI

#Compute the ranges of the % changes
RangeEPI<- max(EPIChange) - min(EPIChange)
RangeIPI<- max(IPIChange) - min(IPIChange)
#Print the ranges
RangeEPI
RangeIPI

#Compute the sample coefficients of variation of the % changes
#and express them in percentages
CVEPI<- sd(EPIChange)/mean(EPIChange) * 100
CVIPI<- sd(IPIChange)/mean(IPIChange) * 100
#Print the sample coefficients of variation
CVEPI
CVIPI

#Compute the 80th percentiles of the % changes
Q80EPI<- quantile(EPIChange, 80/100)
Q80IPI<- quantile(IPIChange, 80/100)
#Print the 80th percentiles 
Q80EPI
Q80IPI 

#Compute the first quarters and the third quarters of the % changes
Q13EPI<- quantile(EPIChange, c(25/100, 75/100))
Q13IPI<- quantile(IPIChange, c(25/100, 75/100))
#Print the first quarters and the third quarters
Q13EPI
Q13IPI

#Compute the second quarters (the medians) of the % changes
Q50EPI<- quantile(EPIChange, 50/100)
Q50IPI<- quantile(IPIChange, 50/100)
#Print the second quarters (the medians)
Q50EPI
Q50IPI 

#Compute the interquartile ranges of the % changes
IQREPI<- IQR(EPIChange)
IQRIPI<- IQR(IPIChange) 
#Print the interquartile ranges
IQREPI
IQRIPI

####Skewness and Kurtosis####
#Import and load the the package "moments"
install.packages("moments")
library(moments)

#Compute the skewness of the % changes
SkewEPI<-skewness(EPIChange)
SkewIPI<-skewness(IPIChange)
#Print the skewness
SkewEPI
SkewIPI

#Import and load the the package "EnvStats"
install.packages("EnvStats")
library(EnvStats)

#Compute the kurtosis of the % changes
KurEPI<-kurtosis(EPIChange, excess = FALSE)
KurIPI<-kurtosis(IPIChange, excess = FALSE)
#Print the kurtosis 
KurEPI
KurIPI

#Compute the excess kurtosis of the % changes
EKurEPI<-kurtosis(EPIChange, excess = TRUE)
EKurIPI<-kurtosis(IPIChange, excess = TRUE)
#Print the excess kurtosis 
EKurEPI
EKurIPI

####Measures of Linear Relationship####
#Compute the sample covariance between EPI and IPI % changes
CovPI<- cov(EPIChange, IPIChange)
#Note that the function "cov" in R gives the sample covariance
#Print the sample covariance
CovPI

#Compute the sample correlation coefficient between EPI and IPI 
#% changes
CorrPI<- cor(EPIChange, IPIChange)
#Note that the function "cor" in R gives the sample correlation
#coefficient
#Print the sample correlation coefficient 
CorrPI 

#Plot a scatterplot for EPI and IPI % changes using ggplot
ggplot() + 
  geom_point(aes(x=IPIChange, y=EPIChange))+ 
  #geom_point gives a scatterplot
  #the aesthetic specification "aes" is used to specify 
  #the x-axis and the y-axis.
  xlab("Import Price Index % Changes") + #label the x-axis
  ylab("Emport Price Index % Changes") + #label the y-axis
  ggtitle("International Trade Price Indexes % Changes") +
  theme_update(plot.title = element_text(hjust = 0.5))
