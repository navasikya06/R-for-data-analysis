#Lecture Week 10: Example 12
#Fit a Normal distribution to the returns data 
#by estimating its mean and standard deviation 
#Use the Maximum Likelihood Estimation (MLE) method

#Install and load the "MASS" package
install.packages("MASS") 
library(MASS)

#Create the returns data and save them as "Returns"
Returns<- c(0.03, -0.021, 0.028, 0.012, -0.018)

#Fit a Normal distribution to the returns data in "Returns"
#by estimating its mean and standard deviation 
#using the MLE method
MLE<-fitdistr(Returns, densfun="normal") 
#Print the fitting result
MLE

#Estimation Results
#> MLE
#mean           sd     
#0.006200000   0.021912553 
#(0.009799592) (0.006929358)

