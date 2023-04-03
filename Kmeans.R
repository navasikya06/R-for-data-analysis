#The k-means exercise of the tutorial in Week 5 
#"Supervised vs. Unsupervised Learning with R" 
#is mainly based on the following and some relevant
#references therein:
#https://www.kaggle.com/xvivancos/tutorial-clustering-wines-with-k-means
#This exercise presents an application of unsupervised 
#learning, say the k-means clustering algorithm, 
#to cluster different types of wines in a Wine data set.

#note that in unsupervised learning, the data points are
#not labeled. In other words, there is no outcome or
#target variable to be predicted.
#this is different from supervised learning.

#### install and load R packages ####
install.packages(c("tidyverse", "corrplot", "gridExtra",
                   "GGally", "knitr"))

library(tidyverse)
library(corrplot) 
#visualize a correlation matrix
#https://cran.r-project.org/web/packages/corrplot/corrplot.pdf
library(gridExtra) 
#work with grid graphics
#https://cran.r-project.org/web/packages/gridExtra/gridExtra.pdf
library(GGally) 
#Extension to ggplot2
#https://www.rdocumentation.org/packages/GGally/versions/1.5.0
library(knitr)
#general-purpose package for dynamic report generation
#https://www.rdocumentation.org/packages/knitr/versions/1.30
#allow reproducible research in R and 
#integration of R code with other programming languages
#https://en.wikipedia.org/wiki/Knitr

#### import, visualize and analyse the data ####

###### import and view the data ####

wines <- read.csv("kmeans_wine.csv")
#import the Wine data set 

wines 
#view the data
#the "Wine" column is not needed 
#as the k-means algorithm belongs to unsupervised
#learning and works with unlabeled data.

wines <- wines[, -1]
#remove the "Wine" column (column 1)

kable(head(wines))
#view the first few rows of the data 
#the kable() function in the knitr package
#returns a single table for a single data frame.
? kable
#https://www.rdocumentation.org/packages/knitr/versions/1.33/topics/kable

kable(tail(wines))
#display the last few rows of the data in a single table

kable(summary(wines))
#evaluate the summary statistics for columns in the
#data frame and display them in a single table

str(wines)
#view the structure of the data

###### explore and visualize the data ####

#create a histogram for each attribute
#or each column in the data frame 

wines %>%
  gather(Attributes, value, 1:13) %>%
  #collect the names of columns 1:13 and 
  #place them in the new variable/column "Attributes"
  #collect the values of columns 1:13 and
  #place them in the new variable/column "value"
  #remove columns 1:13
  #note that instead of using the names of columns 1:13,
  #the numbers of those columns are used 
  #when removing those columns using the gather() function.
  #https://uc-r.github.io/tidyr
  ggplot(aes(x=value, fill=Attributes)) +
  #specify the x-axis in each of the histograms 
  #using the values in the "value" variable and 
  #fill in each of the histograms in accordance with
  #the "Attributes" variable
  geom_histogram(colour="black", show.legend=FALSE) +
  #create the histogram for each attribute with 
  #color "black" and without legend
  facet_wrap(~Attributes, scales="free_x") +
  #create a panel of multiple plots in accordance 
  #with the "Attributes" variable, and
  #the scales in the x-axis of different plots are free, 
  #(i.e. the scales in the x-axis 
  #of different plots can vary)
  #https://ggplot2.tidyverse.org/reference/facet_wrap.html
  labs(x="Values", y="Frequency",
       title="Wines Attributes - Histograms") +
  #label the x-axis and the y-axis, and
  #give the whole plot a title 
  theme_bw()
  ? theme_bw
  #set a dark-on-light theme 
  #https://ggplot2.tidyverse.org/reference/ggtheme.html
#anyway, the plot becomes colorful and nicer  
#by clicking "Zoom" under the "Plots" window. 

#create a density plot for each Attribute
wines %>%
  gather(Attributes, value, 1:13) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_density(colour="black", alpha=0.5, 
               show.legend=FALSE) +
  #create the density plot for each attribute
  #with color "black", opacity (or alpha) 50%,
  #and without legend
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Density",
       title="Wines Attributes - Density plots") +
  theme_bw()
#again, the plot becomes colorful and nicer  
#by clicking "Zoom" under the "Plots" window. 

# create a boxplot for each Attribute  
wines %>%
  gather(Attributes, values, c(1:4, 6:12)) %>%
  #collect columns 1:4, 6:12 and place them
  #in "Attributes"
  #collect the values in columns 1:4, 6:12
  #and place them in "values"
  #do not include the "magnesium" and "proline" columns
  #for their high values may worsen the visualization
  ggplot(aes(x=reorder(Attributes, values, FUN=median), 
             y=values, fill=Attributes)) +
  #specify the x-axis of the boxplot using names
  #in "Attributes", and
  #the reorder() function re-orders the levels/names
  #in "Attributes"  such that the values returned 
  #by FUN (say median) are in increasing order,
  #(i.e., the levels/names in "Attributes are re-ordered
  #by the medians of their values in ascending order).
  #https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/reorder.default
  #specify the y-axis of the boxplot using the values
  #in "values", and fill in the boxplot in accordance
  #with the names in "Attributes"
  geom_boxplot(show.legend=FALSE) +
  #create the boxplot without legend 
  labs(title="Wines Attributes - Boxplots") +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  #do not include titles in the x-axis and the y-axis
  ylim(0, 35) +
  #specify the lower and upper limits of the y-axis
  coord_flip()
  #Flip cartesian coordinates so that 
  #horizontal becomes vertical, and vice versa
  #https://www.rdocumentation.org/packages/ggplot2/versions/1.0.0/topics/coord_flip
  ? coord_flip
#a nice boxplot is created and can be viewed without
#using "Zoom" under "Plots".

#display the relationship between the different attributes
#using correlation matrices

corrplot(cor(wines), type="upper", method="ellipse", 
         tl.cex=0.9)
#create a correlation matrix
#a nicer correlation matrix is displayed using
#"Zoom" under "Plots". 
# a strong linear (positive) correlation between 
#Phenols and Flavanoids, say the correlation
#is close to one.

#fit a linear equation describing the linear correlation
#between Phenols and Flavanoids, and 
#display the linear equation in a scatterplot 

ggplot(wines, aes(x=Phenols, y=Flavanoids)) +
  geom_point() +
  #create the scatterplot 
  geom_smooth(method="lm", se=FALSE) +
  #fit the linear equation using the lm function
  #without including the standard errors of the
  #estimates
  labs(title="Wines Attributes",
       subtitle="Relationship between Phenols 
       and Flavanoids") +
  theme_bw()
? lm 

###### data preparation for the k-means algorithm ####

#data normalization using the scale function
winesNorm <- as.data.frame(scale(wines))

#create a scatterplot for the original data
p1 <- ggplot(wines, aes(x=Alcohol, y=Malic.acid)) +
  geom_point() +
  labs(title="Original data") +
  theme_bw()

#create a scatterplot for the normalized data
p2 <- ggplot(winesNorm, aes(x=Alcohol, y=Malic.acid)) +
  geom_point() +
  labs(title="Normalized data") +
  theme_bw()

#combine the two scatterplots as subplots
grid.arrange(p1, p2, ncol=2)
#the points in the normalized data and the original data
#are the same. 
#the only change is the scales of the two axes.

#### run the k-means algorithm ####

###### run the algorithm and extract the output ####

#Run the k-means clustering algorithm
#with k = 2

set.seed(1234)
wines_k2 <- kmeans(winesNorm, centers=2)

?set.seed
#set the random seeds for the initial centroids

wines_k2 
#view the output from the k-means algorithm

wines_k2$cluster
#extract the cluster to which each point 
#is allocated

wines_k2$centers
#extract the centroids of the two clusters 

wines_k2$size
#extract the number of points in each cluster 

#In addition, the kmeans() function also returns
#betweenss, withinss, tot.withinss and totss

#the between-cluster sum of squares (betweenss):
#ideally, we would like this to be as large as possible
#indicating heterogeneity between clusters.

#the within-cluster sum of squares (withinss):
#ideally, we would like this to be as small as possible
#indicating homogeneity within each cluster.
#if there are k clusters, there will be k withinss.

#total within-cluster sum of squares (tot.withinss):
#the sum of all the withinss

#the total sum of squares (totss): 
#the sum of betweenss and tot.withinss

wines_k2$betweenss
#extract the between-cluster sum of squares

wines_k2$withinss
#extract the within-cluster sum of squares

wines_k2$tot.withinss
#extract the total within-cluster sum of squares

wines_k2$totss
#extract the total sum of squares 

###### how to choose k ? ####

#use graphical methods to choose k giving
#the best partition

#plot betweenss and tot.withinss against k

bss <- numeric()
wss <- numeric()

#run the k-means algorithm for different values of k 
#say, k is from 1 to 10 with an increment of 1.

set.seed(1234)

for(k in 1:10){
  bss[k] <- kmeans(winesNorm, centers=k)$betweenss
  wss[k] <- kmeans(winesNorm, centers=k)$tot.withinss
  #for each k, evaluate betweenss and tot.withinss
}

#plot betweenss against k
p3 <- qplot(1:10, bss, geom=c("point", "line"), 
            xlab="Number of clusters", 
            ylab="Between-cluster sum of squares") +
  #the x-axis is 1:10; the y-axis is bss 
  #geom=c("point", "line") specifies that the plot
  #is given by lines joining points. 
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  #scale_x_continuous specifies the scales 
  #in the x-axis. 
  #https://ggplot2.tidyverse.org/reference/scale_continuous.html
  theme_bw()

#plot withinss against k
p4 <- qplot(1:10, wss, geom=c("point", "line"),
            xlab="Number of clusters", 
            ylab="Total within-cluster sum of squares") +
  #the x-axis is 1:10; the y-axis is wss 
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()

#combine the two graphs as subplots
grid.arrange(p3, p4, ncol=2)

#elbow criterion: choose the value of k such
#that adding another cluster would not 
#improve much the clustering result,
#(i.e., at the point when an increase in
#bss starts to slow down or at the point 
#when a decrease in wss starts to slow down)

###### run the result using the chosen k #### 

#run the k-means algorithm with k=3
set.seed(1234)

wines_k3 <- kmeans(winesNorm, centers=3)

aggregate(wines, by=list(wines_k3$cluster), mean)
#evaluate the mean of each cluster
#the aggregate() function splits the data into subsets,
#computes summary statistics and returns the result
#in a convenient form.
#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/aggregate

#visualize the clustering result using ggpairs in GGally
ggpairs(cbind(wines, Cluster=as.factor(wines_k3$cluster)),
        columns=1:6, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") +
  #ggpairs is used to make a matrix of plots
  #Cluster=as.factor(wines_k3$cluster) converts
  #the data type of the column wines_k3$cluster
  #to factors, and save it as the variable Cluster.
  #the function cbind() combines the data frame wines
  #with the column wines_k3$cluster by columns
  #columns=1:6 extracts the first six columns in 
  #the combined data frame to create the matrix
  #of plots. 
  #colour=Cluster specifies the color of points
  #by clusters. 
  #opacity (alpha) is set as 50%.
  #lower specifies the lower triangular part
  #of the matrix of plots,
  #say the lower triangular part contains
  #plots with points for a continuous variable.
  #upper specifies the upper triangular part
  #of the matrix of plots,
  #say the upper triangular part is blank.
  #axisLabels="none" for no axis labels
  #switch="both" specifies that the top labels
  #will be displayed to the bottom and that 
  #the right-hand side labels will be displayed
  #to the left.
  #https://www.rdocumentation.org/packages/https://www.rdocumentation.org/packages/GGally/versions/1.5.0/topics/ggpairs/versions/1.5.0/topics/ggpairs
  theme_bw()
  #set a dark-on-light theme