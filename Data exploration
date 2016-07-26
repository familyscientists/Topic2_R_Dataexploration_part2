##################################################
## Data scientists family group
## R Tutorial - Data exploration part 2
## Rodrigo Donoso - Vicente Figueroa - Max Rivera
##
##################################################

#Data exploration part 2 includes specific code to explore data sets. Useful when you want to explore and do basic 

######################################################################################################
######                          Specifying  files directory                                        ###### 
######################################################################################################

#already includes in previous code. But this is a good option in case you do not want to code the entire name of the file

rm(list=ls())
setwd("~/Desktop/Clases_familyscientists/Topic2_R/02 Data exporation 2")

#####################################################################################################
######                     Iris data set analysis                                             ###### 
#####################################################################################################

# data exploration
idf <- read.csv("~/Desktop/Clases_familyscientists/Topic2_R/02 Data exporation 2/Iris.csv")
dim(idf)                                      # check the dimension of Iris dataset
names(idf)                                    # chack the names of Iris data
str(idf)                                      # check the structure of Iris data
attributes(idf)                               # check the attributes of data
idf[1:10, "SepalLength"]                    # retrieve the first ten values of SepalLength column
idf$SepalLength[1:10]

# Explore individual variables
summary(idf)                                  # check the distribution of every numeric variable                    
mean(idf$SepalLength)                       # get the averge value of SepalLength column
median(idf$SepalLength)                     # get the median value of SepalLength column
range(idf$SepalLength)                      # get the minimum and maximum of SepalLength
quantile(idf$SepalLength)                   # calculate 0%, 25%,50%,75% and 100% quantile
quantile(idf$SepalLength, c(.1,.3,.65))     # calculate specific quantile
var(idf$SepalLength)                     
hist(idf$SepalLength)                       # check the distribution with histogram
plot(density(idf$SepalLength))              # plot density of SepalLength
fidf = table(idf$Species)                     # calculate the frequency of factors
print(fidf)
a <- pie(fidf, main="Pie family scientists")                                     # pie chart
b <- barplot(fidf, main="Barplot family scientists")                             # bar chart
#remember to use help(xxx) in case you want to add more features to your graphs

# Explore Multiple Variables
cov(idf$SepalLength,idf$PetalLength)      # cov(A,B)calculates covariance between A and B
cor(idf[,1:4])                                # calculate correlation between any two variables
help("aggregate")
aggregate(SepalLength~Species, summary,data=idf) # compute the stats of sepalLength of every 


#####################################################################################################
######                          Iris data set analysis - basic visualization                  ######  
#####################################################################################################

graphics.off() #Use to clear all Plots  

#To display your graphs on 1 screen
par(mfrow=c(2,3)) # 2 row, 3 columns of plots 

# Species
help(boxplot) #in case you want to add color or extra feautres
c <- boxplot(SepalLength~Species, data=idf)      # show the median, first and third quantile of a 

# distribution, and outliers
help(plot) #if you want guide about the parameters of plot
help(with)
d <- with(idf,plot(SepalLength, SepalWidth, col=Species, pch=as.numeric(Species))) #col= color regarding species, pch=changes the form of each variable: circle, triangle and cross 
with(idf,plot(SepalLength, SepalWidth, col=Species)) #here the same graph, without the pch option

help(jitter)
# draw a scatter plot of two numeric variables (2 options)
e <- plot(idf$SepalLength,idf$SepalWidth)
f <- plot(jitter(idf$SepalLength),jitter(idf$SepalWidth))
# to add a small noise to the data in order to


# If you want a classic view:
par(mfrow=c(1,1)) # 1 row, 1 columns of plots 

# show some data may overlap with others
pairs(idf)                                    # create a matrix of scatter plots

# Save Charts into File
pdf(file="~/Desktop/Clases_familyscientists/Topic2_R/02 Data exporation 2/DataExplorationPlot.pdf")
# save as a PDF file
with(idf,plot(SepalLength, SepalWidth, col=Species, pch=as.numeric(Species)))    



############################################################################################
######                   EXTRAS    ovariance Visualization Gift                     #######
############################################################################################

#It displays a colored Matrix to represent covariance between the variables

idf_cor <- idf[,c(-5)] #Covariance of everything but 5th column (Species)
M <- cor(idf_cor, use="complete")
library(corrplot) #package corrplot 
corrplot(M, method = "number") #plot matrix






