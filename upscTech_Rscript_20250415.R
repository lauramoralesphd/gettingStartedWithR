#Install and/or load packages that will be used in this script####
#Packages only need to be installed once on your computer
#Install package for violin plots
install.packages("vioplot")
#Load package for violin plots
library(vioplot)

#Set working directory and load data####
#For Windows, you cannot simply copy-paste the directory
#Windows directories use back-slashes, which R does not understand
#Here is an example of what it would look like if you copy-paste the directory
setwd("C:\Users\lams0004\OneDrive - Sveriges lantbruksuniversitet\teaching")
#Need to change Windows-formatted black-slashes to double back-slashes or forward-slashes
#Not an issue if you use Mac or Linux
setwd("C:\\Users\\lams0004\\OneDrive - Sveriges lantbruksuniversitet\\teaching")
setwd("C:/Users/lams0004/OneDrive - Sveriges lantbruksuniversitet/teaching")

#Load data file with "clean" data that we generated last week
#read.table() function for opening text files with known delimiter (tab-delimited, in this case)
#file= is the file name of your phenotype file
#sep="\t" means that the file is tab-delimited
#header=T means that the first row contains the column names
#as.is=T will load data as-is. If as.is=F, then R will often do things like turn characters into factors
phen <- read.table(file="upscTech_droughtData_clean_20250408.txt", sep="\t", header=T, as.is=T)

#Examples of sub-setting data frames using the which() function####
#Identify rows in the Treatment column of the phen data frame that are "drought"
#Use == when you are looking for an exact match (can only contain one value)
which(phen$Treatment=="drought")
#Identify rows in the Timepoint column of the phen data frame that are 3
which(phen$Timepoint==3)
#Identify rows in the Timepoint column of the phen data frame that are either 3 or 4
#Use %in% when you are looking for a match that could be more than one value
which(phen$Timepoint %in% c(3,4))

#Subset phen data frame for observations from the drought treatment only
#Use the which() function to select rows where the Treatment column contains "drought"
phen_d <- phen[which(phen$Treatment=="drought"),]

#Subset phen data frame for observations from the control treatment only
#Use the which() function to select rows where the Treatment column contains "control"
phen_c <- phen[which(phen$Treatment=="control"),]

#Subset phen data frame so that there are no missing observations in Height
#Use the which() function to select rows where Height is NA
#Use - to remove elements from a vector or from a column or row in a data frame of matrix
is.na(phen$Height)
phen_noNA <- phen[-which(is.na(phen$Height)),]

#Step 3: Analyze clean data####
#Student's t-test for Height vs Treatment
#Use the t.test() function
#data is the phen data frame
#formula is specified as y~x, here Height~Treatment
t.test(data=phen, height ~ Treatment)
mod1 <- t.test(data=phen, Height~Treatment)
#see t-test statistics and output in mod1
mod1

#ANOVA: Height = Genotype + Treatment
#Use aov() function for ANOVA, save the output to mod2 variable
#data is the phen data frame
#formula is specified as y~x1+x2+x..., with interactions denoted as x1*x2
mod2 <- aov(data=phen, formula= Height ~ Genotype + Treatment)

#Use summary() function to get statistics from mod2 variable
summary(mod2)

#Usually linear model functions will have different kinds of statistics
#Use $ to see what the different kinds of results output from aov(), with coefficients and effects as an example
mod2$coefficients
mod2$effects

#Step 4: visualize clean data####
#Histogram of height using hist() function####
#x is the data
#xlab is the x-axis label
#main is the title
hist(x=phen$Height, xlab="Height (cm)", main="Distribution of height")
#You can also change the size of the bins (bars in the histogram) with the breaks argument
#Use the seq() function to generate numbers in a range by specific increments
hist(phen$Height, xlab="Height (cm)", main="Distribution of height",
     breaks=seq(from=0, to=120, by=1))

#Add black dot-dashed vertical line showing the overall mean with abline() function
#v means vertical line (h means horizontal)
#mean() function for calculating mean of height, with na.rm=T to remove missing data
#col is line color, black
#lwd is line width, 2x bigger than default
#lty is line type, 6 means dot-dash
abline(v=mean(phen$Height, na.rm=T), lwd=2, lty=6)

#Add blue dotted vertical line showing mean height for control conditions
#v means vertical line (h means horizontal)
#mean() function for calculating mean of height, with na.rm=T to remove missing data
#use which() function to select rows where treatment=control
#col is line color, blue
#lwd is line width, 2x bigger than default
#lty is line type, 3 means dot
abline(v=mean(phen$Height[which(phen$Treatment=="control")]))
abline(v=mean(phen_c$Height, na.rm=T), col="blue", lwd=2)
abline(v=mean(phen_d$Height, na.rm=T), col="red", lwd=2)
abline(v=mean(phen$Height[which(phen$Treatment=="control")], na.rm=T), col="blue", lwd=2, lty=3)
#Add red dashed vertical line showing mean height for drought conditions
#mean() function for calculating mean of height, with na.rm=T to remove missing data
#use which() function to select rows where treatment=drought
#col is line color, red
#lwd is line width, 2x bigger than default
#lty is line type, 5 means dash

#Box plot of height under control vs drought using boxplot() function####
#formula is written as y ~ x. Here, y is in the Height column and x is in the Treatment column of the phen data frame
#xlab and ylab are the x-axis and y-axis labels, respectively
#col is the box color, with blue and red for control and drought, respectively
#main is the title
boxplot(formula=phen$Height~phen$Treatment, xlab="Treatment", ylab="Height (cm)",
        main="Height vs. treatment", col=c("blue","red"))

#Example of changing the order of character values, with "drought" first and "control" second
#By default, R will sort the unique character values alphabetically 
#Make a new column for treatment as a factor
phen$TreatmentFactor <- NA
phen$TreatmentFactor <- as.factor(phen$Treatment)
#Set the order of the TreatmentFactor column using the levels() function
levels(phen$TreatmentFactor) <- c("drought","control")
#Make boxplot to see if it worked
boxplot(formula= phen$Height ~ phen$TreatmentFactor)

#Plot height over time####
#use plot() function
#x and y are the Timepoint and Height columns in phen, respectively
#xlab, ylab, and main are the x-axis, y-axis, and title labels
plot(x=phen$Timepoint, y=phen$Height, xlab="Time point", ylab="Height (cm)", main="Height vs. time")
#plot regression lines for Height vs. Timepoint overall, drought, and control
#use abline() function with the regression line specified with the lm() function
abline(lm(data=phen, formula=Height ~ Timepoint))
#Can also save linear model output from lm() and see summary statistics with summary() function
mod3 <- lm(data=phen, formula=Height~Timepoint)
summary(mod3)
#Can plot regression line from mod3 directly using abline() function
abline(mod3)
#Plot regression line for drought data only
mod4 <- lm(data=phen[which(phen$Treatment=="drought"),], Height~Timepoint)
abline(mod4,col="red")
#Plot regression line for control data only
mod5 <- lm(data=phen[which(phen$Treatment=="control"),], Height~Timepoint)
abline(mod5,col="blue")

#A few examples of other things that you can add to an existig plot
#add datapoints to specific plot
points() 
#add lines to existing plot
lines() 

#Violin plot of height vs treatment using vioplot() function####
vioplot(formula= phen$Height ~ phen$Treatment)
#Add individual data points to the violin plot with stripchart() function
#method="jitter" means that the points will be slightly offset from each other for better visualization
#add=T means that this plot will be added to the existing plot, in this case the violin plot
#vertical=T means that the plot should be arranged vertically, like the violin plot
#pch can be used in many plotting functions to change the style of the points. pch=16 means that it is a solid circle
#cex can be used in many plotting functions to change the size of the points. cex=1 is standard, cex<1 is smaller, cex>1 is bigger
stripchart(phen$Height~phen$Treatment,
           method="jitter", add=T, vertical=T,
           pch=16, cex=0.5)

#Plot height vs time for each genotype as its own line using a for-loop####
#Make a blank plot, with two different ways of doing it
#Make a plot with invisible data points for Height (y) vs. Timepoint (x)
#col=NA means that the color of the points should be nothing
plot(x=phen$Timepoint, y=phen$Height, col=NA)
#Make a plot with no data points, with x=NA and y=NA
#Need to set x- and y-axis so that the datapoints will fit in the plot with xlim and ylim
#xlim=c(1,9) sets the x-axis limits from 1 to 9
#ylim=c(min(phen$Height,na.rm=T),max(phen$Height,na.rm=T)) sets the y-axis limits from the minimum to the maximum of the Height column
#If you have missing data, need to use na.rm=T argument with min(), max(), mean(), median(), sd() functions
plot(x=NA,y=NA, xla="Timepoint", ylab="Height" ,xlim=c(1,9), 
     ylim=c(min(phen$Height,na.rm=T),max(phen$Height,na.rm=T)))
#Use a for-loop to plot a line of Height vs Timepoint for each tree separately
for(i in unique(phen$Tree)){
  lines(y=phen$Height[which(phen$Tree==i)],
        x=phen$Timepoint[which(phen$Tree==i)])
}
