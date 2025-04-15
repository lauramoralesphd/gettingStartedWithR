#Install and/or load packages that will be used in this script####
#Packages only need to be installed once on your computer
install.packages("corrplot")
library(corrplot)

#Set working directory and load data####
#For Windows, you cannot simply copy-paste the directory
#Windows directories use back-slashes, which R does not understand
#Need to change Windows-formatted black-slashes to double back-slashes or forward-slashes
#Not an issue if you use Mac or Linux
setwd("C:\Users\lams0004\OneDrive - Sveriges lantbruksuniversitet\teaching")
setwd("C:\\Users\\lams0004\\OneDrive - Sveriges lantbruksuniversitet\\teaching")
setwd("C:/Users/lams0004/OneDrive - Sveriges lantbruksuniversitet/teaching")

#Load data file
#read.table() function for opening text files with known delimiter (tab-delimited, in this case)
#file= is the file name of your phenotype file
#sep="\t" means that the file is tab-delimited
#header=T means that the first row contains the column names
#as.is=T will load data as-is. If as.is=F, then R will often do things like turn characters into factors
phen <- read.table(file="C:/Users/lams0004/OneDrive - Sveriges lantbruksuniversitet/teaching/upscTech_droughtData_clean_20250408.txt", sep="\t", header=T, as.is=T)

#We will want to look at height of each genotype under drought or control conditions
#Add new column to phen data frame with genotype X treatment interaction with the paste() function
#sep is the character used to join Genotype and Treatment, in this case "x"
phen$GenotypeTreatment <- paste(phen$Genotype,phen$Treatment, sep="x")

#Examples of sub-setting data frames####
#Subset phen data frame for observations from the drought treatment only
#Use the which() function to select rows where the Treatment column contains "drought"
phen_d <- phen[which(phen$Treatment=="drought"),]
#Subset phen data frame for observations from the control treatment only
#Use the which() function to select rows where the Treatment column contains "control"
phen_c <- phen[which(phen$Treatment=="control"),]
#Subset phen data frame so that there are no missing observations in Height
#Use the which() function to select rows where Height is NA, and remove them with -
phen_noNA <- phen[-which(is.na(phen$Height)),]

#Step 3: Analyze clean data####
#Student's t-test for Height vs Treatment
#Use the t.test() function
#data is the phen data frame
#formula is specified as y~x, here Height~Treatment
mod1 <- t.test(data=phen, Height~Treatment)
#see t-test statistics and output in mod1
mod1

#ANOVA: Height = Genotype + Treatment + Genotype*Treatment + Timepoint
#Use aov() function for ANOVA, save the output to mod2 variable
#data is the phen data frame
#formula is specified as y~x1+x2+x..., with interactions denoted as x1*x2
mod2 <- aov(data=phen, formula=Height~Genotype+Treatment+Genotype*Treatment+Timepoint)
#Use summary() function to get statistics from mod2 variable
summary(mod2)

#Step 4: visualize clean data####
#Histogram of height using hist() function####
#x is the data
#xlab is the x-axis label
#main is the title
hist(x=phen$Height, xlab="Height (cm)", main="Distribution of height")
#Add black dot-dashed vertical line showing the overall mean with abline() function
#v means vertical line (h means horizontal)
#mean() function for calculating mean of height, with na.rm=T to remove missing data
#col is line color, black
#lwd is line width, 2x bigger than default
#lty is line type, 6 means dot-dash
abline(v=mean(phen$Height, na.rm=T), col="black", lwd=2, lty=6)
#Add blue dotted vertical line showing mean height for control conditions
#v means vertical line (h means horizontal)
#mean() function for calculating mean of height, with na.rm=T to remove missing data
#use which() function to select rows where treatment=control
#col is line color, blue
#lwd is line width, 2x bigger than default
#lty is line type, 3 means dot
abline(v=mean(phen$Height[which(phen$Treatment=="control")], na.rm=T), col="blue", lwd=2, lty=3)
#Add red dashed vertical line showing mean height for drought conditions
#mean() function for calculating mean of height, with na.rm=T to remove missing data
#use which() function to select rows where treatment=drought
#col is line color, red
#lwd is line width, 2x bigger than default
#lty is line type, 5 means dash
abline(v=mean(phen$Height[which(phen$Treatment=="drought")], na.rm=T), col="red", lwd=2, lty=5)

#Box plot of height under control vs drought using boxplot() function####
#formula is written as y ~ x. Here, y is in the Height column and x is in the Treatment column of the phen data frame
#xlab and ylab are the x-axis and y-axis labels, respectively
#col is the box color, with blue and red for control and drought, respectively
#main is the title
boxplot(formula=phen$Height~phen$Treatment, xlab="Treatment", ylab="Height (cm)",
        main="Height vs. treatment", col=c("blue","red"))

#Boxplot of height under control vs drought for each genotype using boxplot() function####
#formula is written as y ~ x. Here, y is in the Height column and x is in the GenotypeTreatment column of the phen data frame
#xlab and ylab are the x-axis and y-axis labels, respectively
#main is the title
#col is the box color, with alternating blue and red for control and drought, respectively, for each genotype
#use rep() function to repeat "blue" and "red" 16 times (16 genotypes)
#las rotates the labels on the x-xis tick marks, and las=2 rotates them vertically
boxplot(formula=phen$Height~phen$GenotypeTreatment, las=2, main="Height vs. Genotype*Treatment",
        ylab="Height (cm)", xlab="", col=rep(c("blue","red"),16))
#The margins of the plotting area need to be increased.
#The par() function can be used to change many plotting parameters
#Within par(), the mar() function sets the size of the margins outside of the plot. 
#The margins are set in mar() with a numeric vector c(bottom, left, top, right) and the default is c(5, 4, 4, 2) + 0.1
par(mar=(c(10,4,4,2)))
#Make boxplot again. Now the x-axis labels fit.
boxplot(formula=phen$Height~phen$GenotypeTreatment, las=2, main="Height vs. Genotype*Treatment",
        ylab="Height (cm)", xlab="", col=rep(c("blue","red"),16))
#Use the abline() function to add horizontal lines for overall mean, control mean, and drought mean height
#h means horizontal line
abline(h=mean(phen$Height, na.rm=T))
abline(h=mean(phen$Height[which(phen$Treatment=="control")], na.rm=T),col="blue")
abline(h=mean(phen$Height[which(phen$Treatment=="drought")], na.rm=T),col="red")

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
