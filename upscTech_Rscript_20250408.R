#This is an R script created during the UPSCtech seminar "Getting started with R" on 2024-04-08
#Shortcuts####
#Type arrow symbol <-
##Windows: Alt + -
##Mac: Option + -
#Run a line of code
##Windows: Ctrl + Enter
##Mac: Command + Enter

#Practicing the basics####
#Numbers: example of typing 1
1.0 
1.0000
1
#Characters: example of typing characters with letters and/or numbers
"a"
"word"
"abc123"
"123"
"this is a word"
"these are letters a, b, c and numbers 1, 2, 3"
#What data type does R think 1 is?####
#Is 1 numeric? This will return TRUE if 1 is numeric and FALSE if it is not
is.numeric(1)
#Is 1 an integer? This will return TRUE if 1 is an integer and FALSE if it is not
is.integer(1)
#Convert 1 into integer data type, save it as the variable integer1 for further analysis. You can save the variable with whatever name you want!
integer1 <- as.integer(1)
#Is integer 1 an integer? TRUE=yes, it is an integer. FALSE=no, it is not an integer
is.integer(integer1)
#Convert 1 into character daya type, save it as the variable character1 or whatever name you want
character1 <- as.character(1)
#Is character 1 an integer? TRUE=yes, it is a character. FALSE: no, it is not a character
is.character(character1)
#Look at character1 and/or is.character(1). You should see that 1 is in quotation marks "1"
character1
is.character(1)

#Creating vectors####
#Create a vector called v1 (or another name) and include a combination of numbers and characters. Characters need to be in quotation marks "abc"
v1 <- c(1,2,3,"a",NA,"b","c",NA)
#Look at the v1 vector. Are the numbers still numeric or were they converted to characters?
v1
#Is v1 a numeric vector? No (FALSE)
is.numeric(v1)
#Is v1 a character vector? Yes (TRUE)
is.character(v1)
#Create an empty vector v0 (or another name)
v0 <- NULL
#Is v0 empty (NULL)? Yes (TRUE)
is.null(v0)
#Is v0 numeric or character? No (FALSE)
is.numeric(v0)
is.character(v0)
#Is v1 empty (NULL)? No (FALSE)
is.null(v1)
#Can v1 be converted into numeric? Not completely. "a", "b", and "c" will be set to missing (NA) and you will get a warning message
as.numeric(v1)
#Create a numeric vector v2
v2 <- c(1,2,3,4,5)
#Can v2 be converted into character? Yes
as.character(v2)

#Load packages, install if necessary####
#Only need to install a package once on a device
#Install "stringr" package for converting commas to decimal points
#Install "readxl" package for loading Excel files
install.packages("stringr")
install.packages("readxl")
#Load "stringr" and "readxl" packages
library(stringr)
library(readxl)

#Playing with a real dataset####
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
phen <- read.table(file="C:/Users/lams0004/OneDrive - Sveriges lantbruksuniversitet/teaching/upscTech_droughtData_raw_20250408.txt", sep="\t", header=T, as.is=T)
#Example of loading same data, but in Excel format
phenx <- read_excel(path="upscTech_droughtData_raw_20250408.xlsx", sheet="Phenotypes")
#Example of loading same data, but in comma-separated format
#IMPORTANT! Need to check if your computer settings save csv files with commas or semi-colons
#read.csv() function is for reading comma-separated csv files
#read.csv2() function is for semi-colon-separatd csv files 
phenc <- read.csv("upscTech_droughtData_raw_20250408.csv")

#Examples for subsetting a data frame####
#For data frames, you can select specific rows and columns inside of square brackets yourDataFrame[rows,columns]
#Get first ten rows of phen data frame
phen[1:10,]
#Get first two columns of phen data frame
phen[,1:2]
#Get first row and second column of phen data frame
phen[1,2]
#Get column named "Treatment" from phen data frame 
phen[,"Treatment"]
#Example of a typo: try to get "Treatment" column from phen data frame
phen[,"Tretment"]
#You can also use the dollar sign $ symbol to get columns from data frames. Example of Treatment column of phen data frame
phen$Treatment
#Get first ten rows from Treatment column of phen data frame
phen$Treatment[1:10]

#Step 1: does the data look like we expected? (Jump to Step 2 to fix problems with data)####
#What data types were assigned to our variables?
#One way: look at "phen" under the "Data" section on the upper right side of this window
#Other way: look at the structure of the phen data frame
str(phen)

#First problem with the data
#Height should be numeric data type, but R read it as character...why?
#Because the decimal points were not consistent! Sometimes commas were used, other times periods were used

#Do we have the expected number of genotypes? There should be 15.
#Use unique() function to look at the unique values in the Genotype column
unique(phen$Genotype)
#Use length() and unique() functions to get the number of unique genotype names
length(unique(phen$Genotype))
#No...there are 28 unique genotype names
#Problem: there are synonymous genotype names, sometimes with all-capitals and sometimes not

#Do we have the expected number of treatments? There should be two, drought vs control
#Use unique() function to look at the unique values in the Treatment column
unique(phen$Treatment)
#Use length() and unique() functions to get the number of unique treatments
length(unique(phen$Treatment))
#No...there are 4 unique treatments
#Problem: there are synonymous treatment names: "D" and "drought", "C" and "control"

#Do we have the expected number of timepoints? There should be 9.
#Yes, there are 9 timepoints
unique(phen$Timepoint)
length(unique(phen$Timepoint))

#Step 2: adjust data formatting and consistency####
#Make new data frame variable for "clean" data
phen2 <- phen

#Height: change commas to periods so that all decimal points are formatted as periods
phen2$Height
phen2$Height <- str_replace_all(string=phen2$Height, pattern=",", replace=".")
phen2$Height <- as.numeric(phen2$Height)
#Height: convert from character to numeric data type with as.numeric() function
phen2$Height <- as.numeric(phen2$Height)

#Fix genotype names by using all uppercase letters with toupper() function
phen2$Genotype <- toupper(phen2$Genotype)

#Fix treatment names by only using "drought" or "control"
#Replace observations in the Treatment column that are "D" with "drought" using which() function to select rows
phen2[which(phen2$Treatment=="D"),"Treatment"] <- "drought"
#Replace observations in the Treatment column that are "C" with "control" using which() function to select rows
phen2$Treatment[which(phen2$Treatment=="C")] <- "control"

#Save clean data table
write.table(x=phen2, file="upscTech_droughtData_clean_20250408.txt", sep="\t", quote=F, row.names=F)
