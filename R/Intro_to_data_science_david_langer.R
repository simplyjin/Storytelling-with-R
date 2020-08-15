#R Studio API Code
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load raw data
train <- read.csv("../data/train.csv", header = TRUE)
test <- read.csv("../data/test.csv", header = TRUE)

# Add a "Survived" variable to the test set to allow for combining data sets
test.survived <- data.frame(survived = rep("None", nrow(test)), test[,]) #repeat the value of none, by the nrow in test, then combine it with the entire dataframe test

# Combine data sets
data.combined <- rbind(train, test.survived) #take the train df and append to it row by row the test.survived df

# A bit about R data types (e.g., factors)
str(data.combined)

data.combined$survived <- as.factor(data.combined$survived) #we're not doing calculations on these columns, so they should be a factor
data.combined$pclass <- as.factor(data.combined$pclass)


# Take a look at gross survival rates
table(data.combined$survived)

# Distribution across classes
table(data.combined$pclass) #note that it is interesting to see the most in 3rd class and you would expect 2nd class to be 2nd most popular, but it's not. 1st class has more

# Load up ggplot2 package to use for visualizations
library(ggplot2)

# Hypothesis - Rich folks survived at a higher rate - b/c higher classes have rooms higher up in the boat
train$pclass <- as.factor(train$pclass)
ggplot(train, aes(x = pclass, fill = factor(survived))) + #illustrating that we can convert survived into a factor within ggplot
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 
#analysis - it seems like if you were 3rd class, you were more likely to die than to survive, while in 1st class you were more likely to survive

# Examine the first few names in the training data set
head(as.character(train$name))


# How many unique names are there across both train & test?
length(unique(as.character(data.combined$name))) #we got back 1307, but we're expecting 1309 obs


# Two duplicate names, take a closer look
# First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$name))), "name"])


# Next, take a look at the records in the combined data set
data.combined[which(data.combined$name %in% dup.names),]


# What is up with the 'Miss.' and 'Mr.' thing? 
library(stringr)

# Any correlation with other variables (e.g., sibsp)?
misses <- data.combined[which(str_detect(data.combined$name, "Miss.")),]
misses[1:5,] #we find that 4/5 Misses survived and many of them were in 3rd class


# Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$name, "Mrs.")), ]
mrses[1:5,] #first 5 all survived


# Check out males to see if pattern continues
males <- data.combined[which(data.combined$sex == "male"), ]
males[1:5,] #first 5 all died




# Expand upon the realtionship between `Survived` and `Pclass` by adding the new `Title` variable to the
# data set and then explore a potential 3-dimensional relationship.

# Create a utility function to help with title extraction
# NOTE - Using the grep function here, but could have used the str_detect function as well.
extractTitle <- function(name) { #create a function named extractTitle that takes one parameter "name"
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) { #we use grep to return "miss" in "name" and if the length is greater than 0 return "Miss"
    return ("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}


# NOTE - The code below uses a for loop which is not a very R way of
#        doing things
titles <- NULL
for (i in 1:nrow(data.combined)) { #goes through each row in data.combined
  titles <- c(titles, extractTitle(data.combined[i,"name"])) #run the extracTitle on the name column
}
data.combined$title <- as.factor(titles)


# Since we only have survived labels for the train set, only use the
# first 891 rows
ggplot(data.combined[1:891,], aes(x = title, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

#analysis, we see that if you were a Miss or Mrs, you were far more likely to survive no matter what class