#==============================================================================
#
# Video #7 - Submitting, scoring, and some analysis.
#
#==============================================================================

#
# Rpart scores 0.80383
#
# Subset our test records and features
test.submit.df <- data.combined[892:1309, features]

# Make predictions
rpart.3.preds <- predict(rpart.3.cv.1$finalModel, test.submit.df, type = "class")
table(rpart.3.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rpart.3.preds)

write.csv(submit.df, file = "../output/RPART_SUB_20160619_1.csv", row.names = FALSE)


#
# Random forest scores 0.80861
#
features <- c("pclass", "new.title", "ticket.party.size", "avg.fare")
rf.train.temp <- data.combined[1:891, features]

set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp, y = rf.label, ntree = 1000)
rf.temp


test.submit.df <- data.combined[892:1309, features]

# Make predictions
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.preds)

write.csv(submit.df, file = "../output/RF_SUB_20160619_1.csv", row.names = FALSE)



#
# If we want to improve our model, a good place to start is focusing on where it
# gets things wrong!
#


# First, let's explore our collection of features using mutual information to
# gain some additional insight. Our intuition is that the plot of our tree
# should align well to the definition of mutual information.
#install.packages("infotheo")
library(infotheo)

mutinformation(rf.label, data.combined$pclass[1:891])
mutinformation(rf.label, data.combined$sex[1:891])
mutinformation(rf.label, data.combined$sibsp[1:891])
mutinformation(rf.label, data.combined$parch[1:891])
mutinformation(rf.label, discretize(data.combined$fare[1:891]))
mutinformation(rf.label, data.combined$embarked[1:891])
mutinformation(rf.label, data.combined$title[1:891])
mutinformation(rf.label, data.combined$family.size[1:891])
mutinformation(rf.label, data.combined$ticket.first.char[1:891])
mutinformation(rf.label, data.combined$cabin.multiple[1:891])
mutinformation(rf.label, data.combined$new.title[1:891])
mutinformation(rf.label, data.combined$ticket.party.size[1:891])
mutinformation(rf.label, discretize(data.combined$avg.fare[1:891]))


# OK, now let's leverage the tsne algorithm to create a 2-D representation of our data 
# suitable for visualization starting with folks our model gets right very often - folks
# with titles other than 'Mr."
#install.packages("Rtsne")
library(Rtsne)
most.correct <- data.combined[data.combined$new.title != "Mr.",]
indexes <- which(most.correct$survived != "None")


# NOTE - Bug fix for original version. Rtsne needs a seed to ensure consistent
# output between runs.
set.seed(984357)
tsne.1 <- Rtsne(most.correct[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.1$Y[indexes, 1], y = tsne.1$Y[indexes, 2], 
                 color = most.correct$survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.title Other than 'Mr.'")


# To get a baseline, let's use conditional mutual information on the tsne X and
# Y features for females and boys in 1st and 2nd class. The intuition here is that
# the combination of these features should be higher than any individual feature
# we looked at above.
condinformation(most.correct$survived[indexes], discretize(tsne.1$Y[indexes,]))


# As one more comparison, we can leverage conditional mutual information using
# the top two features used in our tree plot - new.title and pclass
condinformation(rf.label, data.combined[1:891, c("new.title", "pclass")])


# OK, now let's take a look at adult males since our model has the biggest 
# potential upside for improving (i.e., the tree predicts incorrectly for 86
# adult males). Let's visualize with tsne.
misters <- data.combined[data.combined$new.title == "Mr.",]
indexes <- which(misters$survived != "None")

tsne.2 <- Rtsne(misters[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.2$Y[indexes, 1], y = tsne.2$Y[indexes, 2], 
                 color = misters$survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.title of 'Mr.'")


# Now conditional mutual information for tsne features for adult males
condinformation(misters$survived[indexes], discretize(tsne.2$Y[indexes,]))


#
# Idea - How about creating tsne featues for all of the training data and
# using them in our model?
#
tsne.3 <- Rtsne(data.combined[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.3$Y[1:891, 1], y = tsne.3$Y[1:891, 2], 
                 color = data.combined$survived[1:891])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for all Training Data")

# Now conditional mutual information for tsne features for all training
condinformation(data.combined$survived[1:891], discretize(tsne.3$Y[1:891,]))

# Add the tsne features to our data frame for use in model building
data.combined$tsne.x <- tsne.3$Y[,1]
data.combined$tsne.y <- tsne.3$Y[,2]