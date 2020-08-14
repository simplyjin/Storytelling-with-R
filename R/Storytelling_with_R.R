#R Studio API Code
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load Libraries
library(ggplot2)

head(economics)
head(economics_long)

#plot it
ggplot(economics_long, aes(x = date, y = value01, color = variable, group = variable)) +
  geom_line()

#area graph
ggplot(economics_long, aes(x = date, y = value01, color = variable, group = variable)) +
  geom_area()

#reshape 
library(reshape2)

#command to change to long form
#new_var <- melt(dataframe, time_var)

#pick a variable you want to plot
head(economics[c(1,3)])

#reshape data assigning to new dataset
#note toself - this is non-tidy. Long is non-tidy
economics_melt <- melt(economics[c(1,3)],'date')

#check new set 
head(economics_melt)


#datatables - builds interactive searchable tables from dataframes
library(DT)

#create interactive table
# datatable(data, options = list(),
#           rownames, colnames, container, caption = NULL,
#           filter = c("none", "bottom", "top"))

datatable(economics)

#customize page lengths
datatable(economics, options = list(pageLength = 50))



#Dygraphs - dygraphs javascript library
library(dygraphs)
library(xts) #dygraphs only accepts xts time series object

#must first make economics dataset into xts form
#convert string date to date format
date_me <- as.Date(economics$date, format = '%Y-%m-%d')
value_me <- data.frame(date_me, economics$pce)
plot_me <- xts(value_me, order.by=value_me[,1])

dygraph(plot_me)

#add some flare
dygraph(plot_me) %>% dyRangeSelector()


#########################################################################################################
# intro to Data analysis - Jeff Chen - Commerce Academy
dim(economics)
colnames(economics)
head(economics)
str(economics)
summary(economics)
